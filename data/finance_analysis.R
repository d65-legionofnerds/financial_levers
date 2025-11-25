library(tidyverse)

# ============================================================================
# 1. LOAD AND PREPARE BASE DATA
# ============================================================================

finance_data_levers <- read_csv(
  "data/finance data - levers.csv",
  col_types = cols(FY27 = col_number(), FY28 = col_number(), 
                   FY29 = col_number(), FY30 = col_number())
)

# Pivot to long format
finance_levers_base <- finance_data_levers %>%
  pivot_longer(cols = starts_with("FY"), names_to = "year", values_to = "value")

# Create combined NERDS source (original NERDS + Nov3)
nerds_combined <- finance_levers_base %>%
  filter(Source %in% c("NERDS", "Nov3")) %>%
  group_by(`Lever type`, year) %>%
  summarize(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(Source = "NERDS")

# Keep Nov3 separate and add combined NERDS
finance_levers <- finance_levers_base %>%
  filter(Source != "NERDS") %>%  # Remove original NERDS
  bind_rows(nerds_combined) %>%  # Add combined NERDS
  group_by(Source, `Lever type`, year) %>%
  summarize(total = sum(value, na.rm = TRUE), .groups = "drop")

write_csv(finance_levers, "data/finance_levers_yr.csv")

# ============================================================================
# 2. SETUP PARAMETERS
# ============================================================================

# FY27 Budget baseline
fy27_budget <- tibble(
  Source = "Budget",
  `Lever type` = c("revenue", "expenditure"),
  value = c(175485091, 175682880)
)

# Parameter grids
params <- expand_grid(
  CPI = seq(2, 5, by = 0.1),
  ExpensesGrowth = seq(2, 5.25, by = 0.25),
  AdminBloat = seq(0, 0.50, by = 0.1)
)

future_years <- paste0("FY", 28:30)
sources <- setdiff(unique(finance_levers$Source), "Budget")

# Admin cut: $540k per 0.1 (10%)
admin_cut <- function(admin_bloat) (admin_bloat / 0.1) * 540000

# ============================================================================
# 3. PROJECT FUNCTION (applies to both baseline and sources)
# ============================================================================

project_finances <- function(base_values, params_grid, apply_admin = FALSE) {
  # Start with base values and apply admin cut to FY27 FIRST
  base_with_params <- base_values %>%
    crossing(params_grid) %>%
    mutate(
      # Apply admin cut to base value BEFORE growth calculations
      adjusted_base = if_else(
        apply_admin & `Lever type` == "expenditure",
        value - admin_cut(AdminBloat),
        value
      )
    )
  
  # Generate all years (FY27-FY30)
  all_years <- c("FY27", future_years)
  
  # Project across all years FROM THE ADJUSTED BASE
  all_projections <- base_with_params %>%
    group_by(Source, `Lever type`, CPI, ExpensesGrowth, AdminBloat) %>%
    reframe(
      year = all_years,
      value = {
        rate <- if_else(`Lever type`[1] == "revenue", CPI[1], ExpensesGrowth[1]) / 100
        base_val <- adjusted_base[1]  # Use admin-adjusted base
        
        # FY27 = adjusted base (no growth yet)
        # FY28 = adjusted base * (1 + rate)^1
        # FY29 = adjusted base * (1 + rate)^2, etc.
        c(base_val, base_val * (1 + rate) ^ (1:length(future_years)))
      }
    )
  
  all_projections
}

# ============================================================================
# 4. GENERATE ALL PROJECTIONS
# ============================================================================

# Baseline (no admin cuts)
baseline_projections <- project_finances(fy27_budget, params, apply_admin = FALSE)

# Source-specific projections (with admin cuts)
source_projections <- map_df(sources, function(src) {
  # Get FY27 source contributions
  fy27_source <- finance_data_levers %>%
    pivot_longer(cols = starts_with("FY"), names_to = "year", values_to = "source_value") %>%
    filter(Source == src | Source == "Nov3" & src == "NERDS", year == "FY27") %>%
    group_by(`Lever type`) %>%
    summarize(source_value = sum(source_value, na.rm = TRUE), .groups = "drop")
  
  # Merge with budget baseline
  fy27_values <- fy27_budget %>%
    left_join(fy27_source, by = "Lever type") %>%
    mutate(
      value = if_else(
        `Lever type` == "revenue",
        value + coalesce(source_value, 0),
        value - coalesce(source_value, 0)
      ),
      Source = src
    ) %>%
    select(Source, `Lever type`, value)
  
  project_finances(fy27_values, params, apply_admin = TRUE)
})

# Combine all
all_projections <- bind_rows(baseline_projections, source_projections) %>%
  arrange(Source, `Lever type`, year, CPI, ExpensesGrowth, AdminBloat)

write_csv(all_projections, "data/expenses_yr.csv")

# ============================================================================
# 5. CALCULATE CASH ON HAND
# ============================================================================

# Starting fund balance for FY26 (end of prior year)
fy26_starting_balance <- 47949359

coh_calc <- all_projections %>%
  pivot_wider(names_from = "Lever type", values_from = value) %>%
  group_by(Source, CPI, ExpensesGrowth, AdminBloat) %>%
  arrange(year) %>%
  mutate(
    rev_less_exp = revenue - expenditure,
    daily_expenses = expenditure / 365,
    # Calculate fund balance: prior year balance + current year net revenue
    # For FY27: start with FY26 ending balance (47,949,359)
    # For FY28+: use prior year's ending balance
    fund_bal = fy26_starting_balance + cumsum(rev_less_exp),
    coh = fund_bal / daily_expenses
  ) %>%
  ungroup()

write_csv(coh_calc, "data/coh_calc.csv")

# ============================================================================
# 6. VALIDATION CHECKS
# ============================================================================

cat("\n=== Projection Summary ===\n")
all_projections %>%
  group_by(Source, `Lever type`, year) %>%
  summarise(
    scenarios = n(),
    min_value = min(value, na.rm = TRUE),
    max_value = max(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  print(n = 24)

cat("\n=== Sample COH Calculation ===\n")
coh_calc %>%
  filter(Source == sources[1], CPI == 2.5, ExpensesGrowth == 2.5, AdminBloat == 0.3) %>%
  select(year, revenue, expenditure, fund_bal, coh) %>%
  print()