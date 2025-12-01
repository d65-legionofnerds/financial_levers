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


# Keep Nov3 separate and add combined NERDS
finance_levers <- finance_levers_base %>%
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
  value = c(179466529, 183874315 ) 
)

# Parameter grids
params <- expand_grid(
  CPI = seq(2, 5, by = 0.1),
  ExpensesGrowth = seq(2, 5, by = 0.1),
  add_cuts = seq(0, 13, by = 0.5)
)

future_years <- paste0("FY", 28:30)
sources <- setdiff(unique(finance_levers$Source), "Budget")


# ============================================================================
# 3. PROJECT FUNCTION (applies to both baseline and sources)
# ============================================================================

project_finances <- function(base_values, params_grid) {
  # Start with base values and cross with all parameter combinations
  base_with_params <- base_values %>%
    crossing(params_grid)
  
  # Generate all years (FY27-FY30)
  all_years <- c("FY27", future_years)
  
  # Project across all years with cuts applied
  all_projections <- base_with_params %>%
    group_by(Source, `Lever type`, CPI, ExpensesGrowth, add_cuts) %>%  
    reframe(
      year = all_years,
      value = {
        # Determine growth rate based on lever type
        rate <- if_else(`Lever type`[1] == "revenue", CPI[1], ExpensesGrowth[1]) / 100
        
        # Get base value and apply cuts if expenditure
        base_val <- value[1]
        if (`Lever type`[1] == "expenditure" && add_cuts[1] > 0) {
          base_val <- base_val - (add_cuts[1] * 1e6)
        }
        
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
baseline_projections <- project_finances(fy27_budget, params)

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
  
  project_finances(fy27_values, params)
})

# Combine all
all_projections <- bind_rows(baseline_projections, source_projections) %>%
  arrange(Source, `Lever type`, year, CPI, ExpensesGrowth, add_cuts) %>%
  mutate(ExpensesGrowth = round(ExpensesGrowth, 2))


write_csv(all_projections, "data/expenses_yr.csv")

# ============================================================================
# 5. CALCULATE CASH ON HAND
# ============================================================================

# Starting fund balance for FY26 (end of prior year)
fy26_starting_balance <- 39949359 #subtract the 8 million here

coh_calc <- all_projections %>%
  pivot_wider(names_from = "Lever type", values_from = value) %>%
  group_by(Source, CPI, ExpensesGrowth, add_cuts) %>%
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

cat("\n=== Sample COH Calculation (no cuts) ===\n")
coh_calc %>%
  filter(Source == sources[1], CPI == 2.5, ExpensesGrowth == 2.5, add_cuts == 0) %>%
  select(year, revenue, expenditure, fund_bal, coh, add_cuts) %>%
  print()

cat("\n=== Sample COH Calculation (with 5M cuts) ===\n")
coh_calc %>%
  filter(Source == sources[1], CPI == 2.5, ExpensesGrowth == 2.5, add_cuts == 5) %>%
  select(year, revenue, expenditure, fund_bal, coh, add_cuts) %>%
  print()

# Debug: Check if cuts are being applied
cat("\n=== Verify Cuts Applied ===\n")
all_projections %>%
  filter(Source == "Budget", `Lever type` == "expenditure", year == "FY27", 
         CPI == 2.5, ExpensesGrowth == 2.5, add_cuts %in% c(0, 5, 10)) %>%
  select(Source, year, CPI, ExpensesGrowth, add_cuts, value) %>%
  print()
