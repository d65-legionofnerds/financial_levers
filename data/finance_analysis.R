library(tidyverse)


# Load data
finance_data_levers <- read_csv(
  "data/finance data - levers.csv",
  col_types = cols(
    FY27 = col_number(),
    FY28 = col_number(),
    FY29 = col_number(),
    FY30 = col_number()
  )
)

## Pivot and add baseline rows (zeroed expenditures)
baseline_rows <- finance_data_levers %>%
  pivot_longer(
    cols = starts_with("FY"),
    names_to = "year",
    values_to = "value"
  ) %>%
  distinct(year) %>%
  mutate(
    Source = "Baseline",
    `Lever type` = "expenditure",
    value = 0
  ) %>%
  select(Source, `Lever type`, year, value)

finance_levers_base <- finance_data_levers %>%
  pivot_longer(
    cols = starts_with("FY"),
    names_to = "year",
    values_to = "value"
  ) %>%
  bind_rows(baseline_rows)

# Combine NERDS + Nov3 into a single "NERDS" source
nerds_combined <- finance_levers_base %>%
  filter(Source %in% c("NERDS", "Nov3")) %>%
  group_by(`Lever type`, year) %>%
  summarize(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(Source = "NERDS")

finance_levers <- finance_levers_base %>%
  filter(Source != "NERDS") %>%
  bind_rows(nerds_combined) %>%
  group_by(Source, `Lever type`, year) %>%
  summarize(total = sum(value, na.rm = TRUE), .groups = "drop")

# Save intermediate wide->long summary (optional)
write_csv(finance_levers, "data/finance_levers_yr.csv")

# FY27 Budget baseline (starting budget values)
fy27_budget <- tibble(
  Source = "Budget",
  `Lever type` = c("revenue", "expenditure"),
  year = "FY27",
  value = c(175485091, 175682880)
)

# -------------------------
# Admin, CPI, Growth inputs
# -------------------------
cpi_seq <- seq(2, 5, by = 0.1)
exp_seq <- seq(2, 5.25, by = 0.25)
admin_seq <- seq(0, 0.50, by = 0.1)

future_years <- paste0("FY", 28:30)

# Source list (exclude budget)
sources <- finance_levers %>%
  filter(Source != "Budget") %>%
  pull(Source) %>%
  unique()

# -----------------------------------------------
# Function to compute admin cut:
#  * 0.1 admin = 540,000 cut to expenditures
#  admin_bloat is decimal (e.g., 0.1, 0.2, ...)
# -----------------------------------------------
admin_cut_amount <- function(admin_bloat) {
  # admin_bloat / 0.1 gives how many 0.1 increments; multiply by 540,000
  (admin_bloat / 0.1) * 540000
}

# -----------------------------------------------
# Construct projection for each source
# -----------------------------------------------
all_source_projections <- map_df(sources, function(src) {
  
  # Get FY27 values for this source from original pivoted table
  fy27_source <- finance_data_levers %>%
    pivot_longer(cols = starts_with("FY"), names_to = "year", values_to = "value") %>%
    filter(Source == src, year == "FY27") %>%
    select(Source, `Lever type`, source_value = value)
  
  # Merge FY27 budget baseline with source-specific FY27 contributions
  starting_values <- fy27_budget %>%
    left_join(fy27_source, by = "Lever type") %>%
    mutate(
      # For revenue: budget + source contribution (if any)
      # For expenditures: budget - source contribution (if any)
      value = ifelse(
        `Lever type` == "revenue",
        value + coalesce(source_value, 0),
        value - coalesce(source_value, 0)
      ),
      Source = src
    ) %>%
    select(Source, `Lever type`, value)
  
  # Expand to all CPI / ExpensesGrowth / Admin combos for FY27
  fy27_expanded <- starting_values %>%
    expand_grid(
      CPI = cpi_seq,
      ExpensesGrowth = exp_seq,
      AdminBloat = admin_seq
    ) %>%
    # Apply admin cut ONLY to FY27 and ONLY for expenditures
    mutate(
      value = ifelse(
        `Lever type` == "expenditure",
        value - admin_cut_amount(AdminBloat),
        value
      )
    )
  
  # Project FY28-30 from the FY27 (already admin-adjusted where applicable)
  fy28_30 <- fy27_expanded %>%
    group_by(Source, `Lever type`, CPI, ExpensesGrowth, AdminBloat) %>%
    mutate(
      projected = list({
        vals <- numeric(length(future_years))
        # FY28 = FY27 * (1 + rate)
        vals[1] <- ifelse(
          `Lever type` == "revenue",
          value * (1 + CPI / 100),
          value * (1 + ExpensesGrowth / 100)
        )
        if (length(future_years) > 1) {
          for (i in 2:length(future_years)) {
            vals[i] <- ifelse(
              `Lever type` == "revenue",
              vals[i - 1] * (1 + CPI / 100),
              vals[i - 1] * (1 + ExpensesGrowth / 100)
            )
          }
        }
        vals
      })
    ) %>%
    select(-value) %>%
    unnest(cols = c(projected)) %>%
    mutate(year = rep(future_years, times = n() / length(future_years))) %>%
    rename(value = projected)
  
  # Combine FY27 rows (with AdminBloat applied for expenditures) and FY28-30 projections
  fy27_with_year <- fy27_expanded %>% mutate(year = "FY27")
  
  bind_rows(fy27_with_year, fy28_30)
})

# -----------------------------------------------
# Baseline projections (Budget baseline, no admin cut applied)
# -----------------------------------------------
baseline_projections <- fy27_budget %>%
  expand_grid(
    CPI = cpi_seq,
    ExpensesGrowth = exp_seq,
    AdminBloat = admin_seq
  ) %>%
  # Note: baseline does NOT apply admin cut (it is baseline)
  group_by(Source, `Lever type`, CPI, ExpensesGrowth, AdminBloat) %>%
  mutate(
    projected = list({
      vals <- numeric(length(future_years))
      vals[1] <- ifelse(`Lever type` == "revenue",
                        value * (1 + CPI / 100),
                        value * (1 + ExpensesGrowth / 100))
      if (length(future_years) > 1) {
        for (i in 2:length(future_years)) {
          vals[i] <- ifelse(`Lever type` == "revenue",
                            vals[i - 1] * (1 + CPI / 100),
                            vals[i - 1] * (1 + ExpensesGrowth / 100))
        }
      }
      vals
    })
  ) %>%
  select(-value) %>%
  unnest(cols = c(projected)) %>%
  mutate(year = rep(future_years, times = n() / length(future_years))) %>%
  rename(value = projected) %>%
  # Also include the FY27 baseline row expanded for all admin/CPI/growth combos (value unchanged)
  bind_rows(
    fy27_budget %>% expand_grid(CPI = cpi_seq, ExpensesGrowth = exp_seq, AdminBloat = admin_seq)
  )

# -----------------------------------------------
# Combine all projections and save
# -----------------------------------------------
all_projections <- bind_rows(
  baseline_projections,
  all_source_projections
) %>%
  arrange(Source, `Lever type`, year, CPI, ExpensesGrowth, AdminBloat)

# Optional: summary check
all_projections %>%
  group_by(Source, `Lever type`, year) %>%
  summarise(
    min_value = min(value, na.rm = TRUE),
    max_value = max(value, na.rm = TRUE),
    .groups = "drop"
  ) %>% print(n = 50)

# Example check (replace "Nov3" with a real source name present in your data)
all_projections %>% filter(Source == "Nov3" & CPI == 2 & ExpensesGrowth == 3 & AdminBloat == 0.1)

# Save final table
write_csv(all_projections, "data/expenses_yr.csv")


## Calc cash on hand: 
# For each unique combo of Admin, CPI, Exp growth, do the following
# take expenses/365
# take revenue-expenses, plus prior year fund values
# take this value above and divide by expenses/365 to get cash on hand
## Calc cash on hand: 
# For each unique combo of Admin, CPI, Exp growth, do the following
# take expenses/365
# take revenue-expenses, plus prior year fund values
# take this value above and divide by expenses/365 to get cash on hand
coh_calc <- expenses_yr %>% 
  pivot_wider(names_from = "Lever type", values_from = value) %>%
  group_by(Source, CPI, ExpensesGrowth, AdminBloat) %>%
  arrange(year) %>%
  mutate(
    rev_less_exp = revenue - expenditure,
    daily_expenses = expenditure / 365
  ) %>%
  # Calculate fund balance cumulatively
  mutate(
    # First create starting balance column
    starting_balance = if_else(year == "FY27", 47949359, 0),
    # Then calculate fund balance as: starting balance + cumulative sum of net revenue
    fund_bal = starting_balance + cumsum(rev_less_exp),
    # Calculate days of cash on hand
    coh = fund_bal / daily_expenses
  ) %>%
  select(-starting_balance) %>%  # Remove helper column
  ungroup()

write_csv(coh_calc, "data/coh_calc.csv")