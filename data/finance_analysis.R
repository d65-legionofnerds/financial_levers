library(billboarder)
library(tidyverse)
library(shiny)

#load data:
finance_data_levers <- read_csv("data/finance data - levers.csv", 
                                col_types = cols(FY27 = col_number(), 
                                                 FY28 = col_number(), 
                                                 FY29 = col_number(),
                                                 FY30 = col_number()))

## pivot longer and add baseline data 
baseline_rows <- finance_data_levers %>%
  pivot_longer(
    cols = starts_with("FY"),
    names_to = "year",
    values_to = "value"
  ) %>%
  distinct(year) %>%              # get FY27–FY30
  mutate(
    Source = "Baseline",
    `Lever type` = "expenditure", # or "revenue", or both
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

# Create combined NERDS (NERDS + Nov3)
nerds_combined <- finance_levers_base %>%
  filter(Source %in% c("NERDS", "Nov3")) %>%
  group_by(`Lever type`, year) %>%
  summarize(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(Source = "NERDS")

# Keep all sources EXCEPT original NERDS, then add combined NERDS
finance_levers <- finance_levers_base %>%
  filter(Source != "NERDS") %>%
  bind_rows(nerds_combined) %>%
  group_by(Source, `Lever type`, year) %>%
  summarize(total = sum(value, na.rm = TRUE), .groups = "drop")

write_csv(finance_levers, "data/finance_levers_yr.csv")

## make budget data:

## Bring in FY27 budget (updated from FY26)
# Data from 11/3 presentation, slide
# Revenue: 175,485,091
# Expenditures: 175,682,880
# Note: You'll need to update these values for FY27

# FY27 Budget baseline
fy27_budget <- tibble(
  Source = "Budget",
  `Lever type` = c("revenue", "expenditure"),
  year = "FY27",
  value = c(175485091, 175682880),  # Update these to actual FY27 values
)

# CPI and Expenses Growth sequences
cpi_seq <- seq(2, 3, by = 0.1)
exp_seq <- seq(2, 5.25, by = 0.25)

# Define future years (FY28-30 now that we start from FY27)
future_years <- paste0("FY", 28:30)

# Get unique sources from finance_data_levers
sources <- finance_levers %>%
  filter(Source != "Budget") %>%
  pull(Source) %>%
  unique()

# Process each source
all_source_projections <- map_df(sources, function(src) {
  
  # Get FY27 values for this source
  fy27_source <- finance_data_levers %>%
    filter(Source == src) %>%
    pivot_longer(
      cols = starts_with("FY"),
      names_to = "year",
      values_to = "value"
    ) %>%
    filter(year == "FY27") %>%
    select(Source, `Lever type`, value)
  
  # Merge with budget and calculate starting FY27 values
  starting_values <- fy27_budget %>%
    left_join(
      fy27_source %>% select(`Lever type`, source_value = value),
      by = "Lever type"
    ) %>%
    mutate(
      # For revenue: add to budget
      # For expenditures: subtract from budget
      value = ifelse(`Lever type` == "revenue",
                     value + coalesce(source_value, 0),
                     value - coalesce(source_value, 0)),
      Source = src
    ) %>%
    select(-source_value)
  
  # Expand to all CPI/ExpensesGrowth combinations
  fy27_expanded <- starting_values %>%
    expand_grid(
      CPI = cpi_seq,
      ExpensesGrowth = exp_seq
    )
  
  # Project FY28-30
  fy28_30 <- fy27_expanded %>%
    group_by(Source, `Lever type`, CPI, ExpensesGrowth) %>%
    arrange(`Lever type`) %>%
    mutate(
      projected = list({
        vals <- numeric(length(future_years))
        vals[1] <- ifelse(`Lever type` == "revenue",
                          value * (1 + CPI / 100),
                          value * (1 + ExpensesGrowth / 100))
        if(length(future_years) > 1){
          for(i in 2:length(future_years)){
            vals[i] <- ifelse(`Lever type` == "revenue",
                              vals[i-1] * (1 + CPI / 100),
                              vals[i-1] * (1 + ExpensesGrowth / 100))
          }
        }
        vals
      })
    ) %>%
    select(-value) %>%
    unnest(cols = c(projected)) %>%
    mutate(year = rep(future_years, times = n()/length(future_years))) %>%
    rename(value = projected)
  
  # Combine FY27 and projections
  bind_rows(fy27_expanded, fy28_30)
})

# Also create baseline projections (original budget)
baseline_projections <- fy27_budget %>%
  expand_grid(
    CPI = cpi_seq,
    ExpensesGrowth = exp_seq
  ) %>%
  group_by(Source, `Lever type`, CPI, ExpensesGrowth) %>%
  arrange(`Lever type`) %>%
  mutate(
    projected = list({
      vals <- numeric(length(future_years))
      vals[1] <- ifelse(`Lever type` == "revenue",
                        value * (1 + CPI / 100),
                        value * (1 + ExpensesGrowth / 100))
      if(length(future_years) > 1){
        for(i in 2:length(future_years)){
          vals[i] <- ifelse(`Lever type` == "revenue",
                            vals[i-1] * (1 + CPI / 100),
                            vals[i-1] * (1 + ExpensesGrowth / 100))
        }
      }
      vals
    })
  ) %>%
  select(-value) %>%
  unnest(cols = c(projected)) %>%
  mutate(year = rep(future_years, times = n()/length(future_years))) %>%
  rename(value = projected) %>%
  bind_rows(
    fy27_budget %>%
      expand_grid(CPI = cpi_seq, ExpensesGrowth = exp_seq),
    .
  )

# Combine all projections
all_projections <- bind_rows(
  baseline_projections,
  all_source_projections
) %>%
  arrange(Source, `Lever type`, year, CPI, ExpensesGrowth)

# View summary
all_projections %>%
  group_by(Source, `Lever type`, year) %>%
  summarise(
    min_value = min(value),
    max_value = max(value),
    .groups = "drop"
  )

# check
all_projections %>% filter(Source == "Nov3" & CPI == 2 & ExpensesGrowth == 3)

write_csv(all_projections, "data/expenses_yr.csv")