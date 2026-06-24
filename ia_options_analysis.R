# THIS SCRIPT PULLS IA DISASTER DATA AND COMPARES IT AGAINST VARIOUS THRESHOLD SECNARIOS
# ENSURE THE FOLLOWING PACKAGES ARE INSTALLED: tidyverse, readxl
# COMMENT OUT source('ia.data.R') UNLESS YOU NEED TO PULL NEW IA DATA. IT WILL SIGNIFICANTLY SLOW THIS DOWN
# ENSURE THE FOLLOWING FILES ARE AVAILABLE IN THE WORKING DIRECTORY: IA_Disaster_data.xlsx, data_queries.R, ttr_pc.csv
# ALL MONETARY VALUES ARE AUTOMATICALLY ADJUSTED FOR INFLATION

library(tidyverse)
library(readxl)

#source('ia_data.R')
#source('data_queries.R')
ia_adjusted <- read_xlsx('IA_Disaster_data_vEDW.xlsx')
ia_adjusted$year <- as.factor(ia_adjusted$year)

# Baseline ----------------------------------------------------------------

# ICC Ratio
baseline <- ia_adjusted |>
  left_join(ttr_pc_long, by = c('state', 'year')) |>
  mutate(icc_ratio_b = adj_amount / (ttr_adj * 1000))

# Set threshold based on ICC ratio
baseline <- baseline |>
  mutate(pass_icc = case_when(
    icc_ratio_b > 25 ~ 'yes',
    icc_ratio_b <= 25 & icc_ratio_b >= 10 ~ 'maybe',
    icc_ratio_b < 10 ~ 'no'
  ))
  
# Estimated cost of IHP
baseline <- baseline |>
  mutate(pass_cost = case_when(
    adj_amount > 7.5e7 ~ 'yes',
    adj_amount <= 7.5e7 & adj_amount >= 1.5e7 ~ 'maybe',
    adj_amount < 1.5e7 ~ 'no'
  ))

# Disasters that pass both thresholds
baseline <- baseline |>
  mutate(status = case_when(
    pass_icc == 'yes' & pass_cost =='yes' ~ 'likely approval',
    pass_icc == 'maybe' & pass_cost == 'yes' ~ 'lean approval',
    pass_icc == 'yes' & pass_cost == 'maybe' ~ 'lean approval',
    pass_icc == 'maybe' & pass_cost == 'maybe' ~ 'indeterminate',
    pass_icc == 'yes' & pass_cost == 'no' ~ 'indeterminate',
    pass_icc == 'no' & pass_cost == 'yes' ~ 'indeterminate',
    pass_icc == 'no' & pass_cost == 'maybe' ~ 'lean denial',
    pass_icc == 'maybe' & pass_cost == 'no' ~ 'lean denial',
    pass_icc == 'no' & pass_cost == 'no' ~ 'likely denial'
  ))

# Summarize baseline results
baseline_results <- baseline |>
  group_by(status) |>
  summarize(count = n())


# ICC using PCPI at 0.008% ------------------------------------------------

# Multiply PCPI by State Population
pcpi <- pcpi |>
  left_join(state_population, by = c('state', 'year'))

# ICC Ratio
pcpi_1 <- ia_adjusted |>
  left_join(pcpi, by = c('state', 'year')) |>
  select(-income, -deflator.x, -deflator.y) |>
  mutate(icc_ratio_1 = adj_amount / (adj_income * estimate * 8e-5 * .34))

# Set threshold based on ICC ratio
pcpi_1 <- pcpi_1 |>
  mutate(pass_icc = case_when(
    icc_ratio_1 > 25 ~ 'yes',
    icc_ratio_1 <= 25 & icc_ratio_1 >= 10 ~ 'maybe',
    icc_ratio_1 < 10 ~ 'no'
  ))

# Summarize PCPI 0.008 results
pcpi_1_results <- pcpi_1 |>
  group_by(pass_icc) |>
  summarize(count = n())

# ICC using PCPI at 0.002% ------------------------------------------------

# ICC Ratio
pcpi_2 <- ia_adjusted |>
  left_join(pcpi, by = c('state', 'year')) |>
  select(-income, -deflator.x, -deflator.y) |>
  mutate(icc_ratio_2 = adj_amount / (adj_income * estimate * 1e-5 * .34))

# Set threshold based on ICC ratio
pcpi_2 <- pcpi_2 |>
  mutate(pass_icc = case_when(
    icc_ratio_2 > 25 ~ 'yes',
    icc_ratio_2 <= 25 & icc_ratio_2 >= 10 ~ 'maybe',
    icc_ratio_2 < 10 ~ 'no'
  ))

# Summarize PCPI 0.002 results
pcpi_2_results <- pcpi_2 |>
  group_by(pass_icc) |>
  summarize(count = n())


# Clean Up ----------------------------------------------------------------
