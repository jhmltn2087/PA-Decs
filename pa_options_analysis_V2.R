# READ ME FIRST
# THIS SCRPIT TAKES DATA FROM VARIOUS SOURCES AND CREATES NEW DISASTER DECLARATION THRESHOLDS FOR PA, USING OPTIONS IN OMB SLIDES.
# MAKE SURE WORKING DIRECTORY INCLUDES data_queries.r, pa_data.R, coa_indicator.csv and ttr.csv
# INSTALL PACKAGES: tidyverse, readxl, and writexl
# TTR DATA HAS BEEN SHIFTED TO ACCOUNT FOR RELEASE SCHEDULE LAG, FOR EXAMPLE THE 2024 COLUMN IS 2022 DATA FROM TREASURY
# AFTER RUNNING SCRIPT ONCE, COMMENT OUT source LINES TO AVOID RELOADING LARGE DATASETS

# Load Libraries and Data Sources -----------------------------------------

# Load libraries
library(tidyverse)
library(readxl)
library(writexl)

# Import Data
#source('data_queries.R')
#source('pa_data.R')

# Initialize matrix to store results
pa_results_matrix <- matrix(nrow = 5, ncol = 4)
colnames(pa_results_matrix) <- c("recommend", "non_recommend", "fund", "nofund")
rownames(pa_results_matrix) <- c("Option 1", "Option 2", "Option 3", "Option 4", "Option 5")

# Create functions for repetitive tasks -----------------------------------

# Merge dataframes
threshold_data <- function(threshold) {
  pa_adjusted |>
    left_join(threshold, by = c('state', 'years')) |>
    filter(!state %in% c('District of Columbia', 
                         'American Samoa', 'Guam', 'Northern Mariana Islands', 'Puerto Rico', 
                         'U.S. Virgin Islands'))
}

# Test PA amounts against thresholds
process_threshold_data <- function(data, pa_col, threshold_col) {
  data |>
    mutate(
      pass = if_else(.data[[pa_col]] >= .data[[threshold_col]], 'yes', 'no')
    )
}

# Identify disasters that pass the thresholds
identify_disaster_status <- function(test) {
  recommend <- test |> filter(pass == 'yes') |> nrow()
  non_recommend <- test |> filter(pass == 'no') |> nrow()
  data.frame(
    recommend = recommend,
    non_recommend = non_recommend
  )
}

# #Display the total funding
total_funding <- function(test) {
  fund <- test |> filter(pass == 'yes') |> summarize(fund = sum(adj_amount, na.rm = TRUE))
  nofund <- test |> filter(pass == 'no') |> summarize(nofund = sum(adj_amount, na.rm = TRUE))
  data.frame(
    fund = fund$fund,
    nofund = nofund$nofund
  )
}


# Baseline Approach (Option 1) --------------------------------------------

indicator1 <- c(1.41, 1.41, 1.43, 1.46, 1.50, 1.53, 1.55, 1.63, 1.77, 1.84)
years <- c(2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024)
baseline_indicator <- as.data.frame(cbind(indicator1, years))
baseline_indicator$years <- as.integer(years)

# Apply indicators to population to create threshold
thresh_1 <- state_population |>
  left_join(baseline_indicator, by = 'years') |>
  mutate(thresh = indicator1 * estimate)

# Merge disaster data with threshold data
threshold_1 <- threshold_data(threshold = thresh_1)

#Test the values against the threshold
option_1_test <- process_threshold_data(data = threshold_1, pa_col = 'adj_amount', threshold_col = 'thresh')

# Summarize total recommended disasters
option_1_disasters <- identify_disaster_status(test = option_1_test)

# Summarize total funding
option_1_funding <- total_funding(test = option_1_test)


# Historic Inflation (Option 2) -------------------------------------------

indicator2 <- c(2.23, 2.23, 2.26, 2.31, 2.37, 2.42, 2.45, 2.58, 2.80, 2.91)
hist_inf_indicator <- as.data.frame(cbind(indicator1, years))
hist_inf_indicator$years <- as.integer(years)

# Apply indicators to population to create threshold
thresh_2 <- state_population |>
  left_join(hist_inf_indicator, by = 'years') |>
  mutate(thresh = indicator2 * estimate)

#Combine the disaster data with the threshold data
threshold_2 <- threshold_data(threshold = thresh_2)

#Test the values against the threshold
option_2_test <- process_threshold_data(data = threshold_2, pa_col = 'adj_amount', threshold_col = 'thresh')

# Summarize total recommended disasters
option_2_disasters <- identify_disaster_status(test = option_2_test)

# Summarize total funding
option_2_funding <- total_funding(test = option_2_test)



# Cost of Assistance (Option 3) -------------------------------------------

# Estimate State thresholds using 2020 rule methodology

coa <- state_population |>
  left_join(ttr_pc_long, by = c('state', 'years')) |>
  left_join(hist_inf_indicator, by = 'years') |>
  mutate(indicator3 = (ttr_adj * indicator2) / 100) |>
  mutate(thresh3 = indicator3 * estimate)

# Combine the disaster dataframe with the threshold dataframe

threshold_3 <- threshold_data(threshold = coa)

# Ensure all thresholds are above the minimum threshold

threshold_3 <- threshold_3 |>
  mutate(thresh = ifelse(thresh3 < 1.92e6, 1.92e6, thresh3))

#Test the values against the threshold

option_3_test <- process_threshold_data(data = threshold_3, pa_col = 'adj_amount', threshold_col = 'thresh')

# Summarize total recommended disasters

option_3_disasters <- identify_disaster_status(test = option_3_test)

# Summarize total funding

option_3_funding <- total_funding(test = option_3_test)


# PCPI Threshold (Option 4) -----------------------------------------------


# Apply indicators to population to create threshold
thresh_4 <- pcpi |>
  left_join(state_population, by = c('state', 'years')) |>
  mutate(thresh = estimate * 8e-5 * adj_income)

# Combine the disaster dataframe with the threshold dataframe
threshold_4 <- threshold_data(threshold = thresh_4)

#Test the values against the threshold
option_4_test <- process_threshold_data(data = threshold_4, pa_col = 'adj_amount', threshold_col = 'thresh')

# Summarize total recommended disasters
option_4_disasters <- identify_disaster_status(test = option_4_test)

# Summarize total funding
option_4_funding <- total_funding(test = option_4_test)


# TTR Threshold (Option 5) ------------------------------------------------

# Multiply state TTR by 1,000,000,000 and 0.01 percent
threshold_data5 <- ttr_long |>
  mutate(thresh = ttr_adj * 1e5)

# Combine the disaster dataframe with the threshold dataframe
threshold_5 <- threshold_data(threshold = threshold_data5)

#Test the values against the threshold
option_5_test <- process_threshold_data(data = threshold_5, pa_col = 'adj_amount', threshold_col = 'thresh')

# Summarize total recommended disasters
option_5_disasters <- identify_disaster_status(test = option_5_test)

# Summarize total funding
option_5_funding <- total_funding(test = option_5_test)


# Summarize Results and Export to Excel -----------------------------------

# Store results in the results matrix
pa_results_matrix["Option 1", ] <- c(option_1_disasters$recommend, option_1_disasters$non_recommend, option_1_funding$fund, option_1_funding$nofund)
pa_results_matrix["Option 2", ] <- c(option_2_disasters$recommend, option_2_disasters$non_recommend, option_2_funding$fund, option_2_funding$nofund)
pa_results_matrix["Option 3", ] <- c(option_3_disasters$recommend, option_3_disasters$non_recommend, option_3_funding$fund, option_3_funding$nofund)
pa_results_matrix["Option 4", ] <- c(option_4_disasters$recommend, option_4_disasters$non_recommend, option_4_funding$fund, option_4_funding$nofund)
pa_results_matrix["Option 5", ] <- c(option_5_disasters$recommend, option_5_disasters$non_recommend, option_5_funding$fund, option_5_funding$nofund)

# Convert to a dataframe for export
options_summary <- as.data.frame(pa_results_matrix, row.names = c('Baseline', 'Inflation', 'COA', 'PCPI', 'TTR'))
colnames(options_summary) <- c('Recommended', 'Not Recommended', 'Funded', 'Not Funded')

# Export results to Excel
write_xlsx(list('Baseline' = option_1_test, 'Inflation' = option_2_test, 'COA' = option_3_test, 'PCPI' = option_4_test, 'TTR' = option_5_test, 'Summary' = 
                  options_summary), 'PA_threshold_tests.xlsx')


# Plot the results --------------------------------------------------------
option_1_test$pass <- factor(option_1_test$pass, levels = c("yes", "no"))
counts <- table(option_1_test$pass)
custom_labels_1 <- c(
  "yes" = paste0("Yes  ( " , counts["yes"], " )"),
  "no"  = paste0("No    ( " , counts["no"], " )")
)
baseline_plot <- ggplot(option_1_test, aes(x = as.Date(declarationDate), y = adj_amount, color = pass, shape = pass)) +
  geom_point(size = 3) +
  scale_color_manual(values = c('yes' = '#0047AB', 'no' = '#FF0000')
                     , labels = custom_labels_1, name = 'Recommended'
  ) +
  scale_shape_manual(values = c('yes' = 16, 'no' = 15)
                     , labels = custom_labels_1, name = 'Recommended'
  ) +
  scale_y_log10() +
  scale_x_date(date_labels = '%Y') +
  labs(
    title = 'PA Declaration Recommendations Based Solely on Cost of Assistance Factors',
    x = 'Year',
    y = 'PA Funding 2024$ (Log Scale)',
    color = 'Recommended',
    shape = 'Recommended')

option_2_test$pass <- factor(option_2_test$pass, levels = c("yes", "no"))
counts <- table(option_2_test$pass)
custom_labels_2 <- c(
  "yes" = paste0("Yes  ( " , counts["yes"], " )"),
  "no"  = paste0("No    ( " , counts["no"], " )")
)
inflation_plot <- ggplot(option_2_test, aes(x = as.Date(declarationDate), y = adj_amount, color = pass, shape = pass)) +
  geom_point(size = 3) +
  scale_color_manual(values = c('yes' = '#0047AB', 'no' = '#FF0000')
                     , labels = custom_labels_2, name = 'Recommended'
  ) +
  scale_shape_manual(values = c('yes' = 16, 'no' = 15)
                     , labels = custom_labels_2, name = 'Recommended'
  ) +
  scale_y_log10() +
  scale_x_date(date_labels = '%Y') +
  labs(
    title = 'PA Declaration Recommendations Based on Inflation Adjusted Cost of Assistance Factors',
    x = 'Year',
    y = 'PA Funding (Log Scale)',
    color = 'Recommended',
    shape = 'Recommended')

option_3_test$pass <- factor(option_3_test$pass, levels = c("yes", "no"))
counts <- table(option_3_test$pass)
custom_labels_3 <- c(
  "yes" = paste0("Yes  ( " , counts["yes"], " )"),
  "no"  = paste0("No    ( " , counts["no"], " )")
)
coa_plot <- ggplot(option_3_test, aes(x = as.Date(declarationDate), y = adj_amount, color = pass, shape = pass)) +
  geom_point(size = 3) +
  scale_color_manual(values = c('yes' = '#0047AB', 'no' = '#FF0000')
                     , labels = custom_labels_3, name = 'Recommended'
  ) +
  scale_shape_manual(values = c('yes' = 16, 'no' = 15)
                     , labels = custom_labels_3, name = 'Recommended'
  ) +
  scale_y_log10() +
  scale_x_date(date_labels = '%Y') +
  labs(
    title = 'PA Declaration Recommendations Based on 2020 Proposed Rule',
    x = 'Year',
    y = 'PA Funding (Log Scale)',
    color = 'Recommended',
    shape = 'Recommended')

option_4_test$pass <- factor(option_4_test$pass, levels = c("yes", "no"))
counts <- table(option_4_test$pass)
custom_labels_4 <- c(
  "yes" = paste0("Yes  ( " , counts["yes"], " )"),
  "no"  = paste0("No    ( " , counts["no"], " )")
)
pcpi_plot <- ggplot(option_4_test, aes(x = as.Date(declarationDate), y = adj_amount, color = pass, shape = pass)) +
  geom_point(size = 3) +
  scale_color_manual(values = c('yes' = '#0047AB', 'no' = '#FF0000')
                     , labels = custom_labels_4, name = 'Recommended'
  ) +
  scale_shape_manual(values = c('yes' = 16, 'no' = 15)
                     , labels = custom_labels_4, name = 'Recommended' 
  ) +
  scale_y_log10() +
  scale_x_date(date_labels = '%Y') +
  labs(
    title = 'PA Declaration Recommendations Using State PCPI',
    x = 'Year',
    y = 'PA Funding (Log Scale)',
    color = 'Recommended',
    shape = 'Recommended')

option_5_test$pass <- factor(option_5_test$pass, levels = c("yes", "no"))
counts <- table(option_5_test$pass)
custom_labels_5 <- c(
  "yes" = paste0("Yes  ( " , counts["yes"], " )"),
  "no"  = paste0("No    ( " , counts["no"], " )")
)
ttr_plot <- ggplot(option_5_test, aes(x = as.Date(declarationDate), y = adj_amount, color = pass, shape = pass)) +
  geom_point(size = 3) +
  scale_color_manual(values = c('yes' = '#0047AB', 'no' = '#FF0000')
                     , labels = custom_labels_5, name = 'Recommended'
  ) +
  scale_shape_manual(values = c('yes' = 16, 'no' = 15)
                     , labels = custom_labels_5, name = 'Recommended'
  ) +
  scale_y_log10() +
  scale_x_date(date_labels = '%Y') +
  labs(
    title = 'PA Declaration Recommendations Using State TTR',
    x = 'Year',
    y = 'PA Funding (Log Scale)',
    color = 'Recommended',
    shape = 'Recommended')

baseline_plot
inflation_plot
coa_plot
pcpi_plot
ttr_plot