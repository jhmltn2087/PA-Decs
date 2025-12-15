# Test PA data against thresholds -----------------------------------------

# Load libraries
library(tidyverse)
library(readxl)
library(writexl)

# Import Disaster data

pa <- read_xlsx("C:/Users/jhamilt9/OneDrive - FEMA/Desktop/pa_decs/scaled_disaster_data.xlsx")
state_population_2024 <- read_xlsx("C:/Users/jhamilt9/OneDrive - FEMA/Desktop/pa_decs/state_population_2024.xlsx")

# Remove COVID 19 disaster declarations

pa <- pa |>
  filter(incident_type != 'Biological')

# Convert year to factor
pa$year <- as.factor(pa$year)

# Initialize matrix to store results
results_matrix <- matrix(nrow = 5, ncol = 4)
colnames(results_matrix) <- c("recommend", "non_recommend", "fund", "nofund")
rownames(results_matrix) <- c("Option 1", "Option 2", "Option 3", "Option 4", "Option 5")

# Create functions for repetitive tasks -----------------------------------

# Merge dataframes
threshold_data <- function(threshold) {
  pa |>
    left_join(threshold, by = c('state', 'year'))
}

threshold_data3 <- function(threshold) {
  pa |>
    left_join(threshold, by = 'state')
}

# Test PA amounts against thresholds
process_threshold_data <- function(data, new_pa_col, threshold_col) {
  data |>
    filter(!is.na({{new_pa_col}})) |>
    filter(!is.na({{threshold_col}})) |>
    mutate(
      pass = ifelse({{new_pa_col}} >= {{threshold_col}}, 'yes', 'no')
    )
}

# Identify disasters that pass the thresholds
identify_disaster_status <- function(test) {
  recommend <- test |> 
    filter(pass == 'yes') |> nrow()
  non_recommend <- test |> 
    filter(pass == 'no') |> nrow()
  list(recommend = recommend, non_recommend = non_recommend)
}

# #Display the total funding
total_funding <- function(test) {
  fund <- test |> 
    filter(pass == 'yes') |> 
    summarize(fund = sum(new_pa, na.rm = TRUE))
  nofund <- test |> 
    filter(pass == 'no') |> 
    summarize(nofund = sum(new_pa, na.rm = TRUE))
  list(fund = fund$fund, nofund = nofund$nofund)
}


# Baseline Approach (Option 1) --------------------------------------------


indicator1 <- c(1.41, 1.41, 1.43, 1.46, 1.50, 1.53, 1.55, 1.63, 1.77, 1.84)

# Apply indicators to columns 2:11
threshold1 <- bind_cols(state_population_2024[1],
                        map2_dfc(state_population_2024[2:11], indicator1, ~ .x * .y)
) 

# Restore column names
colnames(threshold1)[2:11] <- colnames(state_population_2024)[2:11]

# Convert to threshold data to long format
long_thresh1 <- pivot_longer(threshold1, cols = -state, names_to = 'year', values_to = 'thresh')

# Convert year to factor
long_thresh1$year <- as.factor(long_thresh1$year)

#Combine the disaster dataframe with the threshold dataframe
threshold_1 <- threshold_data(threshold = long_thresh1)

#Test the values against the threshold
option_1_test <- process_threshold_data(data = threshold_1, new_pa_col = new_pa, threshold_col = thresh)

# Summarize total recommended disasters
option_1_disasters <- identify_disaster_status(test = option_1_test)

# Summarize total funding
option_1_funding <- total_funding(test = option_1_test)



# Historic Inflation (Option 2) -------------------------------------------

indicator2 <- c(2.19, 2.19, 2.22, 2.26, 2.32, 2.36, 2.39, 2.52, 2.73, 2.83)

#Apply indicators to columns 2:11
threshold2 <- bind_cols(state_population_2024[1],
                        map2_dfc(state_population_2024[2:11], indicator2, ~ .x * .y))

# Restore column names
colnames(threshold2)[2:11] <- colnames(state_population_2024)[2:11]

# Convert to threshold data to long format
long_thresh2 <- pivot_longer(threshold2, cols = -state, names_to = 'year', values_to = 'thresh')

# Convert year to factor
long_thresh2$year <- as.factor(long_thresh2$year)

#Combine the disaster dataframe with the threshold dataframe
threshold_2 <- threshold_data(threshold = long_thresh2)

#Test the values against the threshold
option_2_test <- process_threshold_data(data = threshold_2, new_pa_col = new_pa, threshold_col = thresh)

# Summarize total recommended disasters
option_2_disasters <- identify_disaster_status(test = option_2_test)

# Summarize total funding
option_2_funding <- total_funding(test = option_2_test)



# Cost of Assistance (Option 3) -------------------------------------------

# Import COA data
coa <- read_excel("C:/Users/jhamilt9/OneDrive - FEMA/Desktop/pa_decs/coa_indicator.xlsx")

# Combine the disaster dataframe with the threshold dataframe
threshold_3 <- threshold_data3(threshold = coa)

# Ensure all thresholds are above the minimum threshold
threshold_3 <- threshold_3 |>
  mutate(thresh = ifelse(proposed < 1.509e6, 1.509e6, proposed))

#Test the values against the threshold
option_3_test <- process_threshold_data(data = threshold_3, new_pa_col = new_pa, threshold_col = thresh)

# Summarize total recommended disasters
option_3_disasters <- identify_disaster_status(test = option_3_test)

# Summarize total funding
option_3_funding <- total_funding(test = option_3_test)



# PCPI Threshold (Option 4) -----------------------------------------------

# Import the scaled PCPI data
scaled_pcpi <- read_excel("C:/Users/jhamilt9/OneDrive - FEMA/Desktop/pa_decs/scaled_pcpi.xlsx")

# Pivot PCPI dataframe to long format
pcpi_long <- scaled_pcpi |>
  pivot_longer(
    cols = starts_with('20'),
    names_to = 'year',
    values_to = 'pcpi',
  ) |>
  mutate(year = as.factor(year))

# Convert state population data to long format
long_thresh4 <- pivot_longer(state_population_2024, cols = -state, names_to = 'year', values_to = 'pop' )

# Combine pcpi dataframe and state population dataframe to create threshold
threshold_data4 <- pcpi_long |>
  left_join(long_thresh4, by = c('state', 'year')) |>
mutate(thresh = pop * 8e-5 * pcpi)

# Combine the disaster dataframe with the threshold dataframe
threshold_4 <- threshold_data(threshold = threshold_data4)

#Test the values against the threshold
option_4_test <- process_threshold_data(data = threshold_4, new_pa_col = new_pa, threshold_col = thresh)

# Summarize total recommended disasters
option_4_disasters <- identify_disaster_status(test = option_4_test)

# Summarize total funding
option_4_funding <- total_funding(test = option_4_test)



# TTR Threshold (Option 5) ------------------------------------------------


# Import the scaled TTR data
scaled_ttr <- read_excel("C:/Users/jhamilt9/OneDrive - FEMA/Desktop/pa_decs/scaled_ttr.xlsx")

# Pivot TTR dataframe to long format
ttr_long <- scaled_ttr |>
  pivot_longer(
    cols = starts_with('20'),
    names_to = 'year',
    values_to = 'ttr',
  ) |>
  mutate(year = as.factor(year))

# Multiply state TTR by 1,000,000,000 and 0.01 percent
threshold_data5 <- ttr_long |>
  mutate(thresh = ttr * 1e5)

# Combine the disaster dataframe with the threshold dataframe
threshold_5 <- threshold_data(threshold = threshold_data5)

#Test the values against the threshold
option_5_test <- process_threshold_data(data = threshold_5, new_pa_col = new_pa, threshold_col = thresh)

# Summarize total recommended disasters
option_5_disasters <- identify_disaster_status(test = option_5_test)

# Summarize total funding
option_5_funding <- total_funding(test = option_5_test)


# Summarize Results and Export to Excel -----------------------------------

# Store results in the results matrix
results_matrix["Option 1", ] <- c(option_1_disasters$recommend, option_1_disasters$non_recommend, option_1_funding$fund, option_1_funding$nofund)
results_matrix["Option 2", ] <- c(option_2_disasters$recommend, option_2_disasters$non_recommend, option_2_funding$fund, option_2_funding$nofund)
results_matrix["Option 3", ] <- c(option_3_disasters$recommend, option_3_disasters$non_recommend, option_3_funding$fund, option_3_funding$nofund)
results_matrix["Option 4", ] <- c(option_4_disasters$recommend, option_4_disasters$non_recommend, option_4_funding$fund, option_4_funding$nofund)
results_matrix["Option 5", ] <- c(option_5_disasters$recommend, option_5_disasters$non_recommend, option_5_funding$fund, option_5_funding$nofund)

# Convert to a dataframe for export
options_summary <- as.data.frame(results_matrix, row.names = c('Baseline', 'Inflation', 'COA', 'PCPI', 'TTR'))
colnames(options_summary) <- c('Recommended', 'Not Recommended', 'Funded', 'Not Funded')

# Export results to Excel
write_xlsx(list('Baseline' = option_1_test, 'Inflation' = option_2_test, 'COA' = option_3_test, 'PCPI' = option_4_test, 'TTR' = option_5_test, 'Summary' = 
                  options_summary), 'PA_threshold_tests.xlsx')


# Plot the results --------------------------------------------------------

baseline_plot <- ggplot(option_1_test, aes(x = declaration_date, y = log(new_pa), color = pass, shape = pass)) +
  geom_point(size = 2) +
  scale_color_manual(values = c('#FF0000','#0047AB')) +
  scale_shape_manual(values = c('yes' = 16, 'no' = 15)) +
  scale_y_log10() +
  labs(
    title = 'PA Declaration Recommendations Based Solely on Cost of Assitance Factors',
    x = 'Year',
    y = 'PA Funding 2024$ (Log Scale)',
    color = 'Recommended',
    shape = 'Recommended')

inflation_plot <- ggplot(option_2_test, aes(x = declaration_date, y = log(new_pa), color = pass, shape = pass)) +
  geom_point(size = 2) +
  scale_color_manual(values = c('#FF0000','#0047AB')) +
  scale_shape_manual(values = c('yes' = 16, 'no' = 15)) +
  scale_y_log10() +
  labs(
    title = 'PA Declaration Recommendations Based on Inflation Adjusted Cost of Assistance Factors',
    x = 'Year',
    y = 'PA Funding (Log Scale)',
    color = 'Recommended',
    shape = 'Recommended')

coa_plot <- ggplot(option_3_test, aes(x = declaration_date, y = log(new_pa), color = pass, shape = pass)) +
  geom_point(size = 2) +
  scale_color_manual(values = c('#FF0000','#0047AB')) +
  scale_shape_manual(values = c('yes' = 16, 'no' = 15)) +
  scale_y_log10() +
  labs(
    title = 'PA Declaration Recommendations Based on 2020 Proposed Rule',
    x = 'Year',
    y = 'PA Funding (Log Scale)',
    color = 'Recommended',
    shape = 'Recommended')

pcpi_plot <- ggplot(option_4_test, aes(x = declaration_date, y = log(new_pa), color = pass, shape = pass)) +
  geom_point(size = 2) +
  scale_color_manual(values = c('#FF0000','#0047AB')) +
  scale_shape_manual(values = c('yes' = 16, 'no' = 15)) +
  scale_y_log10() +
  labs(
    title = 'PA Declaration Recommendations Using State PCPI',
    x = 'Year',
    y = 'PA Funding (Log Scale)',
    color = 'Recommended',
    shape = 'Recommended')

ttr_plot <- ggplot(option_5_test, aes(x = declaration_date, y = log(new_pa), color = pass, shape = pass)) +
  geom_point(size = 2) +
  scale_color_manual(values = c('#FF0000','#0047AB')) +
  scale_shape_manual(values = c('yes' = 16, 'no' = 15)) +
  scale_y_log10() +
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

