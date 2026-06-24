# THIS PULLS HMGP DATA FROM OpenFEMA AND DETERMINES WHICH HMGP DISASTERS WILL BE IMPACTED BY PA DECS
# ENSURE THE FOLLOWING PACAKAGES ARE INSTALLED: tidyverse, jsonlite, readxl
# RUN pa_options_analysis.R PRIOR TO RUNNING THIS SCRIPT SO THAT PA THRESHOLDS ARE AVAILABLE
# ENSURE YOU HAVE PA_threshold_tests.xlsx LOADED IN THE WORKING DIRECTORY

library(jsonlite)
library(tidyverse)
library(readxl)

# Load and Format HMGP Data -----------------------------------------------

# Load database from OpenFEMA
hmgp_data <- fromJSON('https://www.fema.gov/api/open/v2/HazardMitigationGrantProgramDisasterSummaries.json')
hmgp <- hmgp_data$HazardMitigationGrantProgramDisasterSummaries

# Format declarationDate and convert to year
hmgp$year <- format(as.Date(hmgp$declarationDate), "%Y")
hmgp$declarationDate <- ymd_hms(hmgp$declarationDate)

# Select relevant data
hmgp <- hmgp |>
  select(disasterNumber, lockedInCeilingAmount, declarationDate, year, state) |>
  rename(hmgp_amount = lockedInCeilingAmount) |>
  filter(as.numeric(year) >= 2015)

# Load PA Decs data for comparison
baseline <- read_xlsx('PA_threshold_tests.xlsx', sheet = 'Baseline')
option_2 <- read_xlsx('PA_threshold_tests.xlsx', sheet = 'Inflation')
coa <- read_xlsx('PA_threshold_tests.xlsx', sheet = 'COA')
pcpi <- read_xlsx('PA_threshold_tests.xlsx', sheet = 'PCPI')
ttr <- read_xlsx('PA_threshold_tests.xlsx', sheet = 'TTR')

# Initialize matrix to store results
hmgp_results_matrix <- matrix(nrow = 5, ncol = 4)
colnames(hmgp_results_matrix) <- c("recommend", "non_recommend", "fund", "nofund")
rownames(hmgp_results_matrix) <- c("Baseline", "Inflation", "COA", "PCPI", "TTR")

# Inflation Adjustment ----------------------------------------------------

#Input GDP Deflator from BEA
gdp_deflator <- c(97.316, 98.241, 100.000, 102.291, 103.979, 105.377, 110.186, 118.023, 122.390, 125.428)

#Vector of Years
year <- c(2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024)

#Index GDP Deflator
convert <-  function(i) {
  index <- gdp_deflator[10] / i
  return(index)
}

# Create dataframe for GDP deflator
deflator <- convert(gdp_deflator)
inflation <- as.data.frame(cbind(year, deflator))
inflation$year <- as.factor(inflation$year)

# Add GDP deflator to dataframe
hmgp_adjusted <- hmgp |>
  left_join(inflation, by = 'year') |>
  mutate(adj_hmgp_amount = hmgp_amount * deflator)

# Clean HMGP Adjusted database for joining
hmgp_adjusted <- hmgp_adjusted |>
  select(declarationDate, disasterNumber, adj_hmgp_amount, state) |>
  mutate(adj_hmgp_amount = na_if(adj_hmgp_amount, 0))

# Determine HMGP Declarations Based on PA Criteria ------------------------

# Baseline
hmgp_baseline <- hmgp_adjusted |>
  left_join(baseline, by = c('declarationDate', 'state'),
            relationship = 'many-to-many') |>
  select(declarationDate, disasterNumber.x, adj_hmgp_amount, state, year, pass) |>
  filter(!is.na(adj_hmgp_amount), !is.na(year))

# Historic inflation
hmgp_inflation <- hmgp_adjusted |>
  left_join(option_2, by = c('declarationDate', 'state'),
            relationship = 'many-to-many') |>
  select(declarationDate, disasterNumber.x, adj_hmgp_amount, state, year, pass) |>
  filter(!is.na(adj_hmgp_amount), !is.na(year))

# Cost of Assistance
hmgp_coa<- hmgp_adjusted |>
  left_join(coa, by = c('declarationDate', 'state'),
            relationship = 'many-to-many') |>
  select(declarationDate, disasterNumber.x, adj_hmgp_amount, state, year, pass) |>
  filter(!is.na(adj_hmgp_amount), !is.na(year))

# PCPI
hmgp_pcpi <- hmgp_adjusted |>
  left_join(pcpi, by = c('declarationDate', 'state'),
            relationship = 'many-to-many') |>
  select(declarationDate, disasterNumber.x, adj_hmgp_amount, state, year, pass) |>
  filter(!is.na(adj_hmgp_amount), !is.na(year))

# TTR
hmgp_ttr <- hmgp_adjusted |>
  left_join(ttr, by = c('declarationDate', 'state'),
            relationship = 'many-to-many') |>
  select(declarationDate, disasterNumber.x, adj_hmgp_amount, state, year, pass) |>
  filter(!is.na(adj_hmgp_amount), !is.na(year))


# Functions to Summarize Results ------------------------------------------

# Identify disasters that pass the thresholds
identify_disaster_status <- function(test) {
  recommend <- test |> filter(pass == 'yes') |> nrow()
  non_recommend <- test |> filter(pass == 'no') |> nrow()
  data.frame(
    recommend = recommend,
    non_recommend = non_recommend
  )
}

# Display the total funding
total_funding <- function(test) {
  fund <- test |> filter(pass == 'yes') |> summarize(fund = sum(adj_hmgp_amount, na.rm = TRUE))
  nofund <- test |> filter(pass == 'no') |> summarize(nofund = sum(adj_hmgp_amount, na.rm = TRUE))
  data.frame(
    fund = fund$fund,
    nofund = nofund$nofund
  )
}

# Summarize number of declarations for each scenario
baseline_disasters <- identify_disaster_status(hmgp_baseline)
inflation_disasters <- identify_disaster_status(hmgp_inflation)
coa_disasters <- identify_disaster_status(hmgp_coa)
pcpi_disasters <- identify_disaster_status(hmgp_pcpi)
ttr_disasters <- identify_disaster_status(hmgp_ttr)

# Summarize total funding for each scenario
baseline_funding <- total_funding(hmgp_baseline)
inflation_funding <- total_funding(hmgp_inflation)
coa_funding <- total_funding(hmgp_coa)
pcpi_funding <- total_funding(hmgp_pcpi)
ttr_funding <- total_funding(hmgp_ttr)

# Summarize Results and Export to Excel -----------------------------------

# Store results in the results matrix
hmgp_results_matrix["Baseline", ] <- c(baseline_disasters$recommend, baseline_disasters$non_recommend, baseline_funding$fund, baseline_funding$nofund)
hmgp_results_matrix["Inflation", ] <- c(inflation_disasters$recommend, inflation_disasters$non_recommend, inflation_funding$fund, inflation_funding$nofund)
hmgp_results_matrix["COA", ] <- c(coa_disasters$recommend, coa_disasters$non_recommend, coa_funding$fund, coa_funding$nofund)
hmgp_results_matrix["PCPI", ] <- c(pcpi_disasters$recommend, pcpi_disasters$non_recommend, pcpi_funding$fund, pcpi_funding$nofund)
hmgp_results_matrix["TTR", ] <- c(ttr_disasters$recommend, ttr_disasters$non_recommend, ttr_funding$fund, ttr_funding$nofund)

# Convert to a dataframe for export
options_summary <- as.data.frame(hmgp_results_matrix, row.names = c('Baseline', 'Inflation', 'COA', 'PCPI', 'TTR'))
colnames(options_summary) <- c('Recommended', 'Not Recommended', 'Funded', 'Not Funded')

# Export results to Excel
write_xlsx(list('Baseline' = hmgp_baseline, 'Inflation' = hmgp_inflation, 'COA' = hmgp_coa, 'PCPI' = hmgp_pcpi, 'TTR' = hmgp_ttr, 'Summary' = 
                  options_summary), 'HMGP_threshold_tests.xlsx')

# Plot the results --------------------------------------------------------

hmgp_baseline$pass <- factor(hmgp_baseline$pass, levels = c("yes", "no"))
counts <- table(hmgp_baseline$pass)
custom_labels_1 <- c(
  "yes" = paste0("Yes  ( " , counts["yes"], " )"),
  "no"  = paste0("No    ( " , counts["no"], " )")
)

baseline_plot <- ggplot(hmgp_baseline, aes(x = declarationDate, y = adj_hmgp_amount, color = pass, shape = pass)) +
  geom_point(size = 3) +
  scale_color_manual(values = c('#FF0000','#0047AB'),
                     labels = custom_labels_1, name = 'Recommended') +
  scale_shape_manual(values = c('yes' = 16, 'no' = 15),
                     labels = custom_labels_1, name = 'Recommended') +
  scale_y_log10() +
  scale_x_date(date_labels = '%Y') +
  labs(
    title = 'HMGP Declaration Recommendations Based Solely on Cost of Assistance Factors',
    x = 'Year',
    y = 'HMGP Funding 2024$ (Log Scale)',
    color = 'Recommended',
    shape = 'Recommended')

hmgp_inflation$pass <- factor(hmgp_inflation$pass, levels = c("yes", "no"))
counts <- table(hmgp_inflation$pass)
custom_labels_2 <- c(
  "yes" = paste0("Yes  ( " , counts["yes"], " )"),
  "no"  = paste0("No    ( " , counts["no"], " )")
)

inflation_plot <- ggplot(hmgp_inflation, aes(x = declarationDate, y = adj_hmgp_amount, color = pass, shape = pass)) +
  geom_point(size = 3) +
  scale_color_manual(values = c('#FF0000','#0047AB'),
                     labels = custom_labels_2, name = 'Recommended') +
  scale_shape_manual(values = c('yes' = 16, 'no' = 15),
                     labels = custom_labels_2, name = 'Recommended') +
  scale_y_log10() +
  scale_x_date(date_labels = '%Y') +
  labs(
    title = 'HMGP Declaration Recommendations Based Solely on Cost of Assistance Factors',
    x = 'Year',
    y = 'HMGP Funding 2024$ (Log Scale)',
    color = 'Recommended',
    shape = 'Recommended')

hmgp_coa$pass <- factor(hmgp_coa$pass, levels = c("yes", "no"))
counts <- table(hmgp_coa$pass)
custom_labels_3 <- c(
  "yes" = paste0("Yes  ( " , counts["yes"], " )"),
  "no"  = paste0("No    ( " , counts["no"], " )")
)

coa_plot <- ggplot(hmgp_coa, aes(x = declarationDate, y = adj_hmgp_amount, color = pass, shape = pass)) +
  geom_point(size = 3) +
  scale_color_manual(values = c('#FF0000','#0047AB'),
                     labels = custom_labels_3, name = 'Recommended') +
  scale_shape_manual(values = c('yes' = 16, 'no' = 15),
                     labels = custom_labels_3, name = 'Recommended') +
  scale_y_log10() +
  scale_x_date(date_labels = '%Y') +
  labs(
    title = 'HMGP Declaration Recommendations Based Solely on Cost of Assistance Factors',
    x = 'Year',
    y = 'HMGP Funding 2024$ (Log Scale)',
    color = 'Recommended',
    shape = 'Recommended')

hmgp_pcpi$pass <- factor(hmgp_pcpi$pass, levels = c("yes", "no"))
counts <- table(hmgp_pcpi$pass)
custom_labels_4 <- c(
  "yes" = paste0("Yes  ( " , counts["yes"], " )"),
  "no"  = paste0("No    ( " , counts["no"], " )")
)

pcpi_plot <- ggplot(hmgp_pcpi, aes(x = declarationDate, y = adj_hmgp_amount, color = pass, shape = pass)) +
  geom_point(size = 3) +
  scale_color_manual(values = c('#FF0000','#0047AB'),
                     labels = custom_labels_4, name = 'Recommended') +
  scale_shape_manual(values = c('yes' = 16, 'no' = 15),
                     labels = custom_labels_4, name = 'Recommended') +
  scale_y_log10() +
  scale_x_date(date_labels = '%Y') +
  labs(
    title = 'HMGP Declaration Recommendations Based Solely on Cost of Assistance Factors',
    x = 'Year',
    y = 'HMGP Funding 2024$ (Log Scale)',
    color = 'Recommended',
    shape = 'Recommended')

hmgp_ttr$pass <- factor(hmgp_ttr$pass, levels = c("yes", "no"))
counts <- table(hmgp_ttr$pass)
custom_labels_5 <- c(
  "yes" = paste0("Yes  ( " , counts["yes"], " )"),
  "no"  = paste0("No    ( " , counts["no"], " )")
)

ttr_plot <- ggplot(hmgp_ttr, aes(x = declarationDate, y = adj_hmgp_amount, color = pass, shape = pass)) +
  geom_point(size = 3) +
  scale_color_manual(values = c('#FF0000','#0047AB'),
                     labels = custom_labels_5, name = 'Recommended') +
  scale_shape_manual(values = c('yes' = 16, 'no' = 15),
                     labels = custom_labels_5, name = 'Recommended') +
  scale_y_log10() +
  scale_x_date(date_labels = '%Y') +
  labs(
    title = 'HMGP Declaration Recommendations Based Solely on Cost of Assistance Factors',
    x = 'Year',
    y = 'HMGP Funding 2024$ (Log Scale)',
    color = 'Recommended',
    shape = 'Recommended')

# Clean Up ----------------------------------------------------------------
rm(hmgp_data, counts, custom_labels_1, custom_labels_2, custom_labels_3, custom_labels_4,
   custom_labels_5, deflator, gdp_deflator, year)

