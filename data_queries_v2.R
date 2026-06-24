# READ ME FIRST
# THIS SCRIPT AUTOMATICALLY PULLS PCPI DATA FROM BEA AND 5-YEAR ACS STATE POPULATION DATA FROM CENSUS
# MAKE SURE THE FOLLOWING PACKAGES ARE INSTALLED: tidyverse, httr, jsonlite, bea.R, tidycensus AND writexl
# TTR DATA MUST BE MANUALLY DOWNLOADED FROM TREASURY, CHANGE YEARS TO ALING MOST RECENT DATA AND SAVE AS ttr.csv
# INFLATION ADJUSTMENTS ARE ADDED AUTOMATICALLY USING GDP DEFLATOR FOR PCPI AND TTR

library(tidyverse)
library(httr)
library(jsonlite)


# Pull PCPI data from BEA -------------------------------------------------

library(bea.R)
beaKey <- '28B14069-EB85-4B08-B453-BD7808A6E4DD'

url <- "https://apps.bea.gov/api/data/"
params <- list(
  UserID = beaKey,
  Method = "GetData",
  datasetname = "Regional",
  TableName = "SAINC1",
  Frequency = "A",
  Year = "2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024",
  LineCode = "3",
  GeoFIPS = "STATE",
  ResultFormat = "JSON"
)

response <- GET(url, query = params)
data <- fromJSON(content(response, "text"))
head(data$BEAAPI$Results$Data)

pcpi <- as.data.frame(data$BEAAPI$Results$Data)

pcpi <- pcpi |>
  select(GeoName, TimePeriod, DataValue) |>
  rename(
    state = GeoName,
    years = TimePeriod,
    income = DataValue
  )


# Remove * and any trailing spaces from the state column
pcpi$state <- gsub("\\s*\\*", "", pcpi$state)

# Reclassify data fields
pcpi$years <- as.integer(pcpi$years)
pcpi$income <- as.numeric(pcpi$income)

# Adjust PCPI for Inflation -----------------------------------------------

#Input GDP Deflator from BEA
gdp_deflator <- c(97.316, 98.241, 100.000, 102.291, 103.979, 105.377, 110.186, 118.023, 122.390, 125.428)

#Vector of Years
years <- c(2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024)

#Index GDP Deflator
convert <-  function(i) {
  index <- gdp_deflator[10] / i
  return(index)
}

# Create dataframe for GDP deflator
deflator <- convert(gdp_deflator)
inflation <- as.data.frame(cbind(years, deflator))
inflation$years <- as.integer(inflation$years)

# Add GDP deflator to dataframe
pcpi <- pcpi |>
  left_join(inflation, by = 'years') |>
  mutate(adj_income = income * deflator)

# Pull Population data from Census ----------------------------------------

library(tidycensus)

# Set your Census API key
census_api_key('ef98a655402d76e1b61cda4102c8e15b72cd0fb8', install = TRUE, overwrite = TRUE)

# Years available for population estimates
years <- 2015:2024

# Retrieve state population estimates for each year
acs5_pop <- lapply(years, function(years) {
  get_acs(
    geography = "state",
    variables = "B01003_001",  # Total population
    years = year,
    survey = "acs5"            # 5-year ACS estimates
  ) |> 
    transform(years = years)
})

acs5_pop_estimates <- do.call(rbind, acs5_pop)

# Extract only the state name from the NAME column
acs5_pop_estimates$state <- sub(",.*", "", acs5_pop_estimates$NAME)

# Select only relevant columns: state, year, estimate
state_population <- acs5_pop_estimates[, c("state", "years", "estimate")]
state_population$years <- as.integer(state_population$years)

# Import TTR Data ---------------------------------------------------------

# Available at https://home.treasury.gov/policy-issues/economic-policy/total-taxable-resources

ttr <- read_csv('ttr.csv')

# Pivot TTR dataframe to long format
ttr_long <- ttr |>
  pivot_longer(
    cols = starts_with('20'),
    names_to = 'years',
    values_to = 'ttr',
  ) |>
  mutate(years = as.integer(years))

# Add GDP deflator to dataframe
ttr_long <- ttr_long |>
  left_join(inflation, by = 'years') |>
  mutate(ttr_adj = ttr * deflator)


# Load TTR per capita
ttr_pc <- read_csv('ttr_pc.csv')

# Pivot TTR per capita dataframe to long format
ttr_pc_long <- ttr_pc |>
  pivot_longer(
    cols = starts_with('20'),
    names_to = 'years',
    values_to = 'ttr_pc',
  ) |>
  mutate(years = as.integer(years))

# Add GDP deflator to dataframe
ttr_pc_long <- ttr_pc_long |>
  left_join(inflation, by = 'years') |>
  mutate(ttr_adj = ttr_pc * deflator)

# Clean Up ----------------------------------------------------------------
rm(acs5_pop, acs5_pop_estimates, data, params, response, convert,
   beaKey, url, gdp_deflator, ttr, ttr_pc, deflator, years)

# Export to Excel ---------------------------------------------------------

library(writexl)
write_xlsx(list('PCPI' = pcpi, 'State_Population' = state_population), 'Population-PCPI_data.xlsx')
