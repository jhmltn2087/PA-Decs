# READ ME FIRST
# THIS SCRPIT AUTOMATICALLY TAKES DATA FROM THE OPENFEMA DATABASE, PROJECT DATA IS PULLED AND CONSOLODIATED INTO DISASTER LEVEL DATA
# INSTALL PACKAGES: httr, jsonlite, tidyverse, bea.R, and tidycensus

library(httr)
library(jsonlite)
library(tidyverse)

states <- c("AL", "AK", "AS", "AZ", "AR", "CA", "CO", "CT", "DC", "DE", "FL", "GA", "GU", "HI", "ID", "IL", "IN", "IA", "KS", 
            "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", 
            "NC", "ND", "MP", "OH", "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN", "TX", "VI", "UT", "VT", "VA", "WA", "WV", "WI", "WY")
years <- 2015:2024
all_data <- list()

for (state in states) {
  for (year in years) {
    skip <- 0
    repeat {
      filter_value <- paste0(
        "declarationDate ge ", year, "-01-01 and declarationDate le ", year, "-12-31 and stateAbbreviation eq '", state, "'"
      )
      filter_encoded <- URLencode(filter_value, reserved = TRUE)
      url <- paste0(
        "https://www.fema.gov/api/open/v2/PublicAssistanceFundedProjectsDetails?$filter=",
        filter_encoded, "&$top=1000&$skip=", skip
      )
      # Add error handling for failed requests
      response <- try(GET(url), silent = TRUE)
      if (inherits(response, "try-error") || response$status_code != 200) {
        cat("Error downloading data for", state, "in", year, "at skip =", skip, "\n")
        break
      }
      pa_data <- fromJSON(content(response, "text"), flatten = TRUE)
      if (!is.null(pa_data$PublicAssistanceFundedProjectsDetails) && 
          is.data.frame(pa_data$PublicAssistanceFundedProjectsDetails) && 
          nrow(pa_data$PublicAssistanceFundedProjectsDetails) > 0) {
        # Store by state and year for easier troubleshooting
        all_data[[paste(state, year, skip, sep = "_")]] <- pa_data$PublicAssistanceFundedProjectsDetails
        skip <- skip + 1000
        cat("Downloaded:", skip, "records for", state, "in", year, "\n")
      } else {
        break
      }
    }
  }
}
library(dplyr)
# Exclude disaster numbers less than 4205
pa <- do.call(rbind, all_data)
pa <- pa |> filter(disasterNumber >= 4205)

# Format declarationDate and convert to year
pa$years <- format(as.Date(pa$declarationDate), "%Y")
pa$declarationDate <- ymd_hms(pa$declarationDate)

# Select only the desired columns (adjust column names as needed)
pa_final <- pa[, c("disasterNumber", "declarationDate", "years", "stateAbbreviation", "projectAmount", "incidentType", "damageCategoryCode", 
                   "federalShareObligated", "mitigationAmount")]

# Remove Covid-19 disasters
pa_final <- pa_final |>
  filter(incidentType != 'Biological')

# Combine Projects by Disaster --------------------------------------------

pa_disasters <- pa_final |>
  group_by(disasterNumber, stateAbbreviation, years, declarationDate) |>
  summarize(
    total_amount = sum(projectAmount, na.rm = TRUE), 
    mitigationAmt = sum(mitigationAmount, na.rm = TRUE), 
    .groups = 'drop') |>
  mutate(years = as.integer(years))


# Adjust for Inflation ----------------------------------------------------

#Input GDP Deflator from BEA
gdp_deflator <- c(97.316, 98.241, 100.000, 102.291, 103.979, 105.377, 110.186, 118.023, 122.390, 125.428)

#Vector of Years
years <- as.integer(c(2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024))

#Index GDP Deflator
convert <-  function(i) {
  index <- gdp_deflator[10] / i
  return(index)
}

# Create dataframe for GDP deflator
deflator <- convert(gdp_deflator)
inflation <- data.frame(years = years, deflator = deflator)

# Add GDP deflator to dataframe
pa_adjusted <- pa_disasters |>
  left_join(inflation, by = 'years') |>
  mutate(
    total_amount = replace_na(total_amount, 0),
    mitigationAmt = replace_na(mitigationAmt, 0),
    adj_T_amount = total_amount * deflator,
    adj_mitigation = mitigationAmt * deflator,
    adj_amount = ifelse(adj_T_amount + adj_mitigation >= 1,
                        adj_T_amount + adj_mitigation,
                        adj_T_amount)
      )


# Create a matrix of states and abbreviations for joining
state <- c('Alabama', 'Alaska', 'Arizona', 'Arkansas', 'California', 'Colorado', 
           'Connecticut', 'Delaware', 'Florida', 'Georgia', 'Hawaii', 'Idaho', 
           'Illinois', 'Indiana', 'Iowa', 'Kansas', 'Kentucky', 'Louisiana', 
           'Maine', 'Maryland', 'Massachusetts', 'Michigan', 'Minnesota', 
           'Mississippi', 'Missouri', 'Montana', 'Nebraska', 'Nevada', 
           'New Hampshire', 'New Jersey', 'New Mexico', 'New York', 
           'North Carolina', 'North Dakota', 'Ohio', 'Oklahoma', 'Oregon', 
           'Pennsylvania', 'Rhode Island', 'South Carolina', 'South Dakota', 
           'Tennessee', 'Texas', 'Utah', 'Vermont', 'Virginia', 'Washington', 
           'West Virginia', 'Wisconsin', 'Wyoming', 'District of Columbia', 
           'American Samoa', 'Guam', 'Northern Mariana Islands', 'Puerto Rico', 
           'U.S. Virgin Islands')

abbr <- c('AL', 'AK', 'AZ', 'AR', 'CA', 'CO', 'CT', 'DE', 'FL', 'GA', 'HI', 'ID', 
          'IL', 'IN', 'IA', 'KS', 'KY', 'LA', 'ME', 'MD', 'MA', 'MI', 'MN', 'MS', 
          'MO', 'MT', 'NE', 'NV', 'NH', 'NJ', 'NM', 'NY', 'NC', 'ND', 'OH', 'OK', 
          'OR', 'PA', 'RI', 'SC', 'SD', 'TN', 'TX', 'UT', 'VT', 'VA', 'WA', 'WV', 
          'WI', 'WY', 'DC', 'AS', 'GU', 'MP', 'PR', 'VI')

state_abv <- data.frame(state, abbr, stringsAsFactors = FALSE)

pa_adjusted <- pa_adjusted |>
  left_join(state_abv, by = c('stateAbbreviation' = 'abbr')) |>
  select(-stateAbbreviation)


# Export to Excel ---------------------------------------------------------

library(writexl)
write_xlsx(list('PA_Decs_15-24' = pa_adjusted), 'PA_Disaster_data.xlsx')


#DR 4339 value correction for PR adj_amount based on FIDA report-----------

library(readxl)
pa_adjusted <- read_excel("PA_Disaster_data.xlsx")

#FIDA value * deflator value 2017
pa_adjusted$adj_amount[pa_adjusted$disasterNumber == 4339] <- 50962155472 * 1.25428

library(writexl)
write_xlsx(pa_adjusted, "PA_Disaster_data.xlsx")


# Clean Up after Ourselves ------------------------------------------------

rm(all_data, pa_final, pa_data, response, filter_encoded, 
   filter_value, skip, url, years, deflator, inflation, convert, state_abv,
   abbr, gdp_deflator, state)
