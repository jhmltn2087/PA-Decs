# THIS SCRIPT PULLS IA PROJECT LEVEL DATA FROM OPENFEMA USING THE IA VALID REGISTRATIONS DATABASE
# EXPECT THIS TO TAKE SEVERAL HOURS TO A DAY OR TWO TO RUN SINCE IT PULLS ~25 MILLION RECORDS
# THE FOLLOWING PACKAGES ARE NEEDED: tidyverse, foreach, doParallel
# THE SCRIPT WILL PRODUCE A SPREADSHEET WITH IA DATA AT THE DISASTER LEVEL

library(tidyverse)
library(foreach)
library(doParallel)

state_abbr <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", 
            "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", 
            "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", 
            "WI", "WY", 'DC', 'AS', 'GU', 'MP', 'PR', 'VI')

years <- 2015:2024

num_cores <- min(4, parallel::detectCores()) # Adjust number of cores as needed
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Function to fetch all pages for a single state and year with retry logic
fetch_state_year_data <- function(state, year) {
  skip <- 0
  state_year_data <- list()
  max_retries <- 3
  repeat {
    filter_value <- paste0("declarationDate ge ", year, "-01-01 and declarationDate le ", year, "-12-31 and damagedStateAbbreviation eq '", state, "'")
    filter_encoded <- URLencode(filter_value, reserved = TRUE)
    url <- paste0("https://www.fema.gov/api/open/v2/IndividualsAndHouseholdsProgramValidRegistrations?$filter=", 
                  filter_encoded, "&$top=1000&$skip=", skip)
    retry_count <- 0
    success <- FALSE
    while (retry_count < max_retries && !success) {
      response <- GET(url, timeout(120))
      if (http_error(response)) {
        retry_count <- retry_count + 1
        Sys.sleep(5)
      } else {
        success <- TRUE
      }
    }
    if (!success) break
    # Defensive check for API content
    content_text <- content(response, "text")
    if (is.null(content_text) || nchar(content_text) == 0) break
    ihp_data <- tryCatch(fromJSON(content_text, flatten = TRUE), error = function(e) NULL)
    if (is.null(ihp_data)) break
    records <- ihp_data$IndividualsAndHouseholdsProgramValidRegistrations
    # Defensive check for records
    if (!is.null(records) && is.data.frame(records) && nrow(records) > 0) {
      state_year_data[[length(state_year_data) + 1]] <- records
      skip <- skip + 1000
      Sys.sleep(1.0) # Respect API rate limits
    } else {
      break
    }
  }
  if (length(state_year_data) > 0) {
    return(bind_rows(state_year_data))
  } else {
    return(NULL)
  }
}

# Parallel loop over states and years
all_data <- tryCatch({
  foreach(state = state_abbr, .combine = bind_rows, .packages = c("httr", "jsonlite", "dplyr", "lubridate")) %:%
    foreach(year = years, .combine = bind_rows) %dopar% {
      cat("Starting download for", state, "in", year, "\n")
      tryCatch(fetch_state_year_data(state, year), error = function(e) { 
        cat("Error for", state, year, ":", e$message, "\n")
        NULL 
      })
    }
}, error = function(e) {
  cat("Parallel loop failed:", e$message, "\n")
  data.frame() # Return empty data frame if failure
})


stopCluster(cl)

# Check if all_data exists and has rows
if (!exists("all_data") || nrow(all_data) == 0) {
  stop("No data was downloaded. Check for errors in the fetch_state_year_data function or API availability.")
}

# Continue with processing
all_data <- all_data |>
  mutate(declarationDate = ymd_hms(declarationDate),
         year = year(declarationDate))

# Select desired columns
cols <- c("disasterNumber", "declarationDate", "year", "damagedStateAbbreviation", 
          "ihpAmount", "rpfvl", "haAmount", "onaAmount", "onaFuneralAssistAmount")
ihp_final <- all_data[, intersect(cols, names(all_data))]

# Adjust for Covid-19 Funeral Assistance
ihp_final <- ihp_final |>
  mutate(ihp_less_funeral = ihpAmount - onaFuneralAssistAmount) |>
  filter(ihp_less_funeral > 0)

# Combine Projects by disaster
ihp_disasters <- ihp_final |>
  group_by(disasterNumber, damagedStateAbbreviation, year, declarationDate) |>
  summarize(total_amount = sum(ihpAmount, na.rm = TRUE),  .groups = 'drop') |>
  mutate(year = as.factor(year))

# Remove $0 disasters
ihp_disasters <- ihp_disasters |>
  mutate(across(where(is.numeric), ~na_if(., 0)))

# Adjust for Inflation ----------------------------------------------------

#Input GDP Deflator from BEA
gdp_deflator <- c(97.316, 98.241, 100.000, 102.291, 103.979, 105.377, 110.186, 118.023, 122.390, 125.428)

#Vector of Years
years_vec <- c(2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024)

#Index GDP Deflator
convert <-  function(i) {
  index <- gdp_deflator[10] / i
  return(index)
}

# Create dataframe for GDP deflator
deflator <- convert(gdp_deflator)
inflation <- as.data.frame(cbind(years_vec, deflator))
inflation$years_vec <- as.factor(inflation$years_vec)

# Add GDP deflator to dataframe
ihp_adjusted <- ihp_disasters |>
  left_join(inflation, by = c('year' = 'years_vec')) |>
  mutate(adj_amount = total_amount * deflator)

# Create a matrix of states and abbreviations for joining
state_names <- c('Alabama', 'Alaska', 'Arizona', 'Arkansas', 'California', 'Colorado', 
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

state_abv <- data.frame(state = state_names, abbr = state_abbr, stringsAsFactors = FALSE)

ihp_adjusted <- ihp_adjusted |>
  left_join(state_abv, by = c('damagedStateAbbreviation' = 'abbr')) |>
  select(-damagedStateAbbreviation)

# Export to Excel ---------------------------------------------------------
# WARNING: Excel has a row limit (~1,048,576). If your dataset exceeds this, use CSV.

if (nrow(ihp_adjusted) > 1000000) {
  write.csv(ihp_adjusted, "IA_Disaster_data.csv", row.names = FALSE)
  message("Data written to CSV due to Excel row limit.")
} else {
  write_xlsx(list('IA_Decs_15-24' = ihp_adjusted), 'IA_Disaster_data.xlsx')
  message("Data written to Excel.")
}

# Clean up
rm(all_data, inflation, state_abv, state_abbr, state_names, years, abbr, cols, deflator, gdp_deflator,
   convert, fetch_state_year_data, num_cores, cl, years_vec)