# Function to convert calendar date to fiscal year
calendar_to_fiscal_year <- function(date) {
  # Ensure date is Date class
  date <- as.Date(date)
  year <- as.numeric(format(date, "%Y"))
  month <- as.numeric(format(date, "%m"))
  
  # Fiscal year starts in October
  fiscal_year <- ifelse(month >= 10, year + 1, year)
  return(fiscal_year)
}

# Apply fiscal year conversion to PA data
pa_adjusted$year <- calendar_to_fiscal_year(pa_adjusted$declarationDate)
