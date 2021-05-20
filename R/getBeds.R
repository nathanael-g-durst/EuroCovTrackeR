#' COVID-19 Hospital statistics
#'
#' @param codes List or string of country codes
#' @param dates List of dates for which you want
#' the data (you can use the getDays function)
#' @param indicator List of string of the indicator you want.
#' You can have :
#' Daily ICU occupancy (ICU)
#' Weekly new ICU admissions per 100k (ICUPER)
#' Daily hospital occupancy (HOSP)
#' Weekly new hospital admissions per 100k (HOSPPER)
#'
#' @return A data frame containing the data for the indicator selected for each
#' countries mentioned
#' @export
#'
#' @examples
#' getBeds(codes = c("BE", "FR"), dates = c("2021-01-03", "2021-01-04"), indicator = "icu")

getBeds <- function(codes = NULL, dates = NULL, indicator = NULL) {

  # Parameters
  ## Codes
  if (is.null(codes) == FALSE) {
    country <- countrycode::countrycode(codes, origin = 'iso2c', destination = 'country.name')
  }
  ## Indicators
  if (is.null(indicator) == FALSE) {
    if (indicator == "icu" || indicator == "ICU" || indicator == "Icu") {
      indicator <- "Daily ICU occupancy"
    } else if (indicator == "icuper" || indicator == "icuPer" || indicator == "ICUPer" || indicator == "ICUPER" || indicator == "ICUper") {
      indicator <- "Weekly new ICU admissions per 100k"
    } else if (indicator == "hosp" || indicator == "Hosp" || indicator == "hospital" || indicator == "Hospital") {
      indicator <- "Daily hospital occupancy"
    } else if (indicator == "hospper" || indicator == "hospPer" || indicator == "HospPer" || indicator == "hospitalper" || indicator == "HospitalPer" || indicator == "HospitalPer") {
      indicator <- "Weekly new hospital admissions per 100k"
    }
  }

  # URL
  url <- "https://opendata.ecdc.europa.eu/covid19/hospitalicuadmissionrates/json/"

  # Get data
  jsonFile <- as.data.frame(jsonlite::fromJSON(url))

  # Filter
  if (missing(codes) && missing(dates) && missing(indicator)) {
    results <- jsonFile
  } else if (missing(dates) && missing(indicator)) {
    results <- dplyr::filter(jsonFile, jsonFile[1] == country)
  } else if (missing(codes) && missing(indicator)) {
    results <- dplyr::filter(jsonFile, date %in% as.character(dates))
  } else if (missing(codes) && missing(dates)) {
    results <- dplyr::filter(jsonFile, jsonFile[2] == indicator)
  } else if (missing(indicator)) {
    y <- dplyr::filter(jsonFile, jsonFile[1] == country)
    results <- dplyr::filter(y, date %in% as.character(dates))
  } else if (missing(dates)) {
    results <- dplyr::filter(jsonFile, jsonFile[1] %in% country, jsonFile[2] == indicator)
  } else if (missing(codes)) {
    y <- dplyr::filter(jsonFile, jsonFile[2] == indicator)
    results <- dplyr::filter(y, date %in% as.character(dates))
  } else {
    y <- dplyr::filter(jsonFile, jsonFile[1] %in% country, jsonFile[2] == indicator)
    results <- dplyr::filter(y, date %in% as.character(dates))
  }

  # Removing useless columns
  results <- subset(results, select = -c(5, 6))
  # Renaming columns
  colnames <- c("country", "indicator", "date", "week", "numberBeds")
  colnames(results) <- colnames
  # Data types
  results$date <- as.Date(results$date)

  # Return results
  return(results)
}
