#' COVID-19 Tests Statistics
#'
#' @param codes List or string of country codes
#' @param dates List of dates for which you want
#' the data (you can use the getDays function)
#'
#' @return A data frame containing the data on testing for each
#' countries mentioned
#' @export
#'
#' @examples
#' getTests(codes = c("AT", "FR"), dates = getDays(startDate = "2020-10-01", endDate = "2021-01-01"))

getTests <- function(codes = NULL, dates = NULL) {

  # Parameters
  ## Week
  if (is.null(dates) == FALSE) {
    week <- getWeeks(dates = dates, sym = TRUE)
  }

  # URL
  url <- "https://opendata.ecdc.europa.eu/covid19/testing/json/"

  # Get data
  jsonFile <- as.data.frame(jsonlite::fromJSON(url))

  # Filter
  if (missing(codes) && missing(dates)) {
    results <- jsonFile
  } else if (missing(dates)) {
    results <- dplyr::filter(jsonFile, jsonFile$country_code %in% codes)
  } else if (missing(codes)) {
    results <- dplyr::filter(jsonFile, jsonFile[,3] %in% week)
  } else {
    y <- dplyr::filter(jsonFile, jsonFile$country_code %in% codes)
    results <- dplyr::filter(y, y[,3] %in% week)
  }

  # Selecting national data
  results <- results[results$level == "national", ]
  # Selecting only useful columns
  results <- dplyr::select(results, 2, 3, 7, 8, 9, 10, 11)
  # Renaming columns
  colnames <- c("country", "week", "newCases", "testsDone", "latestPopulationData", "testingRate", "positivityRate")
  colnames(results) <- colnames

  # Return results
  return(results)
}
