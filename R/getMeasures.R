#' COVID-19 Political Measures
#'
#' @param codes List or string of country codes
#' @param dates List of dates for which you want
#' the data (you can use the getDays function)
#' @param measure List or string for the measure you want data on.
#' You can find more information on the measures in the data dictionary of the
#' ECDC available here :
#' https://www.ecdc.europa.eu/en/publications-data/download-data-response-measures-covid-19
#'
#' @return A data frame containing the data for the political measures
#' against COVID-19 for each countries mentioned
#' @export
#'
#' @examples
#' getMeasures(codes = c("CH", "AT", "FR"), measure = "MassGatherAll", dates = "2020-03-01")

getMeasures <- function(codes = NULL, measure = NULL, dates = NULL) {

  # Parameters
  ## Codes
  if (is.null(codes) == FALSE) {
    country <- countrycode::countrycode(codes, origin = 'iso2c', destination = 'country.name')
  }
  # Get latest data
  options(warn = -1)

  i <- 0

  while (exists("csvFile") == FALSE) {

    skip <- FALSE

    date <- as.character(Sys.Date()-i)

    ## URL
    url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/response_graphs_data_", date, ".csv", sep = "")

    ## Get data
    tryCatch(csvFile <- utils::read.csv(url), error = function(e) { skip <<- TRUE})

    i <- i+1

  }

  options(warn = 0)

  # Data frame
  ## Duration
  if (is.null(dates) == FALSE) {
    for (i in 1:length(csvFile[,1])) {
      if (is.na(csvFile$date_start[i]) == FALSE && is.na(csvFile$date_end[i]) == FALSE) {
        # List days between dates
        results <- seq(as.Date(csvFile$date_start[i]), as.Date(csvFile$date_end[i]), by="days")
        # Append
        csvFile$duration[i] <- list(results)

      } else {
        csvFile$duration[i] <- NA
      }
    }
  }

  # Filter
  if (missing(codes) && missing(dates) && missing(measure)) {
    results <- csvFile
  } else if (missing(dates) && missing(measure)) {
    results <- dplyr::filter(csvFile, csvFile[1] %in% country)
  } else if (missing(codes) && missing(dates)) {
    results <- dplyr::filter(csvFile, csvFile[2] %in% measure)
  } else if (missing(dates)) {
    results <- dplyr::filter(csvFile, csvFile[1] %in% country, csvFile[2] %in% measure)
  }

  options(warn = -1)

  if (missing(dates) == FALSE) {
    y <- mapply(`%in%`, dates, csvFile$duration)
    if (missing(codes) == FALSE && missing(measure) == FALSE) {
      x <- subset(csvFile, y)
      results <- dplyr::filter(x, csvFile[1] %in% country, csvFile[2] %in% measure)
    } else if (missing(codes) == TRUE && missing(measure) == FALSE) {
      x <- subset(csvFile, y)
      results <- dplyr::filter(x, csvFile[2] %in% measure)
    } else if (missing(codes) == FALSE && missing(measure) == TRUE) {
      x <- subset(csvFile, y)
      results <- dplyr::filter(x, csvFile[1] %in% country)
    } else if (missing(codes) == TRUE && missing(measure) == TRUE) {
      results <- subset(csvFile, y)
    }
  }

  options(warn = 0)

  # Removing useless columns
  results <- subset(results, select = -5)
  # Renaming columns
  colnames <- c("country", "measure", "startDate", "endDate")
  colnames(results) <- colnames
  # Data types
  results$startDate <- as.Date(results$startDate)
  results$endDate <- as.Date(results$endDate)

  # Return results
  return(results)
}
