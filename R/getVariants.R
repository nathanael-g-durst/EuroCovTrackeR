#' COVID-19 Variants Statistics
#'
#' @param codes List or string of country codes
#' @param dates List of dates for which you want
#' the data (you can use the getDays function)
#' @param var String as identifier of the variant you want data on.
#' You can find more information on the measures in the data dictionary of the
#' ECDC available here :
#' www.ecdc.europa.eu/en/publications-data/data-virus-variants-covid-19-eueea
#'
#' @return A data frame containing the data for the political measures
#' against COVID-19 for each countries mentioned
#' @export
#'
#' @examples
#' getVariants(codes = c("AT", "DE"), dates = getDays(startDate = "2020-12-01",
#' endDate = "2021-01-01"), var = "Other")

getVariants <- function(codes = NULL, dates = NULL, var = NULL) {

  # Parameters
  ## Week
  if (is.null(dates) == FALSE) {
    week <- getWeeks(dates = dates, sym = FALSE)
  }

  # URL
  url <- "https://opendata.ecdc.europa.eu/covid19/virusvariant/json"

  # Get data
  jsonFile <- as.data.frame(jsonlite::fromJSON(url))

  # Filter
  if (missing(codes) && missing(dates) && missing(var)) {
    results <- jsonFile
  } else if (missing(dates) && missing(var)) {
    results <- dplyr::filter(jsonFile, jsonFile$country_code %in% codes)
  } else if (missing(codes) && missing(var)) {
    results <- dplyr::filter(jsonFile, jsonFile[,3] %in% week)
  } else if (missing(codes) && missing(dates)) {
    results <- dplyr::filter(jsonFile, jsonFile[,9] %in% var)
  } else if (missing(var)) {
    y <- dplyr::filter(jsonFile, jsonFile$country_code %in% codes)
    results <- dplyr::filter(y, y[,3] %in% week)
  } else if (missing(dates)) {
    results <- dplyr::filter(jsonFile, jsonFile$country_code %in% codes, jsonFile[,9] %in% var)
  } else if (missing(codes)) {
    y <- dplyr::filter(jsonFile, jsonFile[,9] %in% var)
    results <- dplyr::filter(y, y[,3] %in% week)
  } else {
    y <- dplyr::filter(jsonFile, jsonFile$country_code %in% codes)
    z <- dplyr::filter(y, y[,9] %in% var)
    results <- dplyr::filter(z, z[,3] %in% week)
  }

  # Removing useless columns
  results <- subset(results, select = -c(2,4))
  # Renaming columns
  colnames <- c("country", "week", "newCases", "testSequenced", "percentSequenced", "validDenominator",
                "variant", "numberDetected", "percentVariant")
  colnames(results) <- colnames

  # Return results
  return(results)
}
