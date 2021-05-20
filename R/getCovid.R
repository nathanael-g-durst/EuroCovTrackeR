#' COVID-19 statistics
#'
#' @param codes List or string of country codes
#' @param dates List of dates for which you want
#' the data (you can use the getDays function)
#'
#' @return A data frame containing the number of cases and deaths for each
#' countries mentioned as well as the latest population data
#' @export
#'
#' @examples
#' getCovid()
#' getCovid(codes = "FR")
#' getCovid(dates = "2021-04-01")
#' getCovid(codes = c("AT", "FR"),
#' dates = getDays(startDate = "2021-04-01", endDate = "2021-04-04"))

getCovid <- function(codes = NULL, dates = NULL) {
  
  # URL
  url <- "https://opendata.ecdc.europa.eu/covid19/nationalcasedeath_eueea_daily_ei/json/"
  
  # Get data
  jsonFile <- as.data.frame(jsonlite::fromJSON(url))
  
  # Data type date
  if (missing(dates) == FALSE) {
    dates <- as.Date(dates, "%Y-%m-%d")
  }
  jsonFile[1] <- as.Date(jsonFile$records.dateRep, "%d/%m/%Y")
  
  # Filter
  if (missing(codes) && missing(dates)) {
    results <- jsonFile
  } else if (missing(dates)) {
    results <- filter(jsonFile, records.geoId == codes)
  } else if (missing(codes)) {
    results <- jsonFile[jsonFile$records.dateRep >= dates[1] & jsonFile$records.dateRep <= dates[length(dates)],]
  } else {
    results <- filter(jsonFile, records.geoId == codes)
    results <- results[results$records.dateRep >= dates[1] & results$records.dateRep <= dates[length(dates)],]
  }
  
  # Removing useless columns
  results <- subset(results, select = -c(2, 3, 4, 8, 9, 11))
  # Renaming columns
  colnames <- c("date", "cases", "death", "country", "latestPopulationData")
  colnames(results) <- colnames
  # Data types
  results$latestPopulationData <- as.numeric(results$latestPopulationData)
  
  # Return results
  return(results)
}
