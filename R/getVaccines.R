#' COVID-19 Vaccines Statistics
#'
#' @param codes List or string of country codes
#' @param dates List of dates for which you want
#' the data (you can use the getDays function)
#' @param vac A string as identifier of the vaccine you want data on.
#' You can find more information on the measures in the data dictionary of the
#' ECDC available here :
#' https://www.ecdc.europa.eu/en/publications-data/covid-19-testing
#' @param target A string as identifier of the target group for the vaccine
#' you want data on.
#' You can find more information on the measures in the data dictionary of the
#' ECDC available here :
#' https://www.ecdc.europa.eu/en/publications-data/covid-19-testing
#'
#' @return A data frame containing the data on the vaccines
#' against COVID-19 for each countries mentioned
#' @export
#'
#' @examples
#' getVaccines(codes = c("DE", "FR"), dates = "2021-01-01", vac = "AZ", target = "ALL")

getVaccines <- function(codes = NULL, dates = NULL, vac = NULL, target = NULL) {
  
  # Parameters
  ## Week
  if (is.null(dates) == FALSE) {
    week <- getWeeks(dates = dates, sym = TRUE)
  }
  
  # URL
  url <- "https://opendata.ecdc.europa.eu/covid19/vaccine_tracker/json/"
  
  # Get data
  jsonFile <- as.data.frame(jsonlite::fromJSON(url))
  
  # Filter
  if (missing(codes) && missing(dates) && missing(vac) && missing(target)) {
    results <- jsonFile
  } else if (missing(dates) && missing(vac) && missing(target)) {
    results <- filter(jsonFile, records.ReportingCountry %in% codes)
  } else if (missing(codes) && missing(vac) && missing(target)) {
    results <- filter(jsonFile, records.YearWeekISO %in% week)
  } else if (missing(codes) && missing(dates) && missing(target)) {
    results <- filter(jsonFile, records.Vaccine == vac)
  } else if (missing(codes) && missing(dates) && missing(vac)) {
    results <- filter(jsonFile, records.TargetGroup == target)
  } else if (missing(vac) && missing(target)) {
    y <- filter(jsonFile, records.ReportingCountry %in% codes)
    results <- filter(y, records.YearWeekISO %in% week)
  } else if (missing(dates) && missing(target)) {
    results <- filter(jsonFile, records.ReportingCountry %in% codes, records.Vaccine == vac)
  } else if (missing(dates) && missing(vac)) {
    results <- filter(jsonFile, records.ReportingCountry %in% codes, records.TargetGroup == target)
  } else if (missing(codes) && missing(target)) {
    y <- filter(jsonFile, records.Vaccine == vac)
    results <- filter(y, records.YearWeekISO %in% week)
  } else if (missing(codes) && missing(vac)) {
    y <- filter(jsonFile, records.TargetGroup == target)
    results <- filter(y, records.YearWeekISO %in% week)
  } else if (missing(codes) && missing(dates)) {
    results <- filter(jsonFile, records.Vaccine == vac, records.TargetGroup == target)
  } else if (missing(target)) {
    y <- filter(jsonFile, records.ReportingCountry %in% codes, records.Vaccine == vac)
    results <- filter(y, records.YearWeekISO %in% week)
  } else if (missing(vac)) {
    y <- filter(jsonFile, records.ReportingCountry %in% codes, records.TargetGroup == target)
    results <- filter(y, records.YearWeekISO %in% week)
  } else if (missing(dates)) {
    results <- filter(jsonFile, records.ReportingCountry %in% codes, records.Vaccine == vac, records.TargetGroup == target)
  } else if (missing(codes)) {
    y <- filter(jsonFile, records.Vaccine == vac, records.TargetGroup == target)
    results <- filter(y, records.YearWeekISO %in% week)
  } else {
    y <- filter(jsonFile, records.ReportingCountry %in% codes, records.Vaccine == vac, records.TargetGroup == target)
    results <- filter(y, records.YearWeekISO %in% week)
  }
  
  # Selecting national data
  codes <- as.character(unique(unlist(results$records.ReportingCountry)))
  results <- filter(results, results$records.Region %in% codes)
  # Removing useless columns
  results <- subset(results, select = -7)
  # Renaming columns
  colnames <- c("week", "firstDose", "refusedVaccines", "secondDose", "doseUnspecified", "dosesDelivered",
                "latestPopData", "country", "targetGroup", "vaccine", "targetGroupPop")
  colnames(results) <- colnames
  
  # Return results
  return(results)
}
