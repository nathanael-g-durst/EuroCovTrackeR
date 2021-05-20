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
    results <- dplyr::filter(jsonFile, jsonFile$records.ReportingCountry %in% codes)
  } else if (missing(codes) && missing(vac) && missing(target)) {
    results <- dplyr::filter(jsonFile, jsonFile[1] %in% week)
  } else if (missing(codes) && missing(dates) && missing(target)) {
    results <- dplyr::filter(jsonFile, jsonFile$records.Vaccine == vac)
  } else if (missing(codes) && missing(dates) && missing(vac)) {
    results <- dplyr::filter(jsonFile, jsonFile$records.TargetGroup == target)
  } else if (missing(vac) && missing(target)) {
    y <- dplyr::filter(jsonFile, jsonFile$records.ReportingCountry %in% codes)
    results <- dplyr::filter(y, jsonFile[1] %in% week)
  } else if (missing(dates) && missing(target)) {
    results <- dplyr::filter(jsonFile, jsonFile$records.ReportingCountry %in% codes, jsonFile$records.Vaccine == vac)
  } else if (missing(dates) && missing(vac)) {
    results <- dplyr::filter(jsonFile, jsonFile$records.ReportingCountry %in% codes, jsonFile$records.TargetGroup == target)
  } else if (missing(codes) && missing(target)) {
    y <- dplyr::filter(jsonFile, jsonFile$records.Vaccine == vac)
    results <- dplyr::filter(y, jsonFile[1] %in% week)
  } else if (missing(codes) && missing(vac)) {
    y <- dplyr::filter(jsonFile, jsonFile$records.TargetGroup == target)
    results <- dplyr::filter(y, jsonFile[1] %in% week)
  } else if (missing(codes) && missing(dates)) {
    results <- dplyr::filter(jsonFile, jsonFile$records.Vaccine == vac, jsonFile$records.TargetGroup == target)
  } else if (missing(target)) {
    y <- dplyr::filter(jsonFile, jsonFile$records.ReportingCountry %in% codes, jsonFile$records.Vaccine == vac)
    results <- dplyr::filter(y, jsonFile[1] %in% week)
  } else if (missing(vac)) {
    y <- dplyr::filter(jsonFile, jsonFile$records.ReportingCountry %in% codes, jsonFile$records.TargetGroup == target)
    results <- dplyr::filter(y, jsonFile[1] %in% week)
  } else if (missing(dates)) {
    results <- dplyr::filter(jsonFile, jsonFile$records.ReportingCountry %in% codes, jsonFile$records.Vaccine == vac, jsonFile$records.TargetGroup == target)
  } else if (missing(codes)) {
    y <- dplyr::filter(jsonFile, jsonFile$records.Vaccine == vac, jsonFile$records.TargetGroup == target)
    results <- dplyr::filter(y, jsonFile[1] %in% week)
  } else {
    y <- dplyr::filter(jsonFile, jsonFile$records.ReportingCountry %in% codes, jsonFile$records.Vaccine == vac, jsonFile$records.TargetGroup == target)
    results <- dplyr::filter(y, jsonFile[1] %in% week)
  }

  # Selecting national data
  codes <- as.character(unique(unlist(results$records.ReportingCountry)))
  results <- dplyr::filter(results, results$records.Region %in% codes)
  # Removing useless columns
  results <- subset(results, select = -7)
  # Renaming columns
  colnames <- c("week", "firstDose", "refusedVaccines", "secondDose", "doseUnspecified", "dosesDelivered",
                "latestPopData", "country", "targetGroup", "vaccine", "targetGroupPop")
  colnames(results) <- colnames

  # Return results
  return(results)
}
