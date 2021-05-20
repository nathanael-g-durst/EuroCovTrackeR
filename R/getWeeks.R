#' Get the ISO week of dates with or without a W
#'
#' @param dates A string
#' @param sym A logical
#'
#' @return A vector containing the list of the ISO weeks of dates
#' @export
#'
#' @examples
#' getWeeks(dates = c("2021-04-01", "2021-04-04", "2021-05-06"), sym = TRUE)
#' getWeeks(dates = c("1962-03-26", "1966-10-06", "1998-07-21", "2000-03-08"), sym = FALSE)

getWeeks <- function (dates, sym = TRUE) {
  
  # Create vector
  results <- NULL
  
  # List days between dates
  if (sym == TRUE) {
    for (i in 1:length(dates)) {
      if (as.numeric(isoweek(dates[i]) < 9)) {
        week <- paste("0", isoweek(dates[i]), sep = "")
      } else {
        week <- isoweek(dates[i])
      }
      year <- isoyear(dates[i])
      results <- append(results, paste(year, "-W", week, sep = ""), after = length(results))
    }
  } else {
    for (i in 1:length(dates)) {
      if (as.numeric(isoweek(dates[i]) < 9)) {
        week <- paste("0", isoweek(dates[i]), sep = "")
      } else {
        week <- isoweek(dates[i])
      }
      year <- isoyear(dates[i])
      results <- append(results, paste(year, "-", week, sep = ""), after = length(results))
    }
  }
  
  results <- unique(results)
  
  # Return results
  return(results)
}
