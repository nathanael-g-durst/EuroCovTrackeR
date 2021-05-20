#' List all days between two dates
#'
#' @param startDate A string
#' @param endDate A string
#'
#' @return A vector containing the list of all days between
#' the startDate and the endDate
#' @export
#'
#' @examples
#' getDays(startDate = "2021-04-01", endDate = "2021-04-04")

getDays <- function(startDate, endDate) {
  
  # List days between dates
  results <- seq(as.Date(startDate), as.Date(endDate), by="days")
  
  # Return results
  return(results)
}
