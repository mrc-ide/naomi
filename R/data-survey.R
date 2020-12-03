#' Convert Date to Century Month Code (CMC)
#'
#' Converts a date to DHS Century Month Code (CMC).
#'
#' @param date a Date vector
#'
#' @return an integer vector of CMC dates
#'
#' @details
#' CMC date is defined as the number of months since 1900:
#' \deqn{cmc = (year - 1900) * 12 + momth}
#'
#' @references
#' https://dhsprogram.com/Data/Guide-to-DHS-Statistics/Organization_of_DHS_Data.htm?rhtocid=_4_2_0#Structure_of_DHS_Databc-1
#'
#' @examples
#' cmc_date(Sys.Date())
#' cmc_date(as.Date("1987-02-11", format = "%Y-%m-%d"))
#'
#' @export
cmc_date <- function(date) {
  stopifnot(inherits(date, "Date"))
  12 * (as.integer(format(date, "%Y")) - 1900) + as.integer(format(date, "%m"))
}

