#' Define injury case. Generally an episode shoulde be:
#' - Main diagnosis is between S00 to T78
#' - It's an acute injury ie. hastegrad is 1
#' - Posibility to select days from previous to the following injury of similar code for multiple injuries
#' @param d Dataset
#' @param year Year to select
#' @param period Representing 4-months period ie. first, second or third.
#'   Default is 0 to include data for the whole period else use 1, 2 or 3.
#' @param date.col Columname for date for filtering
#' @param acute Default is 1. To select all use 2
#' @param days If diffence in days should be considered ie. when a person has
#'   more than one registered injuries of the same ICD-10 code
#' @param diag.col Columname of codes for main diagnosis

find_case <- function(d, year, period = 0, date.col = "innDato",
                      acute = 1,
                      days = 0,
                      diag.col = "hoveddiagnoser"){

  # Create dummy year for filtering
  if (!missing(year)){
    d[, d.year := lubridate::year(x), env = list(x = date.col)]
    d <- d[d.year == year]
  }

  if (period != 0){
    d[, d.month := lubridate::month(x), env = list(x = date.col)]
    d <- d[d.month %in% period]
  }

  ## Include only codes S00 - T78 as main diagnosis
  d <- get_valid_codes(d = d, "hoveddiagnoser", "hovdiag")
  d <- d[hovdiag == 1]

  if (acute == 1)
    d <- d[Hastegrad == 1]

  # Get difference in days from previous to the following injuries when multiple injuries
  if (days != 0){
    d[, days := x - shift(x, type = "lag"), by = lopenr, env = list(x = date.col)]
  }

  # Dummy columns with  prefix "d."
  d.cols <- c("d.year", "d.month")
  d[, (d.cols) := NULL][]

}
