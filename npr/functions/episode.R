#' Define injury case or episode. Generally an episode shoulde be:
#' - Main diagnosis is between S00 to T78
#' - It's an acute injury ie. hastegrad is 1
#' - Posibility to select days from previous to the following injury of similar code for multiple injuries
#' @param d Dataset
#' @param year Year to select
#' @param period Representing 4-months period ie. first, second or third.
#'   Default is 0 to include data for the whole period else use 1, 2 or 3.
#' @param date.col Columname for date for filtering
#' @param acute Default is `FALSE`. Use `TRUE` to include only acute patients ie. Hastegrad = 1
#' @param days If diffence in days should be considered ie. when a person has
#'   more than one registered injuries of the same ICD-10 code
#' @param diag.col Columname of codes for main diagnosis
#' @examples
#' dd <- find_episode(dt1, year = 2022, period = 1:2, acute = TRUE)

find_episode <- function(d, year, period = 0,
                         date.col = "innDato",
                         acute = FALSE,
                         days = 0,
                         diag.col = "hoveddiagnoser"){

  # Keep dummy columns with prefix "d." for deletion
  d.cols <- NULL

  # Create dummy year for filtering
  if (!missing(year)){
    d[, d.year := lubridate::year(x), env = list(x = date.col)]
    d <- d[d.year == year]
    d.cols <- append(d.cols, "d.year")
  }

  if (any(period %in% 1:3)){
    d[, d.month := lubridate::month(x), env = list(x = date.col)]
    d[, d.tertial := data.table::fcase(d.month %in% 1:4, 1,
                                       d.month %in% 5:8, 2,
                                      d.month %in% 9:12, 3)]

    d <- d[d.tertial %in% period]
    d.cols <- append(d.cols, c("d.month", "d.tertial"))
  }

  ## Include only codes S00 - T78 as main diagnosis
  d <- get_valid_codes(d = d, diag.col, "hovdiag")
  d <- d[hovdiag == 1]

  if (acute)
    d <- d[Hastegrad == 1]

  # Get difference in days from previous to the following injuries when multiple injuries
  if (days != 0){
    d[, days := x - shift(x, type = "lag"), by = lopenr, env = list(x = date.col)]
    d <- check_codes(d = d, id = "lopenr", col = diag.col, cond = days)
  }

  # Dummy columns with  prefix "d."
  d[, (d.cols) := NULL][]

}


#' Find similar codes for selected period
#' @param d Dataset
#' @param id Unque id
#' @param col Column name for codes selection
#' @param cond Condition for selection eg. 3 or 5 days
#' @examples
#' dj <- check_codes(dx, "lopenr", "hoveddiagnoser", 3)
check_codes <- function(d, id, col, cond){
  d[, dx := data.table::shift(x), by = y, env = list(x = col, y = id)]
  d[, case := data.table::fifelse(days == 3 & x == dx, 1, 0), by = y, env = list(x = col, y = id)]
  d[, dx := NULL]
}

#OBS!! What happen when hoveddiagnoser has more than one codes??
# - All codes must exist in the following episode?
# - At least one exists in the following episode?
