#' Define injury dup or episode. Generally an episode shoulde be:
#' - Main diagnosis is between S00 to T78
#' - It's an acute injury ie. hastegrad is 1
#' - Posibility to select days from previous to the following injury of similar code for multiple injuries
#' @param d Dataset
#' @param period Representing 4-months period ie. first, second or third.
#'   Default is 0 to include data for the whole period else use 1, 2 or 3.
#' @param date.col Columname for date for filtering
#' @param id Columname representing unique id
#' @param acute Default is `FALSE`. Use `TRUE` to include only acute patients ie. Hastegrad = 1
#' @param days If diffence in days should be considered ie. when a person has
#'   more than one registered injuries of the same ICD-10 code
#' @param diag.col Columname of codes for main diagnosis
#' @examples
#' dd <- find_episode(dt1, year = 2022, period = 1:2, acute = TRUE)
#' dd <- find_episode(dt1, year = 2022, acute = TRUE, days = 3)

find_episode <- function(d, period = 0,
                         id = "lopenr",
                         date.col = "innDato",
                         acute = FALSE,
                         days = 0,
                         diag.col = "hoveddiagnoser"){

  d <- data.table::copy(d)

  # Keep dummy columns with prefix "d." for deletion
  d.cols <- NULL

  # Cleaning and restructuring
  d <- unique(d)
  data.table::setkeyv(d, c(id, date.col))

  if (any(period %in% 1:3)){
    d[, d.month := data.table::month(x), env = list(x = date.col)]
    d[, d.tertial := data.table::fcase(d.month %in% 1:4, 1,
                                       d.month %in% 5:8, 2,
                                       d.month %in% 9:12, 3)]

    d <- d[d.tertial %in% period]
    d.cols <- append(d.cols, c("d.month", "d.tertial"))
  }

  ## Include only codes S00 - T78 as main diagnosis
  d <- get_valid_codes(d = d, diag.col, "hovdiag", split = " ")
  d <- d[hovdiag == 1]

  if (acute)
    d <- d[Hastegrad == 1]

  # Get difference in days from previous to the following injuries when
  # multiple injuries are registered
  if (days != 0){
    d[, days := x - data.table::shift(x, type = "lag"), by = lopenr, env = list(x = date.col)]
    d <- check_codes(d = d, id = "lopenr", col = diag.col, cond = days)
  }

  # Dummy columns with  prefix "d."
  if (!is.null(d.cols))
    d[, (d.cols) := NULL][]

  return(d[])
}


#' Find similar codes for a chosen period to avoid counting similar injury more than once.
#' Column `dup` with value 1 represents possibility of similar injury. The rows should
#' be carefully evaluated or deleted.
#' @param d Dataset
#' @param id Unque id
#' @param col Column name for codes selection
#' @param cond Condition for selection eg. 3 or 5 days
#' @examples
#' dj <- check_codes(dx, "lopenr", "hoveddiagnoser", 3)
check_codes <- function(d, id, col, cond){
  d[, dx := data.table::shift(x), by = y, env = list(x = col, y = id)]
  d[, dup := data.table::fifelse(days <= dag & x == dx, 1, 0), by = y,
    env = list(dag = cond, x = col, y = id)]

  d[, dx := NULL]
}

#OBS!! What happen when hoveddiagnoser has more than one codes??
# - All codes must exist in the following episode?
# - At least one exists in the following episode?
# Current solution requires the previous and following must all be equal
