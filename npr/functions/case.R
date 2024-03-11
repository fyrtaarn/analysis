#' Identify cases from FMDS dataset
#' @param d1 Dataset for FMDS ie. external explanation for injury
#' @param d2 Dataset for NPR ie. intery registration for injury
#' @param id Unique patient identity
#' @param skade Coloumn name for date of injury
#' @param rhf Coloumn name for health institutions
#' @param suffix Surfix to be added to colum names for filtering eg. xx.var1
#' @param filter Column name for filtering inclusion and exclusion ie. `is.na(filter)`
#' @param days A selected time period to consider as similar injury eg. 3 days
#' @param verbose Show the steps
#' @return A dataset with a DELXX column to indicate non-cases with value 1
find_case <- function(d1, d2, id, skade, rhf, suffix = 1, filter = NULL, days = 3, verbose = FALSE ){

  d <- is_dup_rhf(d = d1, id = id, skade = skade, rhf = rhf, suffix = suffix)
  d <- is_rhf(d1 = d, d2 = d2, id = id, skade = skade, rhf = rhf, filter = filter, days = days)

  d[, DELXX := NA_integer_]

  delCols <- grep("^xx.del", names(d), value = TRUE)

  for (i in delCols){
    d[, DELXX := data.table::fifelse(is.na(DELXX), i, DELXX), env = list(i = i)]
  }

  if (!verbose){
    xxCols <- grep("^xx.*", names(d), value = TRUE)
    d[, (xxCols) := NULL]
  }

  return(d)
}


#' Identify cases with similar date of injury and if found,
#' check if they are registered from the same health institutions.
#' If they are_same keep only the first entry.
#' @param d Dataset
#' @param id Unique patient identity
#' @param skade Coloumn name for date of injury
#' @param rhf Coloumn name for health institutions
#' @param suffix Surfix to be added to colum names for filtering eg. xx.var1
is_dup_rhf <- function(d, id, skade, rhf, suffix = 1){

  d <- data.table::copy(d)

  data.table::setkeyv(d, c(id, skade, "skadeTid" ))
  xdate <- paste0("xx.date", suffix)
  d[, (xdate) := .N, by = c(id, skade)]

  xrhf <- paste0("xx.rhf", suffix)
  d[xdate > 1, (xrhf) := .N, by = .(id, rhf),
    env = list(xdate = xdate, id = id, rhf = rhf)]

  xdel <- paste0("xx.del", suffix)
  dupRows <- duplicated(d, by = c(id, skade, rhf))
  d[dupRows, (xdel) := 1][]
}


#' Identify cases with similar date of injury but has different health institutions.
#' Need to check with injury register to identify which health institutions
#' the patients were registered to in NPR. If the health institution in FMDS isn't
#' similar to those registered in NPR (entry point) within as selected time period
#' then it's considered double registery. Double registery is marked with 1 in column "xx.del"
#' @param d1 Dataset for FMDS ie. external explanation for injury
#' @param d2 Dataset for NPR ie. intery registration for injury
#' @param id Unique patient identity
#' @param skade Coloumn name for date of injury
#' @param rhf Coloumn name for health institutions
#' @param filter Column name for filtering inclusion and exclusion ie. `is.na(filter)`
#' @param days A selected time period to consider as similar injury eg. 3 days
is_rhf <- function(d1, d2, id, skade, rhf, filter = NULL , days = 3){

  if (is.null(filter))
    filter <- grep("xx.del", names(d1), value = TRUE)

  data.table::setkeyv(d2, c(id, "innDato"))

  dx <- d1[!is.na(filter), env = list(filter = filter)]
  d <- d1[is.na(filter), env = list(filter = filter)]

  sufx <- as.integer(gsub("\\D", "", filter)) + 1
  d <- is_dup_rhf(d, id, skade = skade, rhf = rhf, suffix = sufx)
  ## delCol <- paste0("xx.del", sufx)
  ## d[, (delCol) := NULL ]

  date2 <- paste0("xx.date", sufx)
  xDato <- d[!duplicated(id) & date2 > 1,
             .(dateFrom = skade, dateTo = data.table::as.IDate(skade) + days),
             by = id, env = list(id = id, date2 = date2, skade = skade, days = days)]

  cols <- c(id, rhf)
  vecRFH <- vector(mode = "list", length = nrow(xDato))

  for (i in seq_len(nrow(xDato))){

    idx <- xDato[[id]][i]
    dateFrom <- xDato[["dateFrom"]][i]
    dateTo <- xDato[["dateTo"]][i]

    x <- d2[lopenr == idx & innDato %between% c(dateFrom, dateTo),
            .(id, rhf), env = list(id = id, rhf = rhf)]

    vecRFH[[i]] <- x
  }

  dtRHF <- data.table::rbindlist(vecRFH)

  colDate <- c("dateFrom", rhf)
  dtRHF[xDato, on = id, (colDate) := mget(colDate), env = list(id = id)]

  sufDel <- paste0("xx.del", sufx)
  idVec <- unique(dtRHF[[id]])
  for (i in seq_len(length(idVec))){
    ddx <- dtRHF[[id]][i]
    datoF <- dtRHF[["dateFrom"]][i]
    rhfx <- dtRHF[id == ddx, env = list(id = id, ddx = ddx)][[rhf]]

    d[id == ddx & skade == datoF & !(rhf %in% rhfx), (sufDel) := 1,
      env = list(id = id, ddx = ddx, skade = skade, datoF = datoF, rhf = rhf, rhfx = rhfx)]
  }

  DT <- data.table::rbindlist(list(dx, d), fill = TRUE)
  data.table::setkeyv(DT, c(id, skade, "skadeTid"))
  return(DT)
}
