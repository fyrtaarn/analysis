
#' @description Handling case with similar date in FMDS and Somatic data
#' @param d1 FMDS dataset
#' @param d2 Somatic dataset
is_same_dates <- function(d1, d2){

  d1 <- data.table::copy(d1)
  data.table::setkeyv(d1, c("lopenr", "skadeDato"))
  data.table::setkeyv(d2, c("lopenr", "innDato"))

  # Needs linenumber to select cases
  if (!any(names(d1) == "lnr")){
    d1[, lnr := 1:.N]
  }

  selx <- d2[d1, nomatch = 0][helseforetak_nr == i.helseforetak_nr][["lnr"]]
  d1[!( lnr %in% selx ), xx.DEL0 := 1L, by = lnr]
}
