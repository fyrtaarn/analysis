## Functions to use

#' Select codes from column consisting ICD-10 codes that meet the requirement for injury codes ie. between S00 to T78
#' @param d Dataset
#' @param select.col Columnames from dataset with ICD codes to be controlled for. Default is `hoveddiagnoser`
#' @param create.col New columnames consisting logical value to indicate that at least one
#' of these codes S00 to T78 exists ie. TRUE means one or more of the codes in `select.col`
#' is between S00 to T78. Default is `hovdiag`
#' @param split Symbols for splitting the codes when there are multiple codes in the column
#' @return Original column will have suffix `*.old`
#' @examples
#' d1 <- get_valid_codes(dt = dd, "hoveddiagnoser", "hovdiag")
get_valid_codes <- function(d, select.col = "hoveddiagnoser", create.col = "hovdiag", split = " "){

  dx <- data.table::copy(d)

  # Linenumber is needed for counting ICD codes by line
  if (!("lnr" %in% names(dx))){
    dx[, lnr := 1:.N] # linenumber
  }

  # Column can have multiple codes. Split them so each column has single code
  dx[, colnr := length(unlist(strsplit(x = col1, split = split))), by = lnr, env = list(col1 = select.col)]
  cols <- paste0("temp", 1:max(dx$colnr))
  dx[, (cols) := data.table::tstrsplit(x = col1, " "), env = list(col1 = select.col)]

  # Trim codes to keep only the first 3 digits
  for (j in cols){
    if(is.character(dx[[j]]))
      data.table::set(dx, j = j, value = substr(dx[[j]], 1, 3))
  }

  tempCol <- paste0(select.col, "01")
  dx[, (tempCol) := do.call(paste, c(replace(.SD, is.na(.SD), ""), sep = " ")), .SDcols = cols]

  # Select only these codes S00 til T78
  codeURL <- "https://github.com/fyrtaarn/analysis/raw/main/data/validCodes.RDS"
  codes <- readRDS(url(codeURL))

  for (j in cols){
    if(class(dx[[j]]) == 'character')
      set(dx, j = j, value = dx[[j]] %chin% codes)
  }

  dx[ , (create.col) := rowSums(.SD) > 0, .SDcols = cols]

  oldCol <- paste0(select.col, ".old")
  data.table::setnames(dx, old = c(select.col, tempCol), new = c(oldCol, select.col))
  dx[, (select.col) := trimws(col, which = "right"), env = list(col = select.col)]
  dx[col == "" | is.na(old), (select.col) := NA, env = list(col = select.col, old = oldCol)]

  xcols <- c("colnr", "lnr", cols)
  dx[, (xcols) := NULL][]
}


#' Recode age into groups
#' @param d Dataset
#' @param col Column name for age
#' @param category How to categorise age into groups eg. seq(1,70, 10, Inf)
#' @param new Column name for the age group
#' @examples
#' dt2 <- do_agegroup(dt2, "age", c(0, 18, 25, 45, 65, Inf))
do_agegroup <- function(d, col, category, new = NULL){
  AGEGP <- grp <- ageGRP <- up <- lo <- NULL

  dt <- data.table::copy(d)
  if(!is(dt,"data.table"))
    data.table::setDT(dt)

  DT <- data.table::copy(dt)
  x <- DT[, .(min = min(col, na.rm = T), max = max(col, na.rm = T)), env = list(col = col)]

  KB <- data.table::data.table(AGEGP = x$min:x$max, grp = NA)
  KB[, "grp" := cut(AGEGP, breaks = category, right = FALSE)]
  KB[, "ageGRP" := sub("\\[(.*)\\)", "\\1", grp)]

  ageVars <- c("lo", "up")
  KB[, (ageVars) := data.table::tstrsplit(ageGRP, ",")]

  for (j in ageVars){
    suppressWarnings(data.table::set(KB, j = j, value = as.numeric(KB[[j]])))
  }

  KB[, "up" := up - 1]
  agp <- "alderGRP"
  KB[up != Inf, (agp) := paste0(lo, "-", up)]
  KB[up == Inf, (agp) := paste0(lo, "+")]

  delCols <- c(ageVars, "ageGRP", "grp")
  KB[, (delCols) := NULL]
  data.table::setnames(KB, agp, "GRP")

  data.table::setkeyv(DT, col)
  data.table::setkey(KB, AGEGP)

  DT[KB, GRP := GRP]

  if (!is.null(new))
    data.table::setnames(DT, "GRP", new)

  return(DT[])
}


#' Simple scatterplot
plot_dots <- function(dt, x, y){
  plotAge <- ggplot(dt, aes({{x}}, {{y}})) +
    geom_point(aes(size = {{y}})) +
    geom_point(data = dt[x > 100, env = list(x = as.character(substitute(x)))],
               colour = "red")

  ggplotly(plotAge)
}

#' Calculate frequency and percent for selected variable
#' @param data Dataset
#' @param var Selected variable name to be calculated
#' @param code Codebook to define the value in variable if needed. Default is NULL
#' @examples
#' show_pro(dx1, "hovdiag")
#' show_pro(dt2, "fremkomstmiddel", kb)
show_pro <- function(data, var, code = NULL, value = TRUE, sort = NULL){
  dt <- data.table::copy(data)
  x <- dt[, .N, by = var, env = list(var = var)]
  x[, sum := sum(N, na.rm = T)][, prosent := round(100 * N/sum, 1), by = var, env = list(var = var)][, sum := NULL]

  bes <- "beskrivelse"

  if (!is.null(code)) {
    if (!is.character(x[, var, env = list(var = var)]))
      x[, (var) := as.character(kj), env = list(kj = var)]

    data.table::setkeyv(x, var)
    data.table::setkey(code, kode)
    x[code[variabel == var], (bes) := beskrivelse]
  } else {
    data.table::setkeyv(x, var)
  }

  if (!is.null(sort))
    x <- x[order(-get(sort))]

  total <- "Total"
  tot <- x[, sum(N, na.rm = T)]
  if (ncol(x) == 3){
    tt <- list(total, tot, 100)
  } else {
    tt <- list(total, tot, 100, " ")
  }

  x <- rbindlist(list(x, tt))

  if (!value){
    x[, (var) := NULL]
    data.table::setcolorder(x, c(bes, "N", "prosent"))
    tx <- x[, .N]
    x[tx, (bes) := total]
  }

  return(x[])
}

## delidx <- dt[colName %in% selecedValues, which = TRUE]
## Ref https://github.com/Rdatatable/data.table/issues/635#issuecomment-261473829
is_delete_index <- function(dt, delidx){
  # delidx - Row index to be deleted
  keepIdx <- setdiff(dt[, .I], delidx)
  cols = names(dt)
  dtSub <- data.table::data.table(dt[[1]][keepIdx]) #subsetted table
  data.table::setnames(dtSub, cols[1])

  for (col in cols[2:length(cols)]){
    dtSub[, (col) := dt[[col]][keepIdx]]
    dt[, (col) := NULL]
  }

  return(dtSub)
}

## alternative to %ilike% in data.table, ie. ignore case
`%likeci%` <- function (x, pattern) {
  stringi::stri_detect_regex(x, pattern, case_insensitive=TRUE)
}
