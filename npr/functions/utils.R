## Functions to use

#' Select codes from column consisting ICD-10 codes that meet the requirement for injury codes ie. between S00 to T78
#' @param d Dataset
#' @param select.col Columnames from dataset with ICD codes to be controlled for
#' @param create.col New columnames consisting logical value to indicate that at least one
#' of these codes S00 to T78 exists ie. TRUE means one or more of the codes in `select.col`
#' is between S00 to T78
#' @param split Symbols for splitting the codes when there are multiple codes in the column
#' @examples
#' d1 <- get_valid_codes(dt = dd, "hoveddiagnoser", "hovdiag")
get_valid_codes <- function(d, select.col, create.col, split = " "){

  dx <- data.table::copy(d)

  # Linenumber is needed for couting ICD codes by line
  if (!("lnr" %in% names(dx))){
    dx[, lnr := 1:.N] # linenumber
  }

  dx[, colnr := length(unlist(strsplit(x = col1, split = split))), by = lnr, env = list(col1 = select.col)]
  cols <- paste0("temp", 1:max(dx$colnr))
  dx[, (cols) := data.table::tstrsplit(x = col1, " "), env = list(col1 = select.col)]

  # Select only these codes S00 til T78
  codeURL <- "https://github.com/fyrtaarn/analysis/raw/main/data/validCodes.RDS"
  codes <- readRDS(url(codeURL))

  for (j in cols){
    if(class(dx[[j]]) == 'character')
      set(dx, j = j, value = substr(dx[[j]], 1, 3) %chin% codes)
  }

  dx[ , (create.col) := rowSums(.SD) > 0, .SDcols = cols]
  xcols <- c("colnr", cols)
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
show_pro <- function(data, var, code = NULL){
  dt <- data.table::copy(data)
  x <- dt[, .N, by = var, env = list(var = var)]
  x[, sum := sum(N, na.rm = T)][, pro := round(100 * N/sum, 1), by = var, env = list(var = var)][, sum := NULL]

  if (!is.null(code)) {
    if (!is.character(x[, var, env = list(var = var)]))
      x[, (var) := as.character(kj), env = list(kj = var)]

    data.table::setkeyv(x, var)
    data.table::setkey(code, kode)
    x[code[variabel == var], beskrivelse := beskrivelse]
  } else {
    data.table::setkeyv(x, var)
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
