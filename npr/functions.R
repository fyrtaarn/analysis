## Functions to use

#' Split columns with multiple input
#' @param dt Dataset
#' @param select.col Columnames from dataset
#' @param create.col New columnames to be created
#' @param sep Separate symbols
get_valid_codes <- function(dt, select.col, create.col, sep = " "){

  if (!("lnr" %in% names(dt))){
    dt1[, lnr := 1:.N] # linenumber
  }

  dt[, colnr := length(unlist(strsplit(col1, sep))), by = lnr, env = list(col1 = select.col)]

  cols <- paste0("temp", 1:max(dt$colnr))
  dt[, (cols) := data.table::tstrsplit(col1, " "), env = list(col1 = select.col)]

  # Only these codes S00 til T78
  codeURL <- "https://github.com/fyrtaarn/analysis/raw/main/data/validCodes.RDS"
  codes <- readRDS(url(codeURL))

  for (j in cols){
    if(class(dt[[j]]) == 'character')
      set(dt, j = j, value = substr(dt[[j]], 1, 3) %chin% codes)
  }

  dt[ , (create.col) := rowSums(.SD) > 0, .SDcols = cols]
  xcols <- c("colnr", cols)
  dt[, (xcols) := NULL][]
}


#' Recode age into groups
#' @param dt Dataset
#' @param col Column name for age
#' @param category How to categorise age into groups eg. seq(1,70, 10, Inf)
#' @param new Column name for the age group
do_agegroup <- function(dt, col, category, new = NULL){
  AGEGP <- grp <- ageGRP <- up <- lo <- NULL

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
  }

  x[order(var), env = list(var = var)][]
}
