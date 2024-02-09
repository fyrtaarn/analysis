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

