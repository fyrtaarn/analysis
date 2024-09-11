## Use test data
rm(list = ls())

## source("https://raw.githubusercontent.com/helseprofil/misc/main/utils.R")

pkg <- c("pacman","data.table", "fst", "lubridate", "ggplot2", "plotly", "S7", "stringi")
pacman::p_load(char = pkg)
## kh_load(char = pkg)

root <- "~/Git-fhi/analysis/npr"
fx <- list.files(file.path(root, "functions"))
for (i in fx)
  source(file.path(root, "functions", i))

fmd1 <- fread("C:\\Users\\ybka\\Git-fhi\\data\\test-fmds.csv") #22222 not found in somatikk
smt1 <- fread("C:\\Users\\ybka\\Git-fhi\\data\\test-somatik.csv") #118888 not found in FMDS

fmd1
setkey(fmd1, lopenr, skadeDato, skadeTid)

# fmd1[33, helseforetak_nr := 945578564] #make it same date with 3 different times

# Same Dates and Institution ---------------
d1 <- copy(fmd1)
d2 <- copy(smt1)

keyFMDS <- c("lopenr", "skadeDato")
keySOM <- c("lopenr", "innDato")

data.table::setkeyv(d1, keyFMDS )
data.table::setkeyv(d2, keySOM)

## Identify patients in FMDS but not in somatic. These patients will be kept and joined later
idfm <- d1[!duplicated(lopenr)][["lopenr"]]
idsm <- d2[!duplicated(lopenr)][["lopenr"]]

fmx <- setdiff(idfm, idsm)
fmdx <- d1[lopenr %in% fmx]

## Identify patients found in both but has different institution





lnr <- "lineNo"
# Needs linenumber to select cases
if (!any(names(d1) == lnr)){
  d1[, (lnr) := 1:.N]
}

# Identify FMDS patients that aren't in somatic dataset
idSom <- d2[!duplicated(lopenr)][["lopenr"]]
d1[!(lopenr %in% idSom), xx.fmds := 1L]
dx <- d1[xx.fmds == 1L]
d1 <- d1[is.na(xx.fmds)]

selx <- d2[d1, nomatch = 0][helseforetak_nr == i.helseforetak_nr][[lnr]]
d1[!lnr %in% selx, xx.DEL0 := 1L, by = lnr, env = list(lnr = lnr)]

d1 <- data.table::rbindlist(list(d1, dx), fill = TRUE)
d1[, (lnr) := NULL]
data.table::setkeyv(d1, keyFMDS)




# Round 1 -------
# Condition: Duplicated SkadeDato and Institution
# Solution: Keep earliest time only

# Row index duplicated skadeDato per lopenr
fmd1[, .I][duplicated(fmd1, by = c("lopenr", "skadeDato")) | duplicated(fmd1, by = c("lopenr", "skadeDato"), fromLast = TRUE)]

# fmd1[, x.id := .N, keyby = .(lopenr)] # select only those with one episode/lopenr ie. x.id == 1
fmd1[, x.date1 := .N, keyby = .(lopenr, skadeDato)] # select x.date == 1

# if x.rhf == 2 ie. same RHF, pick recent skadeTid
# if x.rhf == 1 ie. different RHF, check with somatikk data for which rhf is in FMDS ie. delete that isn't
fmd1[x.date1 > 1, x.rhf1 := .N, by = .(lopenr, helseforetak_nr)]
fmd1[x.date1 > 1, x.rhf11 := .N, by = .(lopenr, helseforetak_nr, skadeDato)]

# Keep only the first skadeTid when similar date and institution
## dxDato <- unique(fmd1, by = c("lopenr", "skadeDato", "helseforetak_nr"))
## dupRows <- fmd1[, .I][duplicated(fmd1, by = c("lopenr", "skadeDato", "helseforetak_nr"))]
dupRows <- which(duplicated(fmd1, by = c("lopenr", "skadeDato", "helseforetak_nr")))
fmd1[dupRows, x.del1 := 1]

# x.del1 == 1 to be deleted. Indicates similar helseforetak and date but at later time


# Round 2 ------
## delCol <- grep("^x.*", names(fmd1), value = TRUE)
## dxDato[, (delCol) := NULL]

fmd1[is.na(x.del1), x.date2 := .N, by = .(lopenr, skadeDato)] # select x.date == 1

# if x.rhf == 2 ie. same RHF, pick recent skadeTid
# if x.rhf == 1 ie. different RHF, check with somatikk data for which rhf is in FMDS ie. delete that isn't
fmd1[is.na(x.del1) & x.date2 > 1, x.rhf2 := .N, by = .(lopenr, helseforetak_nr)]
fmd1[is.na(x.del1) & x.date2 > 1, x.rhf22 := .N, by = .(lopenr, helseforetak_nr, skadeDato)]

# patient 1198870 have 4 episodes with same date and 3 different RHFs
# Need to delete the double and check for double RHF again

# Make interval eg. 3 days after SkadeDato
# This is to match with the register date is within the interval
xDato <- fmd1[is.na(x.del1) & !duplicated(lopenr) & x.date2 > 1,
             .(dateFrom = skadeDato, dateTo = as.IDate(skadeDato) + 3), by = lopenr]

cols <- c("lopenr", "helseforestak_nr")
rhf <- vector(mode = "list", length = nrow(xDato))

for (i in seq_len(nrow(xDato))){
  id <- xDato$lopenr[i]
  dateFrom <- xDato$dateFrom[i]
  dateTo <- xDato$dateTo[i]
  x <- smt1[lopenr == id & innDato %between% c(dateFrom, dateTo), .(lopenr, helseforetak_nr)]
  rhf[[i]] <- x
}

rhfDT <- rbindlist(rhf)
xDato[rhfDT, on = "lopenr"]

xh <- copy(rhfDT)
coldate <- c("dateFrom", "helseforetak_nr")
xh[xDato, on = "lopenr", (coldate) := mget(coldate)]
## xh[xDato, on = "lopenr", (coldate) := coldate, env = list(coldate = as.list(coldate) )]

# Institution that aren't found in somatik dataset within the time interval will be deleted
idVec <- unique(xh$lopenr)
for (i in seq_len(length(idVec))){
  id <- xh$lopenr[i]
  sDato <- xh$dateFrom[i]
  helseRHF <- xh[lopenr == id]$helseforetak_nr

  fmd1[lopenr == id & skadeDato == sDato & !helseforetak_nr %in% helseRHF, x.del2 := 1] # row to delete
}

## id <- xh$lopenr[2]
## sDato <- xh$dateFrom[2]
## helseRHF <- xh[lopenr == id]$helseforetak_nr

## ## fmd1[lopenr == id & skadeDato == sDato & helseforetak_nr == helseRHF, x.del2 := 0] # row to keep
## fmd1[lopenr == id & skadeDato == sDato & !helseforetak_nr %in% helseRHF, x.del2 := 1] # row to delete
## fmd1

## ## Create dummy case for testing --
## xx1 <- copy(xDato)
## xx1[, skadeDato := skadeDato + 7]
## xDt <- rbindlist(list(xDato, xx1))
## ## ---

# Round 3 -----------
## similar dates only
dx2 <- fmd1[lopenr == 4168]

smt1[dx2, on = c(lopenr = "lopenr", innDato = "skadeDato"), nomatch = 0][helseforetak_nr == i.helseforetak_nr]

setkeyv(dx2, c("lopenr", "skadeDato"))
setkeyv(smt1, c("lopenr", "innDato"))
smt1[dx2, nomatch = 0]

## Testing similar RHF
dx1 <- smt1[lopenr == 4168]
dxx <- rbindlist(list(dx1, dx1))
dxx[2, helseforetak_nr := 970188223]

setkeyv(dx2, c("lopenr", "skadeDato"))
setkeyv(dxx, c("lopenr", "innDato"))
dx2[, lnr := 1:.N]
lineNr <- dxx[dx2, nomatch = 0][helseforetak_nr == i.helseforetak_nr][["lnr"]]
dx2[lnr %in% lineNr, XX := 1L]




## FMDS patients that aren't in somatics
idSom <- smt1[!duplicated(lopenr)][["lopenr"]]
dt2[!(lopenr %in% idSom), fmd1s := 1L]
dt2[fmd1s == 1, .N]


## Checking patient with same RHF and skadeDato using find_case() vs. merging 3 cols
## which are lopenr, helseforetak_nr, skadeDato
d3 <- find_case(fmd1s22, som22, days = 3, verbose = TRUE)
d3[!is.na(lopenr) & is.na(DELXX), .N]

d3[, dds := paste(lopenr, helseforetak_nr, skadeDato, sep = "-")]
d3[duplicated(dds) | duplicated(dds, fromLast = T), xx.dds := 1L, by = lopenr]

d3[!is.na(lopenr) & xx.dds == 1,]

xxcols <- grep("^xx.*", names(d3), value = TRUE)
cols <- c("lopenr", "helseforetak_nr", "helseforetak_Navn", "skadeDato", "skadeTid", xxcols)
d3[lopenr %in% c(212, 581, 3240), ..cols]

lop3 <- d3[!is.na(lopenr) & xx.date3 == 3]$lopenr
d3[lopenr %in% sample(lop3, 5), ..cols]


# Er skadeDato like? -----------
dt2[, sameDate := fifelse(skadeDato == shift(skadeDato, type = "lag"), 1, 0), by = lopenr]

dt2[, .N, by = sameDate]
dt2[sameDate == 1, .N, by = lopenr]

dt2[lopenr == 1198870] #alle like dato
dt2[lopenr == 11947] #like skadeTid
