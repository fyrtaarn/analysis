source("~/Git-fhi/analysis/npr/setup.R")
source("~/Git-fhi/analysis/npr/scratch/data-input.R")

## ----------------
## 2022
## ----------------
library(data.table)
## setDT(dt1) #Somatikk data konverteres til data.table class
## setDT(dt2) #FMDS data konverteres til data.table class
som22 <- dt1[!is.na(lopenr) & year(as.IDate(innDato, "%Y-%m-%d")) == 2022, ] #Velg bare 2022 data og eksludere til med missing lopenr
fmds22 <- dt2[!is.na(lopenr) & year(as.IDate(skadeDato, "%Y-%m-%d")) == 2022, ]
dim(fmds22)

# Identisk ID og skadeDato
setkey(fmds22, lopenr, skadeDato, skadeTid) #sortering
fmds22[, lnrSkade := paste(lopenr, skadeDato, sep = "-")] #kombinere id og skade dato
fmds22[duplicated(lnrSkade) | duplicated(lnrSkade, fromLast = T), x.dup := 1L] #Identifisere duplikate ie. lik ID og skadeDato
fmds22[duplicated(lnrSkade), x.del := 1L] #Beholder observasjonen med tidligst skade tidspunkt
fmds22[is.na(x.del), .N] #Teller alle med unntak de uten ID og de duplikater ID og skadeDato

cols <- c("x.dup", "x.del")
fmds22[, (cols) := NULL]
d0 <- find_case(fmds22, som22, days = 0)
d1 <- find_case(fmds22, som22, days = 1)
d2 <- find_case(fmds22, som22, days = 2)
d3 <- find_case(fmds22, som22, days = 3)
d5 <- find_case(fmds22, som22, days = 5)
d7 <- find_case(fmds22, som22, days = 7, verbose = T)

d0[is.na(DELXX), .N]
d1[is.na(DELXX), .N]
d2[is.na(DELXX), .N]
d3[is.na(DELXX), .N]
d5[is.na(DELXX), .N]
d7[is.na(DELXX), .N]


## FMDS patients that aren't in somatics
dtFMD <- copy(fmds22)
idSom <- som22[!duplicated(lopenr)][["lopenr"]]
dtFMD[!(lopenr %in% idSom), fmds := 1L]
dtFMD[fmds == 1, .N]

idFM <- dtFMD[fmds == 1]$lopenr
som22[lopenr %in% idFM, .N] #checking

# Lag dummy ID for personen med uten løpenr -----------------
fmds22[, xID := fifelse(is.na(lopenr), paste(komNrSkade, helseforetak_nr, fodtAar_FMDS, kjonn_FMDS, sep = "_"), as.character(lopenr))]
fmds22[!duplicated(xID)]
fmds22[is.na(lopenr)][duplicated(xID) | duplicated(xID, fromLast = TRUE)]
fmds22[is.na(lopenr)][duplicated(xID) | duplicated(xID, fromLast = TRUE)][duplicated(skadeDato) | duplicated(skadeDato, fromLast = TRUE)]

fmds22[!duplicated(xID), .N]

fmds22[xID == "3411_983971709_1959_1"]
fmds22[xID == "301_993467049_2000_2"]

# SkadeDato er like
fmds22[xID == "3411_983971709_1959_1"]
fmds22[xID == "301_993467049_1976_1"]
