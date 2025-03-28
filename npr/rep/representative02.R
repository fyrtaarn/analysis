
pkgs <- c("data.table", "fst", "fyr", "ggplot2", "gt")
if(!require(pak)) install.packages("pacman")
pacman::p_load(char = pkgs)

root <- "~/Git-fhi/analysis/npr"
fx <- list.files(file.path(root, "functions"))
for (i in fx)
  source(file.path(root, "functions", i))

FMD <- data.table::fread(file.path(root, "rep/fmd23.csv"))
SOM <- data.table::fread(file.path(root, "rep/som23.csv"))

som23 <- SOM[!is.na(lopenr)]
fmd23 <- FMD[!is.na(lopenr)]

caseAll <- fyr::count_case(som23, days = 3, acute = T)
case3 <- fyr::count_case(som23[Ft_dummy_Spesialist == 1,], days = 3, acute=T)

DT <- caseAll[dup == 0]
dt <- case3[dup == 0]

## kodebok
kbb <- data.table(variabel = "kjonn", beskrivelse = c("Mann", "Kvinne"), kode = 1:2)

kjonn <- lapply(list(DT, dt), show_pro, "kjonn", kbb)
kjonn <- lapply(list(DT, dt), show_pro, "kjonn", kbb) #må kjøres 2 ganger pga. kodebok
age <- lapply(list(DT, dt), show_pro, "agegp")

# Kjønn
cols <- names(kjonn[[1]])
kjonn[[1]] <- setNames(kjonn[[1]], paste(cols, "all", sep ="_"))
kjonn[[2]] <- setNames(kjonn[[2]], paste(cols, "fyr", sep ="_"))

gender <- merge(kjonn[[1]], kjonn[[2]], by.x = "kjonn_all", by.y = "kjonn_fyr")
gender <- gender[1:2,]

rreg::regcom(gender, x = "beskrivelse_all",
             yl = "prosent_fyr", yc = "prosent_all",
             lab1 = "Fyrtrn", lab2 = "Alle",
             leg1 = "Fyrtrn", leg2 = "Alle",)

# Alder
cols <- names(age[[1]])
age[[1]] <- setNames(age[[1]], paste(cols, "all", sep ="_"))
age[[2]] <- setNames(age[[2]], paste(cols, "fyr", sep ="_"))

alder <- merge(age[[1]], age[[2]], by.x = "agegp_all", by.y = "agegp_fyr")
rowDel <- nrow(alder) - 1
alder <- alder[1:rowDel, ]
alder[, nr := 1:.N]
alder <- alder[order(nr, decreasing = T)]

rreg::regcom(alder, x = "agegp_all",
             yl = "prosent_fyr", yc = "prosent_all",
             lab1 = "Fyrtrn", lab2 = "Alle",
             leg1 = "Fyrtrn", leg2 = "Alle",
             ascending = FALSE,
             title = "Andel aldersfordelingen av skadepasienter")

# Diagnoser
source("c:/Users/ybka/Git-fhi/analysis/npr/rep/diagnose.R")

cols <- names(icdOut[[1]])
icdOut[[1]] <- setNames(icdOut[[1]], paste(cols, "all", sep ="_"))
icdOut[[2]] <- setNames(icdOut[[2]], paste(cols, "fyr", sep ="_"))

icdTbl <- merge(icdOut[[1]], icdOut[[2]], by.x = "icd_all", by.y = "icd_fyr")
rowDel <- nrow(icdTbl) - 1
icdTbl <- icdTbl[1:rowDel, ]
icdTbl[, nr := 1:.N]
icdTbl <- icdTbl[order(nr, decreasing = T)]

rreg::regcom(icdTbl, x = "beskrivelse_all",
             yl = "prosent_fyr", yc = "prosent_all",
             lab1 = "Fyrtrn", lab2 = "Alle",
             leg1 = "Fyrtrn", leg2 = "Alle",
             ascending = TRUE,
             title = "Andel diagnoserfordeling av skadepasienter")

# --------------
# population
library(PxWebApiData)
library(norgeo)
library(data.table)
url <- "https://data.ssb.no/api/v0/no/table/07459/"

komm <- norgeo::get_code("kom", 2023)
age <- c(1:150)

## code and text
pop12 <- ApiData12(url, Region = komm[["code"]], Kjonn = 1:2, Alder = age, Tid = "2023")
setDT(pop12)
## pop12[, .N, keyby = region]

pop12[Alder %chin% "105+", Alder := 105][, age := as.integer(Alder)]
pop12 <- do_agegroup(pop12, "age", c(0, 15, 25, 45, 65, 80, Inf), "agegp")
## pop12[, .N, keyby = agegp]

cols <- c("region", "Region", "kjønn", "Kjonn", "Alder", "age", "agegp", "value")
pop12 <- pop12[, ..cols]
setnames(pop12, 3, "kjonn")

# Needed to recode possible old kommune codes
kommx23 <- norgeo::track_change("k", 1990, 2023, fix = TRUE)
