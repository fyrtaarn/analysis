
setwd("f:/Forskningsprosjekter/PDB 3327 - Skader i Norge analy_")
source("https://raw.githubusercontent.com/helseprofil/misc/main/utils.R")

pkg <- c("pacman","data.table", "fst", "lubridate")
kh_load(char = pkg)


## ----------
## kodebok
## ----------
kb <- fread("./Data/Kodebok_Skader_i_Norge.csv", encoding = "Latin-1")

kb[, .N, by = variabel]

kb[variabel == "kjonn"]
kb[variabel == "omsorgsniva"]
kb[variabel == "kontaktType"]
kb[variabel %like% "takst"]
