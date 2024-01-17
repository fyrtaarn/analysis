
setwd("f:/Forskningsprosjekter/PDB 3327 - Skader i Norge analy_")
source("https://raw.githubusercontent.com/helseprofil/misc/main/utils.R")

pkg <- c("pacman","data.table", "fst")
kh_load(char = pkg)


# NPR
# -----
npr <- fread("Data/utlevering/23_48146_kpr_til_utlevering.csv")
fst::write_fst(npr, "npr2023des.fst")
dt1 <- fst::read_fst("./Data/npr2023des.fst")
str(dt1)


# Somatic
# --------------
som <- fread("Data/utlevering/23_31310_som_til_utlevering.csv")
fst::write_fst(som, "som2023des.fst")
dt2 <- fst::read_fst("./Data/som2023des.fst")
str(dt2)
