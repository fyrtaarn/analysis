
# KPR - data fra 01.01.2022 - 01.10.2023
# -----
## kpr <- fread("Data/02_extracted/23_48146_kpr_til_utlevering.csv")
## fst::write_fst(kpr, "./Data/kpr2023des.fst")
dt3 <- fst::read_fst("./Data/kpr2023des.fst")
setDT(dt3)
str(dt3)

dt3[lopenr == 876389]
uniqueN(dt3$lopenr)
names(dt3)

# Demografisk
dt3[!duplicated(lopenr), .N, by = kjonn]

# Kontakttype - mangler

# Tjenestetype
dt3[!duplicated(lopenr), .N, by = tjenestetype]
