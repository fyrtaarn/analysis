
# FMDS
# -----
# run file `setup.R` prior using this file

## fmds <- fread("./Data/02_extracted/20231213/23_31310_fmds_til_utlevering.csv", encoding = "Latin-1")
## fst::write_fst(fmds, "./Data/fmds2023des.fst")

## DT2 <- fst::read_fst("./Data/fmds2023des.fst", as.data.table = TRUE)
DT2 <- fst::read_fst("./Data/fmds2022og2023.fst", as.data.table = TRUE)
DT2[, helseforetak_Navn := do_encode(helseforetak_Navn)]

# Delete duplikater
dt2 <- unique(DT2)

setkey(dt2, lopenr, skadeDato, skadeTid)

# Identity with RHF
dt2[!is.na(lopenr), idno := paste0(lopenr, helseforetak_nr)]
