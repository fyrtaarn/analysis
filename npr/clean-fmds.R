
# FMDS
# -----
root <- "~/Git-fhi/analysis/npr"
source(file.path(root, "setup.R"))
source(file.path(root, "./functions/utils.R"))

## fmds <- fread("Data/02_extracted/23_31310_fmds_til_utlevering.csv", encoding = "Latin-1")
## fst::write_fst(fmds, "./Data/fmds2023des.fst")

DT2 <- fst::read_fst("./Data/fmds2023des.fst", as.data.table = TRUE)
DT2[, helseforetak_Navn := do_encode(helseforetak_Navn)]

# Delete duplikater
dt2 <- unique(DT2)

dt2 <- dt2[order(lopenr, skadeDato,)] #sort
dt2[, lnr := 1:.N] # linenumber
# Create a dummy var for merging
dt2[, mergeVar := skadeDato]
dt2
