
# FMDS
# -----
fmds <- fread("Data/02_extracted/23_31310_fmds_til_utlevering.csv")
fst::write_fst(fmds, "./Data/fmds2023des.fst")

dt2 <- fst::read_fst("./Data/fmds2023des.fst")
setDT(dt2)
str(dt2)
