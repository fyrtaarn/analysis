
# NPR
# -----
## npr <- fread("Data/02_extracted/23_48146_kpr_til_utlevering.csv")
## fst::write_fst(npr, "./Data/npr2023des.fst")
dt1 <- fst::read_fst("./Data/npr2023des.fst")
setDT(dt1)
str(dt1)

dt1[lopenr == 876389]
uniqueN(dt1$lopenr)
