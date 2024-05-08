
#FMDS
DT2 <- fst::read_fst("./Data/fmds2023des.fst", as.data.table = TRUE)
DT2[, helseforetak_Navn := do_encode(helseforetak_Navn)]
dt2 <- unique(DT2)

#Somatic
DT1 <- fst::read_fst("./Data/som2023des.fst", as.data.table = TRUE)
dt1 <- DT1[!duplicated(DT1)]
