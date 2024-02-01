
# Somatic
# --------------
## som <- fread("Data/02_extracted/23_31310_som_til_utlevering.csv")
## fst::write_fst(som, "./Data/som2023des.fst")
DT1 <- fst::read_fst("./Data/som2023des.fst")
setDT(DT1)

# Duplikater
dt1 <- DT1[!duplicated(DT1)]

# Sort
dt1 <- dt1[order(innDato, lopenr)]
dt1[, lnr := 1:.N]
# Create a dummy var for merging
dt1[, mergeVar := innDato]

# Hoved- og bidiagnoser, flere koder skal deles til egne kolonner
nr <- c("hovednr", "bidianr")

dt1[, hovednr := length(unlist(strsplit(hoveddiagnoser, " "))), by = lnr]
dt1[, bidianr := length(unlist(strsplit(bidiagnoser, " "))), by = lnr]

homax <- paste0("hovdiag", 1:max(dt1$hovednr))
dt1[, (homax) := tstrsplit(hoveddiagnoser, " ")]

bimax <- paste0("bidiag", 1:max(dt1$bidianr))
dt1[, (bimax) := tstrsplit(bidiagnoser, " ")]

# Leggetid - bruk innDato og utDato
dt1[, liggetid := as.numeric(difftime(utDato, innDato, units = "days"))]
