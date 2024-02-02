
# Somatic
# --------------
## som <- fread("Data/02_extracted/23_31310_som_til_utlevering.csv")
## fst::write_fst(som, "./Data/som2023des.fst")
DT1 <- fst::read_fst("./Data/som2023des.fst")
setDT(DT1)

# Delete duplikater
dt1 <- DT1[!duplicated(DT1)]

# Sort
dt1 <- dt1[order(innDato, lopenr)]
dt1[, lnr := 1:.N]
# Create a dummy var for merging
dt1[, mergeVar := innDato]

# Hoved- og bidiagnoser
## Hvilke av cases som er gylding dvs S00 til T78 som hoveddiagnose og bidiagnose
dt1 <- get_valid_codes(dt = dt1, "hoveddiagnoser", "hovdiag")
dt1 <- get_valid_codes(dt = dt1, "bidiagnoser", "bidiag", sep = " ")

# Leggetid - bruk innDato og utDato
dt1[, liggetid := as.numeric(difftime(utDato, innDato, units = "days"))]
