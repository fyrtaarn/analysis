
# Somatic
# --------------
root <- "~/Git-fhi/analysis/npr"
source(file.path(root, "setup.R"))
source(file.path(root, "./functions/utils.R"))

## som <- fread("Data/02_extracted/23_31310_som_til_utlevering.csv", encoding = "Latin-1")
## fst::write_fst(som, "./Data/som2023des.fst")
DT1 <- fst::read_fst("./Data/som2023des.fst", as.data.table = TRUE)

# Delete duplikater
dt1 <- DT1[!duplicated(DT1)]

setkey(dt1, lopenr, innDato)
dt1[, lnr := 1:.N] # linenumber
# Create a dummy var for merging
dt1[, mergeVar := innDato]

# Hoved- og bidiagnoser
## Hvilke av cases som er gylding dvs S00 til T78 som hoveddiagnose og bidiagnose
dt1 <- get_valid_codes(dt = dt1, "hoveddiagnoser", "hovdiag")
dt1 <- get_valid_codes(dt = dt1, "bidiagnoser", "bidiag", sep = " ")

# Select only acute patient ie. Hastegrad = 1
dt1[Hastegrad == 1, ]

# Leggetid - bruk innDato og utDato
dt1[, liggetid := as.numeric(difftime(utDato, innDato, units = "days"))]

# Johan spørsmål om antall
# pasienter med bare en eller flere bidiagnoser S00-T78 i 2022
som2022 <- dt1[lubridate::year(innDato) == 2022, ]
som2022[hovdiag == 0 & bidiag == 1, .N]
som2022[hovdiag == 0 & bidiag == 1, ]

som2022[hovdiag == 0 & bidiag == 1, ][!duplicated(lopenr), .N]
som2022[hovdiag == 0 & bidiag == 1, ][!duplicated(lopenr), ]

som2022[hovdiag == 0 & bidiag == 1, ][!duplicated(lopenr) & Hastegrad == 1, .N]
som2022[hovdiag == 0 & bidiag == 1, ][!duplicated(lopenr) & Hastegrad == 1, ]
