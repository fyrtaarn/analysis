
# Somatic
# --------------
## som <- fread("Data/02_extracted/23_31310_som_til_utlevering.csv")
## fst::write_fst(som, "./Data/som2023des.fst")
dt1 <- fst::read_fst("./Data/som2023des.fst")
setDT(dt1)
str(dt1)
names(dt1)

# Sort
dt1 <- dt1[order(innDato, lopenr)]
dt1[, lnr := 1:.N]
dt1

dt1[is.na(lopenr), .N] #ie. ugylding fødselsnummer
dt1[is.na(lopenr), ][1:5]

# Fødselsnummer
dt1[, .N, by = fodsNr_Gyldig] #10164 - ugyldig
dt1[!duplicated(lopenr), .N, by = fodsNr_Gyldig] #607295 og ikke 607296 som i brevet

dt1[fodsNr_Gyldig == 0,][sample(.N, 20)]

# Episoder
opp <- dt1[, .(V1 =.N), by = lopenr][order(V1)]
opp[V1 == 10]

dt1[lopenr == 876389]
dt1[lopenr == 290124] #4 episoder
dt1[lopenr == 679011] #ingen ny episode

dt1[lopenr == 998945] #10

dt1[lopenr == 128429] #15
dt1[lopenr == 33597] #15
dt1[lopenr == 1152734] #15 med flere hoveddiagnoser og bidagnoser per kolonne

dt1[lopenr == 806595][order(innDato)] #20 - Forgifning
dt1[lopenr == 267482]

dt1[lopenr == 40864] #40

# Hoved- og bidiagnoser
nr <- c("hovednr", "bidianr")

dt1[, hovednr := length(unlist(strsplit(hoveddiagnoser, " "))), by = lnr]
dt1[, bidianr := length(unlist(strsplit(bidiagnoser, " "))), by = lnr]
dt1[hovednr > 1 ]
dt1[bidianr > 1 ]

homax <- paste0("hovdiag", 1:max(dt1$hovednr))
dt1[, (homax) := tstrsplit(hoveddiagnoser, " ")]

bimax <- paste0("bidiag", 1:max(dt1$bidianr))
dt1[, (bimax) := tstrsplit(bidiagnoser, " ")]


# Demografisk
dt1[!duplicated(lopenr), .N, by = kjonn]

# Hastegrad 0=ikke akutt 1=akutt
dt1[!duplicated(lopenr), .N, by = Hastegrad]

# Ut tilstand 0=død 1=levende
dt1[!duplicated(lopenr), .N, by = utTilstand]

# Ny tilstand
dt1[!duplicated(lopenr), .N, by = nyTilstand]
dt1[fodsNr_Gyldig == 1, .N, by = nyTilstand]

# Kontakt Type
dt1[!duplicated(lopenr), .N, by = kontaktType]

# Omsorgsnivå
dt1[!duplicated(lopenr), .N, by = omsorgsniva]

# Leggetid - bruk innDato og utDato
dt1[, liggetid := as.numeric(difftime(utDato, innDato, units = "days"))]
summary(dt1)
dt1[liggetid == 659.0, ]


