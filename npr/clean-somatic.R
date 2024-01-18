
# Somatic
# --------------
## som <- fread("Data/02_extracted/23_31310_som_til_utlevering.csv")
## fst::write_fst(som, "./Data/som2023des.fst")
dt3 <- fst::read_fst("./Data/som2023des.fst")
setDT(dt3)
str(dt3)

dt3[lopenr == 876389]

dt3[is.na(lopenr), .N] #ie. ugylding fødselsnummer
dt3[is.na(lopenr), ][1:5]

# Fødselsnummer
dt3[, .N, by = fodsNr_Gyldig] #10164 - ugyldig
dt3[!duplicated(lopenr), .N, by = fodsNr_Gyldig] #607295 og ikke 607296 som i brevet

# Hastegrad 0=ikke akutt 1=akutt
dt3[!duplicated(lopenr), .N, by = Hastegrad]

# Ut tilstand 0=død 1=levende
dt3[!duplicated(lopenr), .N, by = utTilstand]

# Ny tilstand
dt3[!duplicated(lopenr), .N, by = nyTilstand]

# Kontakt Type
dt3[!duplicated(lopenr), .N, by = kontaktType]
