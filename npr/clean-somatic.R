
# Somatic
# --------------
## som <- fread("Data/02_extracted/23_31310_som_til_utlevering.csv")
## fst::write_fst(som, "./Data/som2023des.fst")
dt1 <- fst::read_fst("./Data/som2023des.fst")
setDT(dt1)
str(dt1)
names(dt1)

dt1[lopenr == 876389]
dt1[lopenr == 290124] #4 episoder
dt1[lopenr == 679011] #ingen ny episode

dt1[is.na(lopenr), .N] #ie. ugylding fødselsnummer
dt1[is.na(lopenr), ][1:5]

# Fødselsnummer
dt1[, .N, by = fodsNr_Gyldig] #10164 - ugyldig
dt1[!duplicated(lopenr), .N, by = fodsNr_Gyldig] #607295 og ikke 607296 som i brevet

dt1[fodsNr_Gyldig == 0,][sample(.N, 20)]

# Demografisk
dt1[, .N, by = kjonn]

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
