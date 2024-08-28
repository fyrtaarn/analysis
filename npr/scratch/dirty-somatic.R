
# Somatic
# --------------
som <- fread("Data/02_extracted/NPR20231213/23_31310_som_til_utlevering.csv")
## som <- fread("Data/02_extracted/20240422/24_01903_som_til_utlevering.csv", encoding = "Latin-1")
fst::write_fst(som, "./Data/som2023.fst")
DT1 <- fst::read_fst("./Data/som203.fst", as.data.table = TRUE)
DT1[, behandlingsstedNavn_alternativ := do_encode(behandlingsstedNavn_alternativ)]
## str(DT1)
## names(DT1)

# Duplikater ---------
dim(DT1)
DT1[duplicated(DT1), .N]
dt1 <- DT1[!duplicated(DT1)]
dim(dt1)

# Sort and line  number
# dt1 <- dt1[order(innDato, lopenr)]
setkey(dt1, lopenr, innDato)
dt1[, lnr := 1:.N]
dt1

dt1[lopenr ==876635]
DT1[lopenr ==876635]

ddup <- DT1[duplicated(DT1) | duplicated(DT1, fromLast = TRUE)]
dim(ddup)
ddup[lopenr == 94430]
ddup[lopenr ==876635]


# Sort and line  number
# dt1 <- dt1[order(innDato, lopenr)]
setkey(dt1, lopenr, innDato)
dt1[, lnr := 1:.N]
dt1

# Create a dummy var for merging
dt1[, mergeVar := innDato]

dt1[is.na(lopenr), .N] #ie. ugylding fødselsnummer
dt1[is.na(lopenr), ][1:5]


# Fødselsnummer
dt1[, .N, by = fodsNr_Gyldig] #10164 - ugyldig
dt1[!duplicated(lopenr), .N, by = fodsNr_Gyldig] #607295 og ikke 607296 som i brevet

dt1[fodsNr_Gyldig == 0,][sample(.N, 20)]


# Episoder
opp <- dt1[, .(count =.N), by = lopenr][order(count)]
opp

dt1[lopenr == 876389] #3 - 1 episode pga. Hastegrad 1
dt1[lopenr == 290124] #4 - 2 episoder pga. Hastegrad 1
dt1[lopenr == 679011] #ingen ny episode

opp[count == 10]
dt1[lopenr == 998945] #10 - 1 episoder
dt1[lopenr == 577056] #10 - 3 episoder?
dt1[lopenr == 485540] #10 - 3 episoder?
dt2[lopenr == 485540] #10 - 3 episoder?


dt1[lopenr == 128429] #15
dt1[lopenr == 33597] #15
dt1[lopenr == 1152734] #15 med flere hoveddiagnoser og bidagnoser per kolonne

dt1[lopenr == 806595][order(innDato)] #20 - Forgifning
dt1[lopenr == 267482]

dt1[lopenr == 40864] #40

#98
dt1[lopenr == 804380][Hastegrad == 1]

# Create group by lopenr
## dt1[, grp := .GRP, by = lopenr]
dd <- opp[dt1, on = .(lopenr)]

dd[lopenr == 804676]
dd[lopenr == 778520]
dd[lopenr == 929016]

# Diagnose S00-T78, exclude others
icd10 <- fread("https://raw.githubusercontent.com/k4m1113/ICD-10-CSV/master/codes.csv")
saveRDS(icd10, "ICD10codes.RDS")

## injuryCodes <- icd10[count %like% "^S" | count %like% "^T"][, Codes := substr(count, 1, 3)]
injuryCodes <- icd10[V1 %like% "^S" | V1 %like% "^T"][, Codes := substr(V1, 1, 3)][]
xcode <- paste0("T", c(79, 80:88, 90:98)) #Codes to exclude
injuryCodes[, fmds := grepl(paste0("^(", paste(xcode, collapse = "|"),").*$"), Codes)]
codes <- unique(injuryCodes[fmds == 0, Codes])
saveRDS(codes, "validCodes.RDS")
injuryCodes[Codes %like% "^T76", .(Codes, fmds)]
injuryCodes[Codes %like% "^T79", .(Codes, fmds)]

codeTXT <- paste0("^", paste(codes, collapse = "|"))
dt1[is.na(hoveddiagnoser), case := NA]
dt1[, case := sum(grepl(paste0("^", paste(codes, collapse = "|")), hoveddiagnoser)) > 0, by = lnr]

# Hoved- og bidiagnoser, flere koder skal deles til egne kolonner
## nr <- c("hovednr", "bidianr")
# Telle diagnoser
dt1[, hovednr := length(unlist(strsplit(hoveddiagnoser, " "))), by = lnr]
dt1[, bidianr := length(unlist(strsplit(bidiagnoser, " "))), by = lnr]
dt1[hovednr > 1 ]
dt1[bidianr > 1 ]

homax <- paste0("hovdiag", 1:max(dt1$hovednr))
dt1[, (homax) := tstrsplit(hoveddiagnoser, " ")]

for (j in homax){
  if(class(dt1[[j]]) == 'character')
    set(dt1, j = j, value = substr(dt1[[j]], 1, 3) %chin% codes)
}

dt1[ , case2 := rowSums(.SD) > 0, .SDcols = homax]
dt1
dt1[case != case2]

bimax <- paste0("bidiag", 1:max(dt1$bidianr))
dt1[, (bimax) := tstrsplit(bidiagnoser, " ")]


tt <- "S060 S018 S023 S0290 T90 T80"
tt <- "S06 S01 S02 S02 T90 T80 T78"
unlist(strsplit(tt, " "))
sum(grepl("^[^T79|T80|T90]", unlist(strsplit(tt, " "))))

sum(unlist(strsplit(tt, " ")) %chin% paste0("^", codes)) > 0


# Demografisk
dt1[!duplicated(lopenr), .N, by = kjonn]

# ---------------------------------------------------
# Hastegrad 0=ikke akutt 1=akutt
dt1[!duplicated(lopenr), .N, by = Hastegrad]
dt1[!duplicated(lopenr) & is.na(Hastegrad)]

## Select only acute patient ie. Hastegrad = 1
hast <- dt1[Hastegrad == 1, ][hovdiag == 1]
hnr <- hast[, .(count =.N), by = lopenr][order(count)]

hnr[count == 10]
hast[lopenr == 671564]

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

# ----------------------------------------------
# Find connection with FMDS

dt22 <- dt1[lubridate::year(innDato) == 2022, ]

xCol <- c("kjonn", "Folkeregisterkommune", "Pasientens_Bostedsfylke", "fodsNr_Gyldig",
          "helseregion", "behandlingsstedKode", "nyTilstand", "omsorgsniva", "kontaktType", "utTilstand")
dt22[, (xCol) := NULL]

ids <- c(70883, 552297, 748375, 38367)
dtc <- find_episode(dt22[lopenr %in% ids], acute = TRUE, days = 2)

# Pick lopenr and date that duplicated by definition ie. days
dtc[, lead := shift(dup, fill = NA, type = "lead")]
dtc[is.na(dup) & lead == 1, xx.check := 1L]
xDate <- dtc[xx.check == 1, .(dateFrom = innDato - 3, dateTo = innDato, helseforetak_nr = helseforetak_nr), by = lopenr]

xDate
id <- xDate[["lopenr"]][3]
dateFrom <- xDate[["dateFrom"]][3]
dateTo <- xDate[["dateTo"]][3]

dt2[lopenr == id & skadeDato %between% c(dateFrom, dateTo), ]
