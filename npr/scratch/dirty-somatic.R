
# Somatic
# --------------
## som <- fread("Data/02_extracted/23_31310_som_til_utlevering.csv")
som <- fread("./Data/02_extracted/NPR20240711/24_01903_som_til_utlevering.csv", encoding = "Latin-1")
# fst::write_fst(som, "./Data/som20240711.fst")
DT1 <- fst::read_fst("./Data/som20240711.fst", as.data.table = TRUE)
DT1[, behandlingsstedNavn_alternativ := do_encode(behandlingsstedNavn_alternativ)]
## str(DT1)
## names(DT1)

## Year for filter
DT1[, yr := year(innDato)]

DT1 <-DT1[yr == 2024]

# Duplikater ---------
dim(DT1)
DT1[duplicated(DT1), .N]
dt1 <- DT1[!duplicated(DT1)]
dim(dt1)

# Sort and line  number
# dt1 <- dt1[order(innDato, lopenr)]
setkey(dt1, lopenr, innDato)
dt1[, lnr := 1:.N] # needed when identifying hoveddiagnose
dt1

ddup <- DT1[duplicated(DT1) | duplicated(DT1, fromLast = TRUE)]
dim(ddup)
id01 <- ddup[!duplicated(lopenr)][1:4]$lopenr
ddup[lopenr %in% id01]


# Sort and line  number
# dt1 <- dt1[order(innDato, lopenr)]
setkey(dt1, lopenr, innDato)
dt1[, lnr := 1:.N]
dt1

# Create a dummy var for merging
dt1[, mergeVar := innDato]

dt1[is.na(lopenr), .N] #ie. ugylding fødselsnummer
dt1[is.na(lopenr), ][1:5]

## alternative løpenummer ----------------------
dt1[is.na(alt_lopenr), .N]
dt1[is.na(alt_lopenr), ][1:5]
dt1[!is.na(alt_lopenr) & is.na(lopenr), ][1:5]

lpnx <- dt1[!is.na(alt_lopenr)]$alt_lopenr
lpn1 <- as.integer(gsub("^UNPR", "", lpnx))


# Fødselsnummer
dt1[, .N, by = fodsNr_Gyldig] #10164 - ugyldig
dt1[!duplicated(lopenr), .N, by = fodsNr_Gyldig] #607295 og ikke 607296 som i brevet

dt1[fodsNr_Gyldig == 0,][sample(.N, 20)]

# ----------------------------------------------
# Antall episoder
# ----------------------------------------------
opp <- dt1[, .(count =.N), by = lopenr][order(count)]
opp

( e3 <- opp[count == 3][sample(.N, 2)] )
dt1[lopenr == e3[1, lopenr]]
dt1[lopenr == e3[2, lopenr]]

## dt1[lopenr == 876389] #3 - 1 episode pga. Hastegrad 1
## dt1[lopenr == 290124] #4 - 2 episoder pga. Hastegrad 1
## dt1[lopenr == 679011] #ingen ny episode

( e10 <- opp[count == 10][sample(.N, 2)] )
dt1[lopenr == e10[1, lopenr]]
dt1[lopenr == e10[2, lopenr]]

dt1[lopenr == 868401] #10 - 3 hastegrad 1 samme hoveddiagnose

## dt1[lopenr == 998945] #10 - 1 episoder
## dt1[lopenr == 577056] #10 - 3 episoder?
## dt1[lopenr == 485540] #10 - 3 episoder?
## dt2[lopenr == 485540] #10 - 3 episoder?


## dt1[lopenr == 128429] #15
## dt1[lopenr == 33597] #15
## dt1[lopenr == 1152734] #15 med flere hoveddiagnoser og bidagnoser per kolonne

## dt1[lopenr == 806595][order(innDato)] #20 - Forgifning
## dt1[lopenr == 267482]

## dt1[lopenr == 40864] #40

## #98
## dt1[lopenr == 804380][Hastegrad == 1]

# Create group by lopenr
## dt1[, grp := .GRP, by = lopenr]
dd <- opp[dt1, on = .(lopenr)]

dd[lopenr == 804676]
dd[lopenr == 778520]
dd[lopenr == 929016]


# ICD-10 --------------
codes <- readRDS("./Data/validCodes.RDS")

# Identify at least one of the hoveddiagnose codes is within S00-T78
codeTXT <- paste0("^", paste(codes, collapse = "|"))
dt1[is.na(hoveddiagnoser), case := NA]
dt1[, case := sum(grepl(codeTXT, hoveddiagnoser)) > 0, by = lnr]
# dt1[, case := sum(grepl(paste0("^", paste(codes, collapse = "|")), hoveddiagnoser)) > 0]

## Double check that "case" column is TRUE -----
# Når diagnoser inneholder flere koder
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
  if(is.character(dt1[[j]]))
    set(dt1, j = j, value = substr(dt1[[j]], 1, 3) %chin% codes)
}

dt1[ , case2 := rowSums(.SD) > 0, .SDcols = homax]
dt1
dt1[case != case2]

bimax <- paste0("bidiag", 1:max(dt1$bidianr))
dt1[, (bimax) := tstrsplit(bidiagnoser, " ")]


tt4 <- "S060 T9010 S018 S023 S0290 T801 T780 T802"
tt3 <- "S06 T90 S01 S02 S02 T80 T78 T80"
unlist(strsplit(tt4, " "))
## sum(grepl("^[^T79|T80|T90]", unlist(strsplit(tt4, " "))))
sum(grepl("^T79|T80|T90", unlist(strsplit(tt4, " "))))
sum(grepl(codeTXT, unlist(strsplit(tt4, " ")))) # bruk denne løsningen


unlist(strsplit(tt3, " ")) %chin% codes
sum(unlist(strsplit(tt3, " ")) %chin% codes) > 0

substr(unlist(strsplit(tt4, " ")), 1, 3)
unlist(substr(unlist(strsplit(tt4, " ")), 1, 3)) %chin% codes
sum(unlist(strsplit(tt4, " ")) %chin% codes) > 0


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
