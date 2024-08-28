
## root <- "~/Git-fhi/analysis/npr"
## source(file.path(root, "setup.R"))
## source(file.path(root, "functions.R"))
## kb <- fread("./Data/Kodebok_Skader_i_Norge.csv", encoding = "Latin-1")

# FMDS
# -----
fmds <- fread("Data/02_extracted/NPR20240711/24_01903_fmds_til_utlevering.csv", encoding = "Latin-1")
fst::write_fst(fmds, "./Data/fmds20240711.fst")

DT2 <- fst::read_fst("./Data/fmds20240711.fst", as.data.table = TRUE)
DT2[, helseforetak_Navn := do_encode(helseforetak_Navn)]

# Delete duplikater
dt22 <- unique(DT2)

dt22[, yr  := year(skadeDato)]
dt2 <- dt22[yr == 2024]

# dt2[, tid := chron::times(skadeTid)]
setkey(dt2, lopenr, skadeDato, skadeTid)
dt2[, lnr := 1:.N] # linenumber
# Create a dummy var for merging
dt2[, mergeVar := skadeDato]

names(dt2)

## alternative lopenr
lpnxf <- dt2[!is.na(alt_lopenr)]$alt_lopenr
lpn2 <- as.integer(gsub("^UFMDS", "", lpnxf))

# Flere hendelse av samme ID
# 1. Hvis samme dato, velg nyeste tid
# 2. Hvis forskjellige dato innen rimmelige grense eg. 3 dager, velg nyeste dato
# 3. Hvis forskjellige helseforetak_nr, match på helseforetak_nr på somatikk data hvis det finnes
likeID <- dt2[, .(count =.N), by = lopenr][order(count)]
summary(likeID[!is.na(lopenr)]$count)
likeID[, .N, keyby = count]

# Utfordinger
# - Hvilken skal slettes? Match på helsefortak_nr fra somatikk
# - Har 2 forskjellige inst, men finnes bare en inst i somatikk
# - Lik dato men forskjellige skadetid
# - Forskjell med får dager fra somtaikk til FMDS. Kan det være forsinkelse med registrering?

cols1 <- c("lopenr", "helseforetak_nr", "innDato", "utDato", "Hastegrad", "hoveddiagnoser")
cols2 <- c("lopenr", "helseforetak_nr", "helseforetak_Navn", "alvorlighetsgrad", "skadeDato", "skadeMekanisme")

id2 <- sample(likeID[count == 2]$lopenr, 3)
dt2[lopenr == id2[1], ..cols2]
dt1[lopenr == id2[1], ..cols1]

dt2[lopenr == id2[2], ..cols2]
dt1[lopenr == id2[2], ..cols1]

id4 <- sample(likeID[count == 4]$lopenr, 3)
dt2[lopenr == id4[1], ..cols2]
dt1[lopenr == id4[1], ..cols1]

dt2[lopenr == id4[2], ..cols2]
dt1[lopenr == id4[2], ..cols1]

id6 <- sample(likeID[count == 5]$lopenr, 3)
dt2[lopenr == id6[1], ..cols2]
dt1[lopenr == id6[1], ..cols1]

dt2[lopenr == id6[2], ..cols2]
dt1[lopenr == id6[2], ..cols1]

## dt2[lopenr == 794767] #Bergen og Vestfold, men ulik skadeTid
## dt1[lopenr == 794767] #somatikk data registreres bare Vestfold

## dt2[lopenr == 271] #like dato med forskjellige skade tid
## dt1[lopenr == 271] #2 dager forskjelle i somatikk etter skadeDato i FMDS dvs. bør ikke telles som manglende FMDS

## #likeID[count == 3]
## dt2[lopenr == 50] # 3 dager forskjell fra somatikk to FMDS
## dt1[lopenr == 50]

## dt2[lopenr == 1425]
## dt1[lopenr == 1425]

## #likeID[count == 5]
## dt2[lopenr == 724355] #se somatikk data.. all har FMDS med 1 dag forskjell. En rad i FMDS må slettes dvs. lik dato
## dt1[lopenr == 724355]

## dt2[lopenr == 249769]
## dt1[lopenr == 249769]

## dt2[lopenr == 67514]

## likeID[count == 10]
## dt2[lopenr == 6769]

## likeID[count == 50]
## dt2[lopenr == 390585]
## dt1[lopenr == 390585]
## dd[lopenr == 390585]

# ----------------------------
# Function -------------
## Test data
# valgID <- c(3, 45, 724355, 11947, 271, 249769, 50, 1198870, 4168, 119898)
# 4168 - 2 different dates with 1 day different and 2 RHF
# 119898 - Trondheim Legevakt has 2 register but only 1 will be deleted.. strange

## Find duplicated skadeDato
dtx <- dt2[!is.na(lopenr)]
ddate <- dtx[duplicated(dtx, by = c("lopenr", "skadeDato"))]$lopenr

# Fra 1 terial 2024
valgID <- c(1419077,1396976,207323, #1 registering
            1351180, 1251653,  325365, 399715, #2
            1125315, 1299999,  814830, #4
            1059447, 391641, 584338) #6
valgCol <- c("lopenr", "helseforetak_nr", "skadeDato", "skadeTid")
dtDato <- dt2[lopenr %in% valgID, ..valgCol]
setkey(dtDato, lopenr, skadeDato, skadeTid)

dtDato[4:5, skadeDato := as.IDate("2024-01-16")] # 2 same skadeDato
dtDato

# Round 1 -------

# Find index for duplicated skadeDato per lopenr
dtDato[,.I][duplicated(dtDato, by = c("lopenr", "skadeDato")) | duplicated(dtDato, by = c("lopenr", "skadeDato"), fromLast = T)]

# dtDato[, x.id := .N, keyby = .(lopenr)] # select only those with one episode/lopenr ie. x.id == 1
dtDato[, x.date1 := .N, keyby = .(lopenr, skadeDato)] # select x.date == 1


# if x.rhf == 2 ie. same RHF, pick recent skadeTid
# if x.rhf == 1 ie. different RHF, check with somatikk data for which rhf is in FMDS ie. delete that isn't
dtDato[x.date1 > 1, x.rhf1 := .N, by = .(lopenr, helseforetak_nr)]
dtDato[x.date1 > 1, x.rhf11 := .N, by = .(lopenr, helseforetak_nr, skadeDato)]

# Keep only the first skadeTid when similar date
## dxDato <- unique(dtDato, by = c("lopenr", "skadeDato", "helseforetak_nr"))
(dupRows <- duplicated(dtDato, by = c("lopenr", "skadeDato", "helseforetak_nr")))
dtDato[dupRows, x.del1 := 1]
dupIdx <- dtDato[dupRows, which = TRUE] #index to delete if needed

# Round 2 ------
## delCol <- grep("^x.*", names(dtDato), value = TRUE)
## dxDato[, (delCol) := NULL]

dtDato[is.na(x.del1), x.date2 := .N, by = .(lopenr, skadeDato)] # select x.date == 1

# if x.rhf == 2 ie. same RHF, pick recent skadeTid
# if x.rhf == 1 ie. different RHF, check with somatikk data for which rhf is in FMDS ie. delete that isn't
dtDato[is.na(x.del1) & x.date2 > 1, x.rhf2 := .N, by = .(lopenr, helseforetak_nr)]
dtDato[is.na(x.del1) & x.date2 > 1, x.rhf22 := .N, by = .(lopenr, helseforetak_nr, skadeDato)]

# patient 1198870 have 4 episodes with same date and 3 different RHFs
# Need to delete the double and check for double RHF again

xDato <- dtDato[is.na(x.del1) & !duplicated(lopenr) & x.date2 > 1,
                .(dateFrom = skadeDato, dateTo = as.IDate(skadeDato) + 3), by = lopenr]

cols <- c("lopenr", "helseforestak_nr")
rhf <- vector(mode = "list", length = nrow(xDato))

for (i in seq_len(nrow(xDato))){
  id <- xDato$lopenr[i]
  dateFrom <- xDato$dateFrom[i]
  dateTo <- xDato$dateTo[i]
  x <- dt1[lopenr == id & innDato %between% c(dateFrom, dateTo), .(lopenr, helseforetak_nr)]
  rhf[[i]] <- x
}

rhfDT <- rbindlist(rhf)
xDato[rhfDT, on = "lopenr"]

xh <- copy(rhfDT)
coldate <- c("dateFrom", "helseforetak_nr")
xh[xDato, on = "lopenr", (coldate) := mget(coldate)]
## xh[xDato, on = "lopenr", (coldate) := coldate, env = list(coldate = as.list(coldate) )]


idVec <- unique(xh$lopenr)
for (i in seq_len(length(idVec))){
  id <- xh$lopenr[i]
  sDato <- xh$dateFrom[i]
  helseRHF <- xh[lopenr == id]$helseforetak_nr

  dtDato[lopenr == id & skadeDato == sDato & !helseforetak_nr %in% helseRHF, x.del2 := 1] # row to delete
}

## id <- xh$lopenr[2]
## sDato <- xh$dateFrom[2]
## helseRHF <- xh[lopenr == id]$helseforetak_nr

## ## dtDato[lopenr == id & skadeDato == sDato & helseforetak_nr == helseRHF, x.del2 := 0] # row to keep
## dtDato[lopenr == id & skadeDato == sDato & !helseforetak_nr %in% helseRHF, x.del2 := 1] # row to delete
## dtDato

## ## Create dummy case for testing --
## xx1 <- copy(xDato)
## xx1[, skadeDato := skadeDato + 7]
## xDt <- rbindlist(list(xDato, xx1))
## ## ---

# Round 3 -----------
## similar dates only
dx2 <- dtDato[lopenr == 4168]

dt1[dx2, on = c(lopenr = "lopenr", innDato = "skadeDato"), nomatch = 0][helseforetak_nr == i.helseforetak_nr]

setkeyv(dx2, c("lopenr", "skadeDato"))
setkeyv(d1, c("lopenr", "innDato"))
dt1[dx2, nomatch = 0]

## Testing similar RHF
dx1 <- dt1[lopenr == 4168]
dxx <- rbindlist(list(dx1, dx1))
dxx[2, helseforetak_nr := 970188223]

setkeyv(dx2, c("lopenr", "skadeDato"))
setkeyv(dxx, c("lopenr", "innDato"))
dx2[, lnr := 1:.N]
lineNr <- dxx[dx2, nomatch = 0][helseforetak_nr == i.helseforetak_nr][["lnr"]]
dx2[lnr %in% lineNr, XX := 1L]




## FMDS patients that aren't in somatics
idSom <- dt1[!duplicated(lopenr)][["lopenr"]]
dt2[!(lopenr %in% idSom), fmds := 1L]
dt2[fmds == 1, .N]


## Checking patient with same RHF and skadeDato using find_case() vs. merging 3 cols
## which are lopenr, helseforetak_nr, skadeDato
d3 <- find_case(fmds22, som22, days = 3, verbose = TRUE)
d3[!is.na(lopenr) & is.na(DELXX), .N]

d3[, dds := paste(lopenr, helseforetak_nr, skadeDato, sep = "-")]
d3[duplicated(dds) | duplicated(dds, fromLast = T), xx.dds := 1L, by = lopenr]

d3[!is.na(lopenr) & xx.dds == 1,]

xxcols <- grep("^xx.*", names(d3), value = TRUE)
cols <- c("lopenr", "helseforetak_nr", "helseforetak_Navn", "skadeDato", "skadeTid", xxcols)
d3[lopenr %in% c(212, 581, 3240), ..cols]

lop3 <- d3[!is.na(lopenr) & xx.date3 == 3]$lopenr
d3[lopenr %in% sample(lop3, 5), ..cols]


# Er skadeDato like? -----------
dt2[, sameDate := fifelse(skadeDato == shift(skadeDato, type = "lag"), 1, 0), by = lopenr]

dt2[, .N, by = sameDate]
dt2[sameDate == 1, .N, by = lopenr]

dt2[lopenr == 1198870] #alle like dato
dt2[lopenr == 11947] #like skadeTid

dt2[lopenr == 794767] #bergen og vestfold
dt1[lopenr == 794767]

dt2[lopenr == 1199365] #Innlandet og Lillehammer komm.
dt1[lopenr == 1199365]

dt2[lopenr == 1198144]
dt1[lopenr == 1198144]

# SkadeTid er like? ------
# Velg nyeste hvis skadeTid er like





# Er helseforetak_nr er like? ----------
             dt2[]

             # Demografisk
             dt2[!duplicated(lopenr), .N, by = kjonn]

             # Fødselsnummer
             dt2[, .N, by = fodsNr_Gyldig] #6290 - ugyldig
             dt2[!duplicated(lopenr), .N, by = fodsNr_Gyldig] #226507 og ikke 226508 som i brevet

             dt2[lopenr == 679011]
             dt2[lopenr == 998945] #finnes i somatikk men ikke fmds


             # Fødselsår
             diffAge <- dt2[!is.na(lopenr) & fodtAar_FMDS != fodtAar, .(lopenr, fodtAar_FMDS, fodtAar)]
             diffAge[, diff := abs(fodtAar_FMDS - fodtAar)][]

             dt2[lopenr == 92907] #Fødselsår fmds og kontrollert er veldig forskjell

             dt2[lopenr == 1105092]

             # Alder
             dt2[, age := lubridate::year(skadeDato) - fodtAar]
             dt2[fodtAar_FMDS > 0, ageFMDS := lubridate::year(skadeDato) - fodtAar_FMDS]

             ## renset mot folkeregister
             dtAge <- dt2[, .N, keyby = age]
             dtAge[, summary(age)]
             plot_dots(dtAge, age, N)

             dtAge[age > 100, .N]
             dtAge[age > 100,]

             dt2[age == 142]
             dt2[age == 123]
             dt2[age == 119]


             ## urenset
             dtAgeFMDS <- dt2[, .N, keyby = ageFMDS]
             dtAgeFMDS[, summary(ageFMDS)]

             plot_dots(dtAgeFMDS[ageFMDS < 2000], ageFMDS, N)

             dtAgeFMDS[ageFMDS > 100, .N]
             dtAgeFMDS[ageFMDS > 100]

             dt2[ageFMDS == 2023]
             dt2[ageFMDS == 135]
             dt2[ageFMDS == 122]
             dt2[ageFMDS == 114]
             dt2[ageFMDS == 112]

             dt2[is.na(fodtAar), .N]
             dt2[is.na(age), .N]

             dt2[is.na(fodtAar_FMDS), .N]
             dt2[is.na(ageFMDS), .N]
             dt2[fodtAar_FMDS == 0, ]

             dt2[is.na(age) & !is.na(ageFMDS)]

             # kontaktarsakSkade
             dt2[!duplicated(lopenr), .N, by = kontaktarsakSkade]

             # alvorlighetsgrad
             dt2[!duplicated(lopenr), .N, by = alvorlighetsgrad]

             # skadeSted
             dt2[!duplicated(lopenr), .N, by = skadeSted]

             # fremkomstmiddel
             dt2[!duplicated(lopenr), .N, by = fremkomstmiddel]
