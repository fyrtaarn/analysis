
## ----------
## kodebok
## ----------
kb <- fread("./Data/Kodebok_Skader_i_Norge.csv", encoding = "Latin-1")

kb[, .N, by = variabel]

kb[variabel == "kjonn"]
kb[variabel == "omsorgsniva"]
kb[variabel == "kontaktType"]
kb[variabel %like% "takst"]
kb[variabel == "helseregion"]
kb[variabel %like% "Mekanisme"]
kb[variabel %like% "Haste"]
kb[variabel %like% "^Org.nr"]


## -------------
## Restructuring
## -------------
install.packages("codebook")
install.packages("labelled")

library(codebook)
# new_codebook_rmd()

dtmiss <- detect_missing(dt1)
dtm1 <- detect_scales(dtmiss)

codebook(dtm1)

## Somatikk
DT1 <- copy(dt1)
koded <- c("kjonn", "omsorgsniva", "Hastegrad", "kontaktType")
codebook(DT1[, mget(koded)])


var_label(DT1$kjonn) <- "Kjønn"
kjonn <- c(Ukjent = 0, Mann = 1, Kvinne = 2, Ukjent = 9)
val_labels(DT1$kjonn) <- kjonn

var_label(DT1$omsorgsniva) <- "Omsorgnivå"
omsorg <- c("Døgnopphold" = 1,
            "Feil rapportert verdi" = -1,
            "Dagbehandling" = 2,
            "Poliklinisk konsult/kontakt" = 3,
            "Poliklinisk inneliggende pasient" = 8)
val_labels(DT1$omsorgsniva) <- omsorg


var_label(DT1$kontaktType) <- "Kontakt Type"
kontakt <- c("Utredning" = 1,
             "Feil rapportert verdi" = -1,
             "Parsientadmin behandling" = 12,
             "Opplæring" = 13,
             "Screening" = 14,
             "Behandling" = 2,
             "Kontrol" = 3,
             "Indirekte pasientkontakt" = 5,
             "Videokonsultasjon" = 6,
             "Telefonkonsult" = 7)

val_labels(DT1$kontaktType) <- kontakt

var_label(DT1$Hastegrad) <- "Hastegraden"
hast <- c("ikke akutt" = 0, "akutt" = 1)
val_labels(DT1$Hastegrad) <- hast
