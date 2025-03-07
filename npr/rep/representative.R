froot <- "~/Git-fhi/analysis/npr"
fx <- list.files(file.path(froot, "functions"))
for (i in fx)
  source(file.path(froot, "functions", i))

## if(!require(pak)) install.packages("pak")
## pak::pkg_install("fyrtaarn/fyr")

library(data.table)
library(fst)
library(fyr)
library(skimr)
library(collapse)

root <- "f:/Forskningsprosjekter/PDB 3327 - Skader i Norge analy_"

fmd_raw <- setDT(read_fst(file.path(root, "Data/fmds20240711.fst")))
som_raw <- setDT(read_fst(file.path(root, "Data/som20240711.fst")))

fmd_raw[, helseforetak_Navn := fyr:::is_encode(helseforetak_Navn)]
som_raw[, behandlingsstedNavn_alternativ := fyr:::is_encode(behandlingsstedNavn_alternativ)]
som_raw[, Ft_Enhet_Spesialist := fyr:::is_encode(Ft_Enhet_Spesialist)]

# Trenger år for å velge årgang data
fmd_raw[, let(year = year(skadeDato), month = month(skadeDato))]
som_raw[, let(year = year(innDato), month = month(innDato))]

# Alder
fmd_raw[, fodtMix := fifelse(is.na(fodtAar), fodtAar_FMDS, fodtAar)]
som_raw[, age := year - fodselsar]
fmd_raw[, age := year - fodtMix]

som_raw <- do_agegroup(som_raw, "age", c(0, 15, 25, 45, 65, 80, Inf), "agegp")
fmd_raw <- do_agegroup(fmd_raw, "age", c(0, 15, 25, 45, 65, 80, Inf), "agegp")

som_raw[, .N, keyby = agegp]
fmd_raw[, .N, keyby = agegp]

skimr::skim(som_raw)
skimr::skim(fmd_raw)

collapse::descr(som_raw)
as.data.table(descr(som_raw))


# Subset 2023
# ------------
FMD23 <- fmd_raw[year == 2023]
SOM23 <- som_raw[year == 2023]

## fwrite(FMD23, "fmd23.csv")
## fwrite(SOM23, "som23.csv")

som23 <- SOM23[!is.na(lopenr)]
fmd23 <- FMD23[!is.na(lopenr)]

caseN0all <- count_case(som23, acute=T)
caseN3all <- count_case(som23, days = 3, acute=T)

caseN0 <- count_case(som23[Ft_dummy_Spesialist == 1,], acute=T)
caseN3 <- count_case(som23[Ft_dummy_Spesialist == 1,], days = 3, acute=T)

caseN3all[, .N, keyby = dup]
caseN3[, .N, keyby = dup]

som1 <- caseN3all[dup == 0]
som2 <- caseN3[dup == 0]

kbb <- data.table(variabel = "kjonn", beskrivelse = c("Mann", "Kvinne"), kode = 1:2)

lapply(list(som1, som2), show_pro, "kjonn", kbb)
lapply(list(som1, som2), show_pro, "agegp")

ageMen <- lapply(list(som1[kjonn == 1], som2[kjonn==1]), show_pro, "agegp") #mann
ageMen <- setNames(ageMen, c("Alle", "Fyrtårn"))
ageMen[[1]][, let(var = "Alle", kjonn = "Men")]
ageMen[[2]][, let(var = "Alle", kjonn = "Men")]
ageM <- rbindlist(ageMen)

ageKvn <- lapply(list(som1[kjonn == 2], som2[kjonn==2]), show_pro, "agegp") #kvinne
ageKvn <- setNames(ageKvn, c("Alle", "Fyrtårn"))
ageKvn[[1]][, let(var = "Alle", kjonn = "Kvinner")]
ageKvn[[2]][, let(var = "Alle", kjonn = "Kvinner")]
ageK <- rbindlist(ageKvn)




## ICD10 Diagnoser
## ----------------------------
somm <- caseN3[!is.na(lopenr) & dup != 1 & Ft_dummy_Spesialist == 1,]
icd <- fyr::get_valid_codes(somm, keep = TRUE)

icd[, icd := fcase(icd_1 %chin% paste0("S0", 0:9), 0,
                   icd_1 %chin% paste0("S1", 0:9), 1,
                   icd_1 %chin% paste0("S2", 0:9), 2,
                   icd_1 %chin% paste0("S3", 0:9), 3,
                   icd_1 %chin% paste0("S4", 0:9), 4,
                   icd_1 %chin% paste0("S5", 0:9), 5,
                   icd_1 %chin% paste0("S6", 0:9), 6,
                   icd_1 %chin% paste0("S7", 0:9), 7,
                   icd_1 %chin% paste0("S8", 0:9), 8,
                   icd_1 %chin% paste0("S9", 0:9), 9,
                   icd_1 %chin% paste0("T0", 0:7), 10,
                   icd_1 %chin% paste0("T0", 8:9), 11, #Uspesifisert
                   icd_1 %chin% paste0("T", 10:14), 11, #Uspesifisert
                   icd_1 %chin% paste0("T1", 5:9), 12, # Fremmedlegeme
                   icd_1 %chin% paste0("T2", 0:9), 13, #Brannskader
                   icd_1 %chin% paste0("T3", 0:2), 13, #Brannskader
                   icd_1 %chin% paste0("T3", 3:5), 14, #Frostskade
                   icd_1 %chin% paste0("T4", c(1, "n")), 15, #Forgiftning
                   icd_1 %chin% "T50", 15, #Forgiftning
                   icd_1 %chin% paste0("T5", 1:9), 16, #Toksiske virkninger
                   icd_1 %chin% paste0("T6", 0:5), 16, #Toksiske virkninger
                   icd_1 %chin% paste0("T6", 6:9), 17, #Annet
                   icd_1 %chin% paste0("T7", 0:8), 17 #Annet
                   )]

## Use icd 2 if necessary
icd[is.na(icd), icd := fcase(icd_2 %chin% paste0("S0", 0:9), 0,
                   icd_2 %chin% paste0("S1", 0:9), 1,
                   icd_2 %chin% paste0("S2", 0:9), 2,
                   icd_2 %chin% paste0("S3", 0:9), 3,
                   icd_2 %chin% paste0("S4", 0:9), 4,
                   icd_2 %chin% paste0("S5", 0:9), 5,
                   icd_2 %chin% paste0("S6", 0:9), 6,
                   icd_2 %chin% paste0("S7", 0:9), 7,
                   icd_2 %chin% paste0("S8", 0:9), 8,
                   icd_2 %chin% paste0("S9", 0:9), 9,
                   icd_2 %chin% paste0("T0", 0:7), 10,
                   icd_2 %chin% paste0("T0", 8:9), 11, #Uspesifisert
                   icd_2 %chin% paste0("T", 10:14), 11, #Uspesifisert
                   icd_2 %chin% paste0("T1", 5:9), 12, # Fremmedlegeme
                   icd_2 %chin% paste0("T2", 0:9), 13, #Brannskader
                   icd_2 %chin% paste0("T3", 0:2), 13, #Brannskader
                   icd_2 %chin% paste0("T3", 3:5), 14, #Frostskade
                   icd_2 %chin% paste0("T4", c(1, "n")), 15, #Forgiftning
                   icd_2 %chin% "T50", 15, #Forgiftning
                   icd_2 %chin% paste0("T5", 1:9), 16, #Toksiske virkninger
                   icd_2 %chin% paste0("T6", 0:5), 16, #Toksiske virkninger
                   icd_2 %chin% paste0("T6", 6:9), 17, #Annet
                   icd_2 %chin% paste0("T7", 0:8), 17 #Annet
                   )]

## Kodebok må ha disse 3 variablene
icdkode <- data.table(kode = 0:17,
                      variabel = "icd",
                      beskrivelse = c("Hode",
                                   "Hals",
                                   "Toraks",
                                   "Buk, nedre rygg",
                                   "Skulder, overarm",
                                   "Albu, underarm",
                                   "Håndledd, hånd",
                                   "Hofte, lår",
                                   "Kne, legg",
                                   "Ankel, fot",
                                   "Multiple",
                                   "Uspesifisert",
                                   "Fremmedlegeme",
                                   "Brannskader",
                                   "Forstskade",
                                   "Forgiftning",
                                   "Toksiske virkninger",
                                   "Annet"))


show_pro(icd, "icd", code = icdkode)
