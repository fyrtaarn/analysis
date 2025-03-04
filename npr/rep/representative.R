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

fmd <- setDT(read_fst(file.path(root, "Data/fmds20240711.fst")))
som <- setDT(read_fst(file.path(root, "Data/som20240711.fst")))

fmd[, helseforetak_Navn := fyr:::is_encode(helseforetak_Navn)]
som[, behandlingsstedNavn_alternativ := fyr:::is_encode(behandlingsstedNavn_alternativ)]
som[, Ft_Enhet_Spesialist := fyr:::is_encode(Ft_Enhet_Spesialist)]

fmd[, let(year = year(skadeDato), month = month(skadeDato))]
som[, let(year = year(innDato), month = month(innDato))]

# Alder
fmd[, fodtMix := fifelse(is.na(fodtAar), fodtAar_FMDS, fodtAar)]
som[, age := year - fodselsar]
fmd[, age := year - fodtMix]

som <- do_agegroup(som, "age", c(0, 15, 25, 45, 65, 80, Inf), "agegp")
fmd <- do_agegroup(fmd, "age", c(0, 15, 25, 45, 65, 80, Inf), "agegp")

som[, .N, keyby = agegp]
fmd[, .N, keyby = agegp]

skimr::skim(som)
skimr::skim(fmd)

collapse::descr(som)
as.data.table(descr(som))


# Subset
fmd23 <- fmd[year == 2023]
som23 <- som[year == 2023]

caseN0 <- count_case(som23, acute=T)
caseN3 <- count_case(som23, days = 3, acute=T)

all <- caseN3[!is.na(lopenr) & dup != 1,]
fm <- caseN3[!is.na(lopenr) & dup != 1 & Ft_dummy_Spesialist == 1, ]

dim(all)
dim(fm)

show_pro(all, "kjonn")
show_pro(all,"agegp")

show_pro(fm, "kjonn")
show_pro(fm,"agegp")


## ICD10 Diagnoser
## ----------------------------
icd <- fyr::get_valid_codes(som, keep = TRUE)

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
                   icd_1 %chin% paste0("T4", c(1, "n", 0)), 15, #Forgiftning
                   icd_1 %chin% paste0("T5", 1:9), 16, #Toksiske virkninger
                   icd_1 %chin% paste0("T6", 0:5), 16, #Toksiske virkninger
                   icd_1 %chin% paste0("T6", 6:9), 17, #Annet
                   icd_1 %chin% paste0("T7", 0:8), 17 #Annet
                   )]


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

## Må være character for å kunne bruke som kodebok
icdkode[, kode := as.character(kode)]

show_pro(icd, "icd", code = icdkode)
