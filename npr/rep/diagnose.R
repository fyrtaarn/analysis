
icdAll <- fyr::get_valid_codes(DT, keep = TRUE)
icd <- fyr::get_valid_codes(dt, keep = TRUE)

icdAll[, icd := fcase(icd_1 %chin% paste0("S0", 0:9), 0,
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

icdOut <- lapply(list(icdAll,icd), show_pro, "icd", code = icdkode, digits = 2)
