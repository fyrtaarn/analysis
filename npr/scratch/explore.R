
DT1 <- fst::read_fst("./Data/som2023des.fst", as.data.table = TRUE)
dt1 <- unique(DT1)
dt1[!is.na(lopenr), idno := paste0(lopenr, helseforetak_nr)]

DT2 <- fst::read_fst("./Data/fmds2023des.fst", as.data.table = TRUE)
DT2[, helseforetak_Navn := do_encode(helseforetak_Navn)]
dt2 <- unique(DT2)
dt2[!is.na(lopenr), idno := paste0(lopenr, helseforetak_nr)]

# 2022
fmds22all <- dt2[lubridate::year(skadeDato) == 2022, ]
som22all <- dt1[lubridate::year(innDato) == 2022, ]

som22all[!is.na(lopenr), .N]
fmds22all[!is.na(lopenr), .N]

# Fyrtårn utvalgte
## Somatisk --------
som22 <- som22all[!is.na(lopenr) & helseforetak_nr %in% c(sykehus, legevakt)]
dim(som22)

somDX <- find_episode(som22, id = "idno", acute = TRUE, days = 3)
somDX[, dxsum := sum(dup, na.rm = T), by = .(lopenr, hoveddiagnoser)]
somDX[dxsum > 2, .N, by = lopenr]

somDX[, unikDiag := uniqueN(hoveddiagnoser), by = lopenr]
somDX[, .N, by = unikDiag]
somDX[unikDiag == 3, .N, by = lopenr]
somDX[lopenr == 1001384] #kode med 5 taller. Bør alle være 4 eller 3?

## lopenr 1000098 with 2 different hoveddiagnoser
somDX[lopenr == 1000098, ..visCols]

## Notes
## Flere RHF same pasient
diagCols <- grep("diagnose", names(dt1), value = TRUE)
visCols <- c("lopenr", "helseforetak_nr", "fodselsar", "kjonn", "innDato", "utDato", "Hastegrad", "utTilstand", diagCols, "idno", "days")
helnr <- somDX[, .(rhf = uniqueN(helseforetak_nr)), keyby = lopenr]
helnr[, .N, by = rhf]
helnr[rhf == 3, .N, by = lopenr]

somDX[lopenr == 54500, ..visCols]
somDX[lopenr == 81171, ..visCols]





## FMDS --------
fmds22 <- fmds22all[!is.na(lopenr) & helseforetak_nr %in% c(sykehus, legevakt)]
dim(fmds22)

d0 <- find_case(fmds22, som22, days = 0)
d0[!is.na(lopenr) & is.na(DELXX), .N]

d3v <- find_case(fmds22, som22, days = 3, verbose = TRUE)
d3 <- find_case(fmds22, som22, days = 3)
d3[!is.na(lopenr) & is.na(DELXX), .N]

d5 <- find_case(fmds22, som22, days = 5)
d5[!is.na(lopenr) & is.na(DELXX), .N]

d7 <- find_case(fmds22, som22, days = 7)
d7[!is.na(lopenr) & is.na(DELXX), .N]

d3[, dds := paste(lopenr, helseforetak_nr, skadeDato, sep = "-")]
d3[duplicated(dds) | duplicated(dds, fromLast = T), xx.dds := 1L, by = lopenr]

d3[!is.na(lopenr) & xx.dds == 1,]

xxcols <- grep("^xx.*", names(d3), value = TRUE)
cols <- c("lopenr", "helseforetak_nr", "helseforetak_Navn", "skadeDato", "skadeTid", xxcols)
d3[lopenr %in% c(212, 581, 3240), ..cols]

lop3 <- d3[!is.na(lopenr) & xx.date3 == 3]$lopenr
d3[lopenr %in% sample(lop3, 5), ..cols]
