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

skim(som)
skim(fmd)

# Subset
fmd23 <- fmd[year == 2023]
som23 <- som[year == 2023]

caseN0 <- count_case(som23, acute=T)
caseN3 <- count_case(som23, days = 3, acute=T)

all <- caseN3[!is.na(lopenr) & dup != 1,]
fm <- caseN3[!is.na(lopenr) & dup != 1 & Ft_dummy_Spesialist, ]

dim(all)
dim(fm)

show_pro(all, "kjonn")
show_pro(fm, "kjonn")
