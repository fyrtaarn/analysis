# Setup
root <- "~/Git-fhi/analysis/npr"
source(file.path(root, "setup.R"))
source(file.path(root, "clean-fmds.R"))

# Hendelser / episoder ---------------------
DT1 <- fst::read_fst("./Data/som2023des.fst", as.data.table = TRUE)

dt1 <- DT1[!duplicated(DT1)] # slett duplikater
dt1 <- dt1[order(innDato, lopenr)] #sort
dt1[, lnr := 1:.N] # linenumber

dt1[is.na(lopenr), .N]
dt1 <- dt1[!is.na(lopenr)]

## Bare for 2022 ------
dd <- dt1[lubridate::year(innDato) == 2022, ]

(opp <- dd[, .(count =.N), by = lopenr][order(count)])

## S00 - T78 --------
dx1 <- get_valid_codes(dt = dd, "hoveddiagnoser", "hovdiag")

show_pro(dx1, "hovdiag")

dx2 <- dx1[hovdiag == 1]

## Hastegrad 1 --------
show_pro(dx2, "Hastegrad", kb) # 336 544 i rapporten fra 2022
dx3 <- dx2[Hastegrad == 1]

(opp1 <- dx3[, .(count =.N), by = lopenr][order(count)])

# x1 <- opp1[count == 6,]$lopenr
# dx3[dager %between% c(3,5), ][lopenr %in% x1, .(lopenr)]
dx3[lopenr == 935970] #
dx3[lopenr == 553390] #

# Forskjeller i dager fra forriger hendelse til den neste
dx3[, dager := innDato - shift(innDato, type = "lag"), by = lopenr]
(lnr3 <- dx3[dager == 3][!duplicated(lopenr), .(lopenr)])

dx3[lopenr == 75421]
dx3[lopenr == 111537]

dx3[lopenr == 423869] #like S662 - ingen FMDS
dx3[lopenr == 202036] #like S711 - en i FMDS
dx3[lopenr == 503386] #2 forskjellige koder - ingen FMDS

( lnr5 <- dx3[dager == 5][!duplicated(lopenr), .(lopenr)] )
dx3[lopenr == 71513]
dx3[lopenr == 94874]

# Finnes det på FMDS 2022 ----------------------------
fdt <- dt2[lubridate::year(skadeDato) == 2022, ]

fdt[lopenr == 202036] #S711 - bare en i FMDS mens to episoder i NPR
fdt[lopenr == 503386] #2 forskjellige koder men ingen FMDS

## 3 dager forskjell i innDato
f3 <- fdt[lopenr %in% lnr3$lopenr, .N, by = lopenr]
f3[N == 3]
fdt[lopenr == 75421]
fdt[lopenr == 111537]

## 4 dager forskjell i innDato
f5 <- fdt[lopenr %in% lnr5$lopenr, .N, by = lopenr]
f5[N == 3]
fdt[lopenr == 71513]
fdt[lopenr == 94874]

# Hendelse FMDS
fdt[lopenr == 11947] #Hvilken skal slettes?


# Feil rapportert verdi
source(file.path(root, "clean-fmds.R"))
show_pro(dt2, "fremkomstmiddel", kb)

# Valg av episoder
dd <- find_episode(dt1, year = 2022, acute = TRUE, days = 3)

dd[lopenr == 164629] #example
dd[lopenr == 879285]

## ---------------------------------
# Johan spørsmål om antall
# pasienter med bare en eller flere bidiagnoser S00-T78 i 2022
dj <- get_valid_codes(dt = dx1, "bidiagnoser", "bidiag", sep = " ")

dj[hovdiag == 0 & bidiag == 1, .N]
dj[hovdiag == 0 & bidiag == 1, ]

dj[hovdiag == 0 & bidiag == 1, ][!duplicated(lopenr), .N]
dj[hovdiag == 0 & bidiag == 1, ][!duplicated(lopenr), ]

dj[hovdiag == 0 & bidiag == 1, ][!duplicated(lopenr) & Hastegrad == 1, .N]
dj[hovdiag == 0 & bidiag == 1, ][!duplicated(lopenr) & Hastegrad == 1, ]
