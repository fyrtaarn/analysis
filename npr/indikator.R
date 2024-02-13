
root <- "~/Git-fhi/analysis/npr"
source(file.path(root, "functions.R"))
source(file.path(root, "clean-fmds.R"))
kb <- fread("./Data/Kodebok_Skader_i_Norge.csv", encoding = "Latin-1")

# Alder
dt2[, age := lubridate::year(skadeDato) - fodtAar]
dt2 <- do_agegroup(dt2, "age", c(0, 18, 25, 45, 65, Inf))

# Hardt skadd i trafikkulykker
#-------------------------------
ais3 <- dt2[kontaktarsakSkade == 1, ][ #Ulykkesskade
  alvorlighetsgrad == 3][ #AIS 3+
    skadeSted %in% c("V1","N"),]

# Kjønn
aisKJ <- ais3[, .N, by = kjonn]
aisKJ[, kjonn := as.character(kjonn)]
aisKJ[kb[variabel == "kjonn",], on = c(kjonn = "kode"), beskrivelse := beskrivelse][]

# Alder
ais3[, .N, keyby = GRP]

# Kjønn og Alder
ais3[, .N, keyby = .(kjonn, GRP)]

# Fremkomstmiddel
aisFrem <- ais3[, .N, by = fremkomstmiddel]
aisFrem[kb[variabel == "fremkomstmiddel",], on = c(fremkomstmiddel = "kode"), beskrivelse := beskrivelse][]
aisFrem[order(fremkomstmiddel)]


# Skademekanisme
aisMek <- ais3[, .N, by = skadeMekanisme]
aisMek[kb[variabel == "skadeMekanisme",], on = c(skadeMekanisme = "kode"), beskrivelse := beskrivelse][]
aisMek[order(skadeMekanisme)]


# Lettere skadd i trafikkulykker
# ------------------------------
ais2 <- dt2[kontaktarsakSkade == 1, ][ #Ulykkesskade
  alvorlighetsgrad %in% 1:2][ #AIS 3+
    skadeSted %in% c("V1","N"),]

# Kjønn
aisKJ2 <- ais2[, .N, by = kjonn]
aisKJ2[, kjonn := as.character(kjonn)]
aisKJ2[kb[variabel == "kjonn",], on = c(kjonn = "kode"), beskrivelse := beskrivelse][]

# Alder
ais2[, .N, keyby = GRP]

# Kjønn og Alder
ais2[, .N, keyby = .(kjonn, GRP)]

# Fremkomstmiddel
aisFrem <- ais2[, .N, by = fremkomstmiddel]
aisFrem[kb[variabel == "fremkomstmiddel",], on = c(fremkomstmiddel = "kode"), beskrivelse := beskrivelse][]
aisFrem[order(fremkomstmiddel)]


# Skademekanisme
aisMek <- ais2[, .N, by = skadeMekanisme]
aisMek[kb[variabel == "skadeMekanisme",], on = c(skadeMekanisme = "kode"), beskrivelse := beskrivelse][]
aisMek[order(skadeMekanisme)]
