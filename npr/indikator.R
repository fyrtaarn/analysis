
root <- "~/Git-fhi/analysis/npr"
source(file.path(root, "setup.R"))
source(file.path(root, "functions.R"))
source(file.path(root, "clean-fmds.R"))
kb <- fread("./Data/Kodebok_Skader_i_Norge.csv", encoding = "Latin-1")

# Alder
dt2[, age := lubridate::year(skadeDato) - fodtAar]
dt2 <- do_agegroup(dt2, "age", c(0, 18, 25, 45, 65, Inf))

# Koder skade sted
# ----------------
## kb[variabel == "skadeSted"]
sted <- dt2[, .N, by = skadeSted]
stedTBL <- show_pro(dt2, "skadeSted", kb)
stedTBL

# Hardt skadd i trafikkulykker
#-------------------------------
skadeKB <- c("V1", "N")
ais3 <- dt2[kontaktarsakSkade == 1, ][ #Ulykkesskade
  alvorlighetsgrad == 3][ #AIS 3+
    skadeSted %in% skadeKB,]

# Kjønn
kjonnTBL3 <- show_pro(ais3, "kjonn", kb)

# Alder
ageTBL3 <- show_pro(ais3, "GRP")

# Kjønn og Alder
ais3[, .N, keyby = .(kjonn, GRP)]

# Koder for fremkomstmiddel (OBS! mange feil rapportert)
# --------------------------
fremTBL3 <- show_pro(dt2, "fremkomstmiddel", kb)

# Fremkomstmiddel
aisFrem <- ais3[, .N, by = fremkomstmiddel]
aisFrem[kb[variabel == "fremkomstmiddel",], on = c(fremkomstmiddel = "kode"), beskrivelse := beskrivelse][
  order(fremkomstmiddel)]


# Skademekanisme
aisMek <- ais3[, .N, by = skadeMekanisme]
aisMek[kb[variabel == "skadeMekanisme",], on = c(skadeMekanisme = "kode"), beskrivelse := beskrivelse][order(skadeMekanisme)]


# Lettere skadd i trafikkulykker
# ------------------------------
ais2 <- dt2[kontaktarsakSkade == 1, ][ #Ulykkesskade
  alvorlighetsgrad %in% 1:2][ #AIS 1 & 2
    skadeSted %in% skadeKB]

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
aisFrem[kb[variabel == "fremkomstmiddel",], on = c(fremkomstmiddel = "kode"), beskrivelse := beskrivelse][order(fremkomstmiddel)]


# Skademekanisme
aisMek <- ais2[, .N, by = skadeMekanisme]
aisMek[kb[variabel == "skadeMekanisme",], on = c(skadeMekanisme = "kode"), beskrivelse := beskrivelse][order(skadeMekanisme)]
