## Indikatorer
root <- "~/Git-fhi/analysis/npr"
source(file.path(root, "setup.R"))
source(file.path(root, "./functions/utils.R"))
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

skadeKB <- c("V1", "N")

# -----------------------------------
# Hardt skadd i trafikkulykker AIS 3+
# -----------------------------------
ais3 <- dt2[kontaktarsakSkade == 1, ][ #Ulykkesskade
  alvorlighetsgrad == 3][ #AIS 3+
    skadeSted %in% skadeKB,]

# Kjønn AIS3+
kjonnTBL3 <- show_pro(ais3, "kjonn", kb)

# Alder AIS3+
ageTBL3 <- show_pro(ais3, "GRP")

# Kjønn og Alder
ais3[, .N, keyby = .(kjonn, GRP)]


# Koder for fremkomstmiddel (OBS! mange feil rapportert)
# --------------------------
fremTBL <- show_pro(dt2, "fremkomstmiddel", kb)

# Fremkomstmiddel AIS3+
fremTBL3 <- show_pro(ais3, "fremkomstmiddel", kb)

# Skademekanisme AIS3+
mekTBL3 <- show_pro(ais3, "skadeMekanisme", kb)


# ----------------------------------------
# Lettere skadd i trafikkulykker AIS 1 & 2
# ----------------------------------------
ais2 <- dt2[kontaktarsakSkade == 1, ][ #Ulykkesskade
  alvorlighetsgrad %in% 1:2][ #AIS 1 & 2
    skadeSted %in% skadeKB]

# Kjønn
kjonnTBL2 <- show_pro(ais2, "kjonn", kb)

# Alder
ageTBL2 <- show_pro(ais2, "GRP")

# Kjønn og Alder
ais2[, .N, keyby = .(kjonn, GRP)]

# Fremkomstmiddel
fremTBL2 <- show_pro(ais2, "fremkomstmiddel", kb)

# Skademekanisme
mekTBL2 <- show_pro(ais2, "skadeMekanisme", kb)
