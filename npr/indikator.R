source(file.path(root, "clean-fmds.R"))
kb <- fread("./Data/Kodebok_Skader_i_Norge.csv", encoding = "Latin-1")

# Alder


# Hardt skadd i trafikkulykker
#-------------------------------
ais3 <- dt2[kontaktarsakSkade == 1, ][ #Ulykkesskade
  alvorlighetsgrad == 3][ #AIS 3+
    skadeSted %in% c("V1","N"),]


aisFrem <- ais3[, .N, by = fremkomstmiddel]
aisMek <- ais3[, .N, by = skadeMekanisme]

aisFrem[kb[variabel == "fremkomstmiddel",], on = c(fremkomstmiddel = "kode"), beskrivelse := beskrivelse][]
aisMek[kb[variabel == "skadeMekanisme",], on = c(skadeMekanisme = "kode"), beskrivelse := beskrivelse][]

aisFrem[order(fremkomstmiddel)]
aisMek[order(skadeMekanisme)]
