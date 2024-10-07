# Analyse for fotgjengere
# -----------------------
source(file.path(here::here(), "misc", "functions.R"))
pkg <- c("data.table", "here")
loadpkg(pkg)

fnc <- "c:/Users/ybka/Git-fhi/analysis/npr/functions/utils.R"
source(fnc)

fdrive <- "f:/Forskningsprosjekter/PDB 3327 - Skader i Norge analy_"

# kodebok
kb <- fread(file.path(fdrive, "Data/Kodebok_Skader_i_Norge.csv"), encoding = "Latin-1")

# Somatikk
SOM <- fread(file.path(fdrive,"Data/02_extracted/NPR20240711/24_01903_som_til_utlevering.csv"),
             encoding = "Latin-1")

somdt <- unique(SOM)
somdt[, yr := year(innDato)]
## somdt <- somdt[yr == 2024]
setkey(somdt, lopenr, innDato)

# FMDS
FMDS <- fread(file.path(fdrive,"Data/02_extracted/NPR20240711/24_01903_fmds_til_utlevering.csv"),
              encoding = "Latin-1")

fmddt <- unique(FMDS)
fmddt[, yr  := year(skadeDato)]
## fmddt <- fmddt[yr == 2024]

setkey(fmddt, lopenr, skadeDato, skadeTid)

fmddt[, .N, by = yr]

show_pro(fmddt, "skadeMekanisme", kb)
show_pro(fmddt, "skadeSted", kb)
show_pro(fmddt, "fremkomstmiddel", kb)

fmddt[skadeMekanisme == "F2", .N] #Annet fall
fmddt[skadeMekanisme == "F2" & skadeMekanisme != "F1", .N] #Annet fall
fmddt[skadeSted %chin% c("V1", "V2")] #Vei, gate etc for trafikkulykke og ikke trafikkulykke

vars <- c("V1", "V2")

fotx <- fmddt[skadeSted %chin% vars, env = list(vars = I(vars))][skadeMekanisme == "F2"]
fot <- fmddt[skadeSted %chin% c("V1", "V2")][skadeMekanisme == "F2"][fremkomstmiddel == "F"]
fotm <- fmddt[skadeSted %chin% c("V1", "V2")][skadeMekanisme == "F2"][fremkomstmiddel %chin% c("F", "A", "U")] #Til fots, Annet, Ukjent

fotx[, .N, by = yr]
fot[, .N, by = yr]
fotm[, .N, by = yr]
