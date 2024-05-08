
setwd("f:/Forskningsprosjekter/PDB 3327 - Skader i Norge analy_")
source("https://raw.githubusercontent.com/helseprofil/misc/main/utils.R")

pkg <- c("pacman","data.table", "fst", "lubridate", "ggplot2", "plotly", "S7")
kh_load(char = pkg)

root <- "~/Git-fhi/analysis/npr"
fx <- list.files(file.path(root, "functions"))
for (i in fx)
  source(file.path(root, "functions", i))

# Encoding solution with some modification from
# https://github.com/StoXProject/RstoxData/issues/10#issuecomment-510542301
do_encode <- function(x) gsub("Ã¦", "æ", useBytes = TRUE,
                              gsub("Ã¥|\xe5", "å", useBytes = TRUE,
                                   gsub("Ã¸|xe6|\xf8", "ø", useBytes = TRUE,
                                        gsub("\xed", "i", useBytes = TRUE,
                                             gsub("\xc5", "Å", useBytes = TRUE,
                                                  gsub("\xd8", "Ø", x, useBytes = TRUE))))))


kb <- fread("./Data/Kodebok_Skader_i_Norge.csv", encoding = "Latin-1")
# kb[variabel %like% "^Org"][beskrivelse %like% "OUS"][, .(beskrivelse, kode)]


# helseforetak_nr for Fyrtårnprosjektet
sykehus <- c(
  974795639, #UNN Harstad
  974795787, #UNN Tromsø
  883974832, #St. Olavs Hospital HF
  974749025, #St. Olavs hospital
  983975240, #Sørlandet Sykehus HF
  974633574, #Sykehuset i Vestfold
  983975259, #Sykehuset i Vestfold HF
  983971709, #Sykehuset Innlandet HF
  974631768, #SI Elverum - Hamar
  874632562, #SI Lillehammer
  974589095, #OUS somatikk
  993467049 #Oslo universitetssykehus HF
  # ??? OUS Skadelegevakten
)

#behandlingsstedKode
behandssted <- c(
  974589095, # OUS Ullevål
  974589087, # OUS Oslo legevakt (Storgata)
  922573026, #Helse Bergen, Skadepoliklinikken Bergen helsehus
  974557746, #Helse Bergen, Haukeland
  974749025, #St. Olavs hospital, Trondheim
  974329506 #St. Olavs hospital, Orkdal
)

legevakt <- c(
  993187178, #Trondheim legevakt
  998563143, #Hedmarken interkomm. legevakt
  927161222, #Hedmarken interkomm. legevakt
  998580072, #Lillehammer interkomm. legevakt
  974057344 #Tromsø Komm. Legevakt
  # Kristiansand legevakt
  # Tønsberg-regionen legevakt
  # Bergen legevakt
)
