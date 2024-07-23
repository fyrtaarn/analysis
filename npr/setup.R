
setwd("f:/Forskningsprosjekter/PDB 3327 - Skader i Norge analy_")
source("https://raw.githubusercontent.com/helseprofil/misc/main/utils.R")

pkg <- c("pacman","data.table", "fst", "lubridate", "ggplot2", "plotly", "S7", "stringi")
kh_load(char = pkg)

root <- "~/Git-fhi/analysis/npr"
fx <- list.files(file.path(root, "functions"))
for (i in fx)
  source(file.path(root, "functions", i))

# Encoding solution with some modification from
# https://github.com/StoXProject/RstoxData/issues/10#issuecomment-510542301
do_encode <- function(x) gsub("Ã¦|\xe6", "æ", useBytes = TRUE,
                              gsub("Ã¥|\xe5", "å", useBytes = TRUE,
                                   gsub("Ã¸|\xf8", "ø", useBytes = TRUE,
                                        gsub("\xed", "i", useBytes = TRUE,
                                             gsub("\xc5", "Å", useBytes = TRUE,
                                                  gsub("\xd8", "Ø", x, useBytes = TRUE))))))

# Encoding solution with some modification from
# https://github.com/StoXProject/RstoxData/issues/10#issuecomment-510542301
is_encode <- function(x) gsub("\\u00c3\\u00a6|\xe6", "\u00e6", useBytes = TRUE,
                              gsub("\\u00c3\\u00a5|\xe5", "\u00e5", useBytes = TRUE,
                                   gsub("\\u00c3\\u00b8|\xf8", "\u00f8", useBytes = TRUE,
                                        gsub("\xed", "i", useBytes = TRUE,
                                             gsub("\xc5", "\u00c5", useBytes = TRUE,
                                                  gsub("\xd8", "\u00d8", x, useBytes = TRUE))))))

kb <- fread("./Data/Kodebok_Skader_i_Norge.csv", encoding = "Latin-1")
# kb[variabel %like% "^Org"][beskrivelse %like% "OUS"][, .(beskrivelse, kode)]

# Check of institutions codes or names
# -------------------------------------
## Somatic dataset
inst1 <- function(var, d1 = dt1){
  if (is.numeric(var)){
    d1[helseforetak_nr %in% var | behandlingsstedKode %in% var, .N, by = .(helseforetak_nr, behandlingsstedNavn_alternativ, behandlingsstedKode)]
  } else {
    dt1[behandlingsstedNavn_alternativ %ilike% var, .N, by = .(helseforetak_nr, behandlingsstedNavn_alternativ, behandlingsstedKode)]
  }
}

## FMDS dataset
inst2 <- function(var, d2 = dt2){
  if(is.numeric(var)){
    dt2[helseforetak_nr %in% var, .N, by = .(helseforetak_nr, helseforetak_Navn)]
  } else {
    dt2[helseforetak_Navn %ilike% var , .N, by = .(helseforetak_nr, helseforetak_Navn)]
  }
}

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

## Helseforetak og InstitusjonsID fra filen "Organisering FT-enheter 2022 og 2023.xlsx"
helseforetak <- c(
  #2022
  983974724, # Helse Bergen HF
  883974832, # St.Olav Hospital HF
  974329506, # Orkdal sjukehus
  993187178, # Trondheim Legevakt
  970188223, # Trondheim legevakt - HF
  983974899, # Universitetssykehuset i Nord-Norge HF
  983971709, # Sykehuset Innlandet HF
  983975240, # Sørlandet Sykehus HF
  983975259, # Sykehuset i Vestfold HF
  993467049, # OUS HF
  #2023
  983974724, # Helse Bergen HF
  883974832, # St.Olav Hospital HF
  993187178, # Trondheim Legevakt - InstID
  970188223, # Trondheim legevakt - HF
  940101808, # Tromsø kommunale legevakt - InstID
  974057344, # Tromsø kommune - HF
  998580072, # Lillehammer i.k. legevakt - InstID
  898564282, # Lillehammer i.k. legevakt - HF
  983971709, # Sykehuset Innlandet HF
  974631792, # Sykehuset Innlandet, Sanderud - InstID
  983975240, # Sørlandet Sykehus HF
  983975259, # Sykehuset i Vestfold HF
  993467049, # OUS HF
  998563143  # Hedmarken interkommunale legevakt
)
