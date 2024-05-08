## behandlingsstedOrgNr fra filen "Organisering 2022 FT-enheter_fra_Inger_Dahlstrøm.xlsx"
behandsstedskode1 <- c(
  922573026,
  974557746,
  883974832,
  974329506,
  996320731,
  974749025,
  974329506,
  993546550,
  976677544,
  993187178,
  974748304,
  974795639,
  974795787,
  974589087,
  974589095
)

bh1 <- dt1[behandlingsstedKode %in% behandsstedskode1]
bh1[, .N, by = .(behandlingsstedKode, helseforetak_nr, behandlingsstedNavn_alternativ)]

insx <- setdiff(behandsstedskode1, bh1$behandlingsstedKode)


dd1 <- dt1[!( behandlingsstedKode %in% behandsstedskode1 )]
bh2 <- dd1[helseforetak_nr %in% insx]
bh2[, .N, by = .(helseforetak_nr, behandlingsstedKode, behandlingsstedNavn_alternativ)]


## InstitusjonsID fra filen "Organisering FT-enheter 2022 og 2023.xlsx"
behandsstedskode <- c(
  #2022
  983974724, # Helse Bergen HF
  883974832, # St.Olav Hospital HF
  974329506, # Orkdal sjukehus
  993187178, # Trondheim Legevakt
  983974899, # Universitetssykehuset i Nord-Norge HF
  983971709, # Sykehuset Innlandet HF
  983975240, # Sørlandet Sykehus HF
  983975259, # Sykehuset i Vestfold HF
  993467049, # OUS HF
  #2023
  983974724, # Helse Bergen HF
  883974832, # St.Olav Hospital HF
  993187178, # Trondheim Legevakt
  940101808, # Tromsø kommunale legevakt
  998580072, # Lillehammer i.k. legevakt
  974631792, # Sykehuset Innlandet, Sanderud
  983971709, # Sykehuset Innlandet HF
  983975240, # Sørlandet Sykehus HF
  983975259, # Sykehuset i Vestfold HF
  993467049, # OUS HF
  998563143 # Hedmarken interkommunale legevakt
)

bh1 <- dt1[helseforetak_nr %in% behandsstedskode]
bh1[, .N, by = .(behandlingsstedKode, helseforetak_nr, behandlingsstedNavn_alternativ)]

insx <- setdiff(behandsstedskode, bh1$behandlingsstedKode)


dd1 <- dt1[!( behandlingsstedKode %in% behandsstedskode )]
bh2 <- dd1[helseforetak_nr %in% insx]
bh2[, .N, by = .(helseforetak_nr, behandlingsstedKode, behandlingsstedNavn_alternativ)]

## St. Olav
dt1[helseforetak_nr == 883974832, .N, by = behandlingsstedNavn_alternativ]

# somatic og FMDS
## Lillehammeri.k. legevakt
dt1[helseforetak_nr == 998580072, .N, by = helseforetak_Navn]
dt1[helseforetak_nr == 998580072, .N, by = helseforetak_Navn]
## Lillehammeri.k. legevakt
dt2[helseforetak_nr == 998580072, .N, by = helseforetak_Navn]
dt2[helseforetak_nr == 998580072, .N, by = helseforetak_Navn]

dt2[helseforetak_Navn %like% "Oslo", .N, by = helseforetak_Navn]
dt2[, .N, by = .(helseforetak_Navn, helseforetak_nr)]
