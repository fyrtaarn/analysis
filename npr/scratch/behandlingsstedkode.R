# Somatic dataset
inst1 <- function(var, d1 = dt1){
  if (is.numeric(var)){
    d1[helseforetak_nr %in% var | behandlingsstedKode %in% var, .N, by = .(helseforetak_nr, behandlingsstedNavn_alternativ, behandlingsstedKode)]
  } else {
    dt1[behandlingsstedNavn_alternativ %ilike% var, .N, by = .(helseforetak_nr, behandlingsstedNavn_alternativ, behandlingsstedKode)]
  }
}

# FMDS dataset
inst2 <- function(var, d2 = dt2){
  if(is.numeric(var)){
    dt2[helseforetak_nr %in% var, .N, by = .(helseforetak_nr, helseforetak_Navn)]
  } else {
    dt2[helseforetak_Navn %ilike% var , .N, by = .(helseforetak_nr, helseforetak_Navn)]
  }
}

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

## Somatic
dt1[, .N, by = .(behandlingsstedNavn_alternativ)]

# Bergen eller Haukeland
inst1("Bergen")
inst1("Haukeland")


bh1 <- dt1[behandlingsstedKode %in% behandsstedskode1]
bh1[, .N, by = .(behandlingsstedKode, helseforetak_nr, behandlingsstedNavn_alternativ)]

insx <- setdiff(behandsstedskode1, bh1$behandlingsstedKode)


dd1 <- dt1[!( behandlingsstedKode %in% behandsstedskode1 )]
bh2 <- dd1[helseforetak_nr %in% insx]
bh2[, .N, by = .(helseforetak_nr, behandlingsstedKode, behandlingsstedNavn_alternativ)]

## FMDS
dt2[, .N, by = .(helseforetak_Navn, helseforetak_nr)]
inst2("Bergen")

## InstitusjonsID fra filen "Organisering FT-enheter 2022 og 2023.xlsx"
behandsstedskode <- c(
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
  998563143 # Hedmarken interkommunale legevakt
)

bh1 <- dt1[helseforetak_nr %in% behandsstedskode]
bh1[, .N, by = .(behandlingsstedKode, helseforetak_nr, behandlingsstedNavn_alternativ)]

insx <- setdiff(behandsstedskode, bh1$behandlingsstedKode)


dd1 <- dt1[!( behandlingsstedKode %in% behandsstedskode )]
bh2 <- dd1[helseforetak_nr %in% insx]
bh2[, .N, by = .(helseforetak_nr, behandlingsstedKode, behandlingsstedNavn_alternativ)]


dt2[, .N, by = .(helseforetak_Navn, helseforetak_nr)]

## St. Olav
inst1(883974832)

inst2("trondheim")
inst2("olavs")

inst2("trondheim")
inst2("olavs")

# somatic og FMDS
## Lillehammeri.k. legevakt
inst1(998580072)
## Lillehammeri.k. legevakt
inst2(998580072)

## Nord
inst1(c(974057344,940101808,983974899, 983974899))
inst2(c(974057344,940101808,983974899, 983974899))

## Oslo
inst1("OUS")
inst1("Ullevål")
inst1("Storgata")
