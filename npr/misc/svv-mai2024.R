# Bestilling fra SVV om
# - Antall skader fordelt på trafikanttyper og skadegrad
# - Delt for kommunene eller byvekstavtale som Oslo, Trondheim, Bergen, Tromsø, Stavanger og Fredirkstad

root <- "~/Git-fhi/analysis/npr"
source(file.path(root, "setup.R"))

# Somatic
DT1 <- fst::read_fst("./Data/som2022og2023.fst", as.data.table = TRUE)
DT1[, behandlingsstedNavn_alternativ := do_encode(behandlingsstedNavn_alternativ)]
dt1 <- DT1[!duplicated(DT1)]
setkey(dt1, lopenr, innDato)

# FMDS
DT2 <- fst::read_fst("./Data/fmds2022og2023.fst", as.data.table = TRUE)
DT2[, helseforetak_Navn := do_encode(helseforetak_Navn)]
dt2 <- unique(DT2)
setkey(dt2, lopenr, skadeDato, skadeTid)

## # Case
## dt1[, nr := as.character(lopenr)]
## dt1[is.na(nr), nr := alt_lopenr]
## dt2[, nr := as.character(lopenr)]
## dt2[is.na(nr), nr := alt_lopenr]
## DTS <- find_case(dt2, dt1, id = "nr")

# Clean FMDS after `find_case()` above
DSS <- fst::read_fst("./Data/cleanFMDS.fst", as.data.table = TRUE)

# Filter final data only for 2023
dss <- DSS[data.table:::year(skadeDato) == 2023 & is.na(DELXX)
           ][helseforetak_nr %in% helseforetak]


dss[, byer := fcase(helseforetak_nr == 993467049, "Oslo",
                    helseforetak_nr %in% c(883974832,970188223), "Trondheim",
                    helseforetak_nr == 983974724, "Bergen",
                    helseforetak_nr %in% c(974057344,983974899), "Tromsø")]

ds <- dss[!is.na(byer)]


# alvorlighetsgrad
ds[, .N, by = alvorlighetsgrad]
show_pro(ds, "alvorlighetsgrad", kb)

# fremkomstmiddel
ds[!is.na(byer), .N, by = .(fremkomstmiddel, byer)]
dx <- show_pro(ds, "fremkomstmiddel", kb)

# AIS
show_pro(ds[alvorlighetsgrad == 1,], "fremkomstmiddel", kb)
show_pro(ds[alvorlighetsgrad == 2,], "fremkomstmiddel", kb)
show_pro(ds[alvorlighetsgrad == 3,], "fremkomstmiddel", kb)

show_ais <- function(ais,  var, filter, kb = kb, dt = ds){
  #kb = kodebok
  #dt = dataset
  #var = variable to select
  #filter = City to filter
  dd <- dt[byer == filter & alvorlighetsgrad == ais][var != "", env = list(var = var)]
  dx <- show_pro(dd, "fremkomstmiddel", kb)
  gp <- dx[!(var %chin% "-1"), env = list(var = var)][N > 3]
  txt <- sprintf("AIS %s for %s", ais, filter)
  rreg::regbar(gp, beskrivelse, N, title = txt)
}

show_ais(ais = 1, "fremkomstmiddel", filter = "Oslo", kb)
show_ais(ais = 2, "fremkomstmiddel", filter = "Oslo", kb)
show_ais(ais = 3, "fremkomstmiddel", filter = "Oslo", kb)

show_ais(ais = 1, "fremkomstmiddel", filter = "Bergen", kb)
show_ais(ais = 2, "fremkomstmiddel", filter = "Bergen", kb)
show_ais(ais = 3, "fremkomstmiddel", filter = "Bergen", kb)

show_ais(ais = 1, "fremkomstmiddel", filter = "Trondheim", kb)
show_ais(ais = 2, "fremkomstmiddel", filter = "Trondheim", kb)
show_ais(ais = 3, "fremkomstmiddel", filter = "Trondheim", kb)

show_ais(ais = 1, "fremkomstmiddel", filter = "Tromsø", kb)
show_ais(ais = 2, "fremkomstmiddel", filter = "Tromsø", kb)
show_ais(ais = 3, "fremkomstmiddel", filter = "Tromsø", kb)
