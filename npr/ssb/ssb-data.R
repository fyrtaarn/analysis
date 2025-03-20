library(data.table)
library(norgeo)
library("PxWebApiData")
vignette("Introduction", package ="PxWebApiData")

## Tabel for befolkning

url <- "https://data.ssb.no/api/v0/no/table/11342"

## 2024 population
kom24 <- get_code("kom", 2024, names = F)$code
dtKomm <- ApiData(url,
                  Region = list("agg_single:KommGjeldende", kom24),
                  ContentsCode = "Folkemengde",
                  Tid = "2024")

## 2023 population
kom23 <- get_code("kom", 2020, names = F)$code
kom20 <- kom23[-length(kom23)] # 9999 isn't in 2023 kommuner

dtKomm23 <- ApiData(url,
                    Region = list("agg_single:Komm2020", kom20),
                    ContentsCode = "Folkemengde",
                    Tid = "2023")

# ---------------------------------------
# Tabel for age and population
# Define the URL for the API endpoint
# ---------------------------------------
# Using SSB recommended approach
# ------------------------------
library(PxWebApiData)
library(norgeo)
library(orgdata)
library(data.table)
url <- "https://data.ssb.no/api/v0/no/table/07459/"

komm <- norgeo::get_code("kom", 2023)
age <- c(1:150)

## only codes
pop <- ApiData2(url, Region = komm[["code"]], Kjonn = 1:2, Alder = age, Tid = "2023")
## code and text
pop12 <- ApiData12(url, Region = komm[["code"]], Kjonn = 1:2, Alder = age, Tid = "2023")
setDT(pop)
setDT(pop12)
pop
pop12

pop[, .N, keyby = Alder]
pop12[, .N, keyby = region]

pop12[Alder %chin% "105+", Alder := 105][, age := as.integer(Alder)]
pop12 <- do_agegroup(pop12, "age", c(0, 15, 25, 45, 65, 80, Inf), "agegp")
pop12[, .N, keyby = agegp]

names(pop12)
cols <- c("region", "Region", "kjønn", "Kjonn", "Alder", "age", "agegp", "value")
pop12 <- pop12[, ..cols]
setnames(pop12, 3, "kjonn")

komm23 <- norgeo::track_change("k", 1990, 2023, fix = TRUE)
