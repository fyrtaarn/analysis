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
