library(data.table)
library(norgeo)
library("PxWebApiData")
vignette("Introduction", package ="PxWebApiData")

## Tabel for befolkning

url <- "https://data.ssb.no/api/v0/no/table/11342"
kom24 <- get_code("kom", 2024, names = F)$code

dtKomm <- ApiData(url,
                  Region = list("agg_single:KommGjeldende", kom24),
                  ContentsCode = "Folkemengde",
                  Tid = "2024")
