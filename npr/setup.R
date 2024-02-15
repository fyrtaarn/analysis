
setwd("f:/Forskningsprosjekter/PDB 3327 - Skader i Norge analy_")
source("https://raw.githubusercontent.com/helseprofil/misc/main/utils.R")

pkg <- c("pacman","data.table", "fst", "lubridate", "ggplot2", "plotly")
kh_load(char = pkg)

root <- "~/Git-fhi/analysis/npr"
source(file.path(root, "functions.R"))

# Encoding solution with some modification from
# https://github.com/StoXProject/RstoxData/issues/10#issuecomment-510542301
do_encode <- function(x) gsub("Ã¦", "æ", useBytes = TRUE,
                              gsub("Ã¥|\xe5", "å", useBytes = TRUE,
                                   gsub("Ã¸|xe6|\xf8", "ø", useBytes = TRUE,
                                        gsub("\xed", "i", useBytes = TRUE,
                                             gsub("\xc5", "Å", useBytes = TRUE,
                                                  gsub("\xd8", "Ø", x, useBytes = TRUE))))))
