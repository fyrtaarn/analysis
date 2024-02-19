library(shiny)
library(data.table)
library(rlang)
# 
# froot <- "f:/Forskningsprosjekter/PDB 3327 - Skader i Norge analy_"
# ffile <- file.path(froot, "Data/Kodebok_Skader_i_Norge.csv")
# kb <- data.table::fread(ffile, encoding = "Latin-1")

kb <- readRDS("code.RDS")

ui <- fluidPage(
  titlePanel("Kodebok"),
  selectInput(inputId = "kode",
              label = "Velg variabelnavn:",
              choices = c("Aktivitet ved skade" = "aktivitetSkade",
                          "Alvorlighetsgrad" = "alvorlighetsgrad",
                          "Arbeidsgiver bransje" = "arbeidsgiver",
                          "Fremkomstmiddel" = "fremkomstmiddel",
                          "Hastegrad" = "Hastegrad",
                          "Kjønn" = "kjonn",
                          "Kontaktårsak" = "kontaktarsakSkade",
                          "Kontakttype" = "kontaktType",
                          "Omsorgsnivå" = "omsorgsniva",
                          "Skademekanisme" = "skadeMekanisme",
                          "Skadested" = "skadeSted",
                          "Uttilstand" = "utTilstand"
                          )),

  tableOutput("kode")
)

server <- function(input, output, session){
  output$kode <- renderTable(kb[variabel %like% input$kode] )
}


shinyApp(ui, server)
