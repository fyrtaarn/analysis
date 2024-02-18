library(shiny)
library(data.table)

froot <- "f:/Forskningsprosjekter/PDB 3327 - Skader i Norge analy_"
ffile <- file.path(froot, "Data/Kodebok_Skader_i_Norge.csv")
kb <- data.table::fread(ffile, encoding = "Latin-1")

ui <- fluidPage(
  titlePanel("Kodebok"),
  selectInput(inputId = "kode",
              label = "Valg koder:",
              choices = c("Aktivitet ved skade" = "aktivitetSkade",
                          "Alvorlighetsgrad" = "alvorlighetsgrad",
                          "Fremkomstmiddel" = "fremkomstmiddel",
                          "Hastegrad" = "Hastegrad",
                          "Kontaktårsak skade" = "kontaktarsakSkade",
                          "Kontakttype" = "kontaktType",
                          "Omsorgsnivå" = "omsorgsniva",
                          "Skademekanisme" = "skadeMekanisme",
                          "Skadested" = "skadeSted",
                          "Uttilstand" = "utTilstand"
                          )),

  tableOutput("kode")
)

server <- function(input, output, session){
  output$kode <- renderTable(kb[variabel == input$kode] )
}

shinyApp(ui, server)
