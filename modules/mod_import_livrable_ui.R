mod_import_livrable_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    h2("Importer un fichier xml / QUESU / EDILABO"),
    
    fluidRow(
      column(
        4,
        # 🔵 Sélection du marché (nouveau)
        selectInput(
          ns("marche"), "Sélectionnez le marché",
          choices = NULL
        ),
        
        # 🔵 Liste filtrée des BDC
        selectInput(
          ns("bco"), "Sélectionnez le bon de commande",
          choices = NULL
        ),
        
        fileInput(
          ns("fichier"), "Sélectionner un fichier XML/QUESU/EDILABO",
          accept = c(".xml")
        ),
        actionButton(ns("lancer_test"), "Tester la conformité", icon = icon("check")),
        hr(),
        downloadButton(ns("dl_rapport"), "Télécharger rapport .xlsx")
      ),
      
      column(
        8,
        tabsetPanel(
          id = ns("tabs_test"),
          
          tabPanel("Résumé", verbatimTextOutput(ns("resume"))),
          
          tabPanel("Non-conformités",
                   uiOutput(ns("ui_nonconf"))
          ),
          
          tabPanel("Données extraites",
                   DT::dataTableOutput(ns("table_analyses"))
          )
        )
      )
    )
  )
}