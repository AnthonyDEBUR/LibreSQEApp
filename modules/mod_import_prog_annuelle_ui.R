
mod_import_prog_annuelle_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    shinyjs::useShinyjs(),
    h3("Importer la programmation annuelle"),
    
    fluidRow(
      column(
        width = 3,
        
        # Inputs
        selectInput(ns("select_marche"), "Marché (mar_id)", choices = NULL),
        numericInput(ns("select_annee"), "Année de programmation",
                     value = as.integer(format(Sys.Date(), "%Y")),
                     min = 2000, max = 2100),
        selectInput(ns("select_freq_bdc"), "Fréquence des BDC",
                    choices = c("mensuelle","bimestrielle","trimestrielle",
                                "semestrielle","annuelle")),
        textInput(ns("prefixe_bdc"), "Préfixe BDC (optionnel)", value = ""),
        
        fileInput(
          ns("file_xlsx"),
          "Fichier .xlsx (programme_annuel + calendrier)",
          accept = ".xlsx"
        ),
        
        # --- Boutons masqués par défaut --- #
        shinyjs::hidden(
          div(
            id = ns("zone_btn_verifier"),
            actionButton(
              ns("btn_verifier"),
              "Vérifier la conformité",
              icon = icon("magnifying-glass")
            )
          )
        ),
        
        shinyjs::hidden(
          div(
            id = ns("zone_btn_importer"),
            actionButton(
              ns("btn_importer"),
              "Importer",
              icon = icon("cloud-arrow-up"),
              class = "btn-primary"
            )
          )
        ),
        
        shinyjs::hidden(
          div(
            id = ns("zone_btn_ecraser"),
            actionButton(
              ns("btn_ecraser_et_importer"),
              "Écraser puis Importer",
              icon = icon("triangle-exclamation"),
              class = "btn-danger"
            )
          )
        ),
        
        downloadButton(ns("btn_dl_rapport"), "Télécharger le rapport")
      ),
      
      column(
        width = 9,
        
        tabsetPanel(
          tabPanel("Prévisualisation - programme_annuel",
                   DT::DTOutput(ns("dt_prog_annuel"))),
          tabPanel("Prévisualisation - calendrier",
                   DT::DTOutput(ns("dt_calendrier"))),
          tabPanel("Données déjà en base (nb de lignes)",
                   DT::DTOutput(ns("dt_etat_base")),
                   p(em("Données existantes pour le marché et l'année indiqués."))),
          tabPanel("Rapport de conformité",
                   tags$pre(style="white-space: pre-wrap;",
                            textOutput(ns("rapport_text"))))
        )
      )
    )
  )
}
