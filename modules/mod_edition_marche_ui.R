
# mod_edition_marche_ui
mod_edition_marche_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    # Bandeau : filtre, nouveau, statut calculé
    shiny::fluidRow(
      shiny::column(
        width = 4,
        shiny::radioButtons(
          ns("filtre_marches"),
          label = "Afficher",
          choices = c("Marchés actifs" = "actifs", "Tous les marchés" = "tous"),
          selected = "actifs", inline = TRUE
        )
      ),
      shiny::column(
        width = 4,
        shiny::actionButton(ns("new"), "Nouveau marché", icon = shiny::icon("plus"), class = "btn-success")
      ),
      shiny::column(
        width = 4,
        shiny::uiOutput(ns("statut_ui"))
      )
    ),
    
    # Tableau des marchés
    DT::DTOutput(ns("tbl_marches")),
    shiny::hr(),
    
    # --- Formulaire d'édition ---
    shiny::textInput(ns("mar_nom"), "Nom du marché (unique)", value = ""),
    shiny::textInput(ns("mar_nom_long"), "Nom long du marché (requis)", value = ""),
    shiny::textInput(ns("mar_reference"), "Référence du marché", value = ""),
    shiny::dateInput(ns("mar_datedebut"), "Date de début", value = Sys.Date(), format = "yyyy-mm-dd", language = "fr"),
    shiny::dateInput(ns("mar_datefin"), "Date de fin de validité", value = Sys.Date() + 365, format = "yyyy-mm-dd", language = "fr"),
    
    shiny::fluidRow(
      shiny::column(
        6,
        shiny::numericInput(
          ns("mar_montantmin"),
          label = "Montant HT minimum du marché",
          value = NA_real_, min = 0, step = 100
        )
      ),
      shiny::column(
        6,
        shiny::numericInput(
          ns("mar_montantmax"),
          label = "Montant HT maximum du marché",
          value = NA_real_, min = 0, step = 100
        )
      )
    ),
    
    # Titulaire principal (sélection unique)
    shiny::selectInput(
      ns("titulaire_id"),
      label = "Titulaire du marché (prestataire principal)",
      choices = NULL,    # alimenté côté serveur
      selected = NULL
    ),
    
    shiny::br(),
    shiny::fluidRow(
      shiny::column(4, shiny::actionButton(ns("save"),   "Enregistrer",  icon = shiny::icon("save"),  class = "btn-primary")),
      shiny::column(4, shiny::actionButton(ns("reset"),  "Réinitialiser", icon = shiny::icon("undo"))),
      shiny::column(4, shiny::actionButton(ns("delete"), "Supprimer",    icon = shiny::icon("trash"), class = "btn-danger"))
    ),
    
    shiny::hr(),
    shiny::verbatimTextOutput(ns("debug")) # optionnel
  )
}
