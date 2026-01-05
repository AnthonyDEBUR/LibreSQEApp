
# modules/mod_import_bpu_progtypes_ui.R
mod_import_bpu_progtypes_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::p("Edition accessible seulement aux administrateurs. Pour les autres on ne peut que visualiser"),
    bslib::card(
      bslib::card_header("Sélection du marché et du fichier"),
      shiny::fluidRow(
        shiny::column(
          width = 4,
          shiny::selectInput(
            ns("marche_id"),
            "Sélectionnez le marché concerné",
            choices = setNames(numeric(0), character(0)) # peuplé au server
          ),
          shiny::fileInput(
            ns("xlsx"),
            label = "Importer fichier xlsx bpu_prog_type",
            accept = c(".xlsx")
          ),
          shiny::actionButton(ns("analyser"), "Analyser le fichier", icon = shiny::icon("magnifying-glass"))
        ),
        shiny::column(
          width = 8,
          bslib::card(
            bslib::card_header("Rapport de conformité"),
            shiny::htmlOutput(ns("rapport"))
          )
        )
      )
    ),
    
    bslib::card(
      bslib::card_header("Prévisualisation des onglets"),
      shiny::tabsetPanel(
        id = ns("tabs_preview"),
        type = "pills",
        shiny::tabPanel("BPU", DT::DTOutput(ns("prev_bpu"))),
        shiny::tabPanel("Cout runs analytiques", DT::DTOutput(ns("prev_runs"))),
        shiny::tabPanel("Programmes types", DT::DTOutput(ns("prev_progtypes")))
      )
    ),
    
    bslib::card(
      bslib::card_header("Chargement en base"),
      shiny::fluidRow(
        shiny::column(
          width = 6,
          shiny::actionButton(ns("charger"), "Charger (si données absentes)", icon = shiny::icon("upload")),
          shiny::helpText("Exécute LibreSQE::func_charge_marche si tout est conforme et aucune donnée existante pour ce marché.")
        ),
        shiny::column(
          width = 6,
          shiny::actionButton(ns("ecraser"), "Écraser & recharger", icon = shiny::icon("trash-can")),
          shiny::helpText("Supprime les données existantes liées au marché (runs, prix runs, BPU & prix, programmes types) puis recharge.")
        )
      ),
      shiny::verbatimTextOutput(ns("log"))
    )
  )
}
