
# modules/mod_fusion_bdc_ui.R
mod_fusion_bdc_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        3,
        h4("Sélection"),
        selectInput(ns("select_marche"), "Marché", choices = character(0)),
        selectInput(
          ns("select_perimetre"),
          "Périmètre de gestion",
          choices = character(0),
          multiple = FALSE
        ),
        selectInput(
          ns("select_annees"),
          "Année(s) des bons de commandes à afficher",
          choices = character(0),
          multiple = TRUE
        ),
        checkboxInput(
          ns("chk_delete_bco"),
          "Supprimer les BCO fusionnés (sauf le premier)",
          value = FALSE
        ),
        actionButton(
          ns("btn_fusion"),
          "Fusionner les bons de commandes sélectionnés",
          icon = icon("object-group"),
          class = "btn btn-primary"
        ),
        br(), br(),
        verbatimTextOutput(ns("txt_resume"))
      ),
      column(
        9,
        h4("Liste des bons de commandes"),
        p("Sélectionnez au moins deux lignes pour activer la fusion."),
        DT::DTOutput(ns("dt_bco"))
      )
    )
  )
}