
# modules/mod_perimetres_ui.R
mod_perimetres_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h1("Périmètres de gestion"),
    shiny::p("Visualiser, éditer et ajouter des périmètres de gestion."),
    DT::DTOutput(ns("dt_per")),
    shiny::div(
      class = "mt-3",
      shiny::actionButton(ns("btn_edit_row"), "Éditer la ligne sélectionnée", icon = shiny::icon("pen")),
      shiny::actionButton(ns("btn_add_row"),  "Ajouter un périmètre",        icon = shiny::icon("plus"))
    )
  )
}
