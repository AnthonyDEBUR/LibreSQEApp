
# modules/mod_prestataires_ui.R
prestatairesUI <- function(id) {
  ns <- shiny::NS(id)
  
  tagList(
    tags$h1("Prestataires (référentiel)"),
    tags$p("Liste issue de refer.tr_prestataire_pre : pre_nom, SIRET, code SANDRE."),
    DT::DTOutput(ns("dt_prestataires")),
    tags$hr(),
    
    bslib::card(
      header = "Ajouter un prestataire (recherche dans le référentiel SANDRE des intervenants)",
      bslib::card_body(
        fluidRow(
          column(
            6,
            # --- Recherche dans la table SANDRE des intervenants ---
            tags$h3("Recherche dans refer.tr_intervenantsandre_isa"),
            radioButtons(
              ns("search_mode"), "Mode de recherche",
              choices = c("Code SANDRE" = "code",
                          "Nom" = "nom",
                          "Mnémonique" = "mnemo"),
              selected = "code", inline = TRUE
            ),
            textInput(ns("search_text"), "Saisir la valeur à rechercher"),
            actionButton(ns("btn_search"), "Rechercher", icon = icon("magnifying-glass")),
            DT::DTOutput(ns("dt_candidates")),
            tags$small("Cliquez une ligne ci-dessus pour pré-remplir le formulaire.")
          ),
          column(
            6,
            # --- Formulaire d'ajout d'un prestataire ---
            tags$h3("Nouveau prestataire"),
            textInput(ns("pre_nom"), "Nom du prestataire (pre_nom)"),
            textInput(
              ns("pre_siret"),
              "SIRET (affiché à la place du code SANDRE ; par défaut identique au code SANDRE)",
              placeholder = "14 chiffres ou identique au code SANDRE si inconnu"
            ),
            textInput(ns("pre_isa_codesandre"), "Code SANDRE (pre_isa_codesandre)", placeholder = "Ex. ISA123456"),
            actionButton(ns("btn_add"), "Ajouter à refer.tr_prestataire_pre", icon = icon("plus")),
            tags$br(), tags$br(),
            uiOutput(ns("add_status"))
          )
        )
      )
    )
  )
}
