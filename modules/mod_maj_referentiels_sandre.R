
# --- Module : Mise à jour des référentiels SANDRE ----------------------------


# modules/mod_maj_global.R
majrefSANDRE_UI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Référentiels SANDRE — suivi des mises à jour"),
    shiny::p("Dernières dates de mise à jour par référentiel."),
    shiny::actionButton(ns("maj_all"), "Mettre à jour l'ensemble des référentiels", icon = shiny::icon("sync")),
    shiny::tags$hr(),
    shiny::uiOutput(ns("rows"))
  )
}

majrefSANDRE_server <- function(id, con) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # --- Helpers DB : compatibles pool::Pool et DBI::PqConnection ------------
    is_pool <- function(con) inherits(con, "Pool")
    
    db_get <- function(con, sql) {
      if (is_pool(con) && requireNamespace("pool", quietly = TRUE)) {
        pool::dbGetQuery(con, sql)
      } else {
        DBI::dbGetQuery(con, sql)
      }
    }
    
    # Lecture du tableau SQE + rafraîchissement
    reload_data <- function() {
      db_get(con, "SELECT * FROM sqe.ts_suivi_maj_refer;")
    }
    ref_rv <- shiny::reactiveVal(reload_data())
    
    fmt_fr <- function(x) format(as.Date(x), "%d/%m/%Y")
    
    # --- Affichage (2 colonnes : nom + date) ---------------------------------
    output$rows <- shiny::renderUI({
      df <- ref_rv()
      shiny::req(nrow(df) > 0)
      shiny::tagList(
        lapply(seq_len(nrow(df)), function(i) {
          shiny::fluidRow(
            class = "mb-2",
            shiny::column(6, shiny::strong(df$ts_nom_referentiel[i])),
            shiny::column(6, fmt_fr(df$ts_date[i]))
          )
        })
      )
    })
    
    # --- Bouton global : mise à jour de toutes les tables --------------------
    shiny::observeEvent(input$maj_all, {
      shiny::withProgress(message = "Mise à jour de l'ensemble des référentiels SANDRE...", value = 0, {
        tryCatch({
          # Appel sans paramètre
          LibreSQE::func_maj_referentiels_sandre(con)
          
          shiny::showNotification("Tous les référentiels ont été mis à jour.", type = "message")
        }, error = function(e) {
          shiny::showNotification(paste0("Échec mise à jour globale : ", e$message), type = "error", duration = 10)
        })
      })
      # Rafraîchir le tableau des dates
      ref_rv(reload_data())
    })
  })
}
