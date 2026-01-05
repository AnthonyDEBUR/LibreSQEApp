
# mod_edition_marche_server
mod_edition_marche_server <- function(
    id,
    pool,
    schema_sqe   = "sqe",
    table_mar    = "t_marche_mar",
    schema_refer = "refer",
    table_presta = "tr_prestataire_pre",
    presta_id    = "pre_id",
    presta_label = "pre_nom",
    allow_multi  = FALSE,  # un seul titulaire
    pivot_table  = NULL    # non utilisé (multi-titulaires supprimé)
) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # -------- UTIL --------
    today <- Sys.Date()
    compute_statut <- function(d1, d2) {
      if (is.na(d1) || is.na(d2)) return(NA_character_)
      if (today < d1) return("Prévu")
      if (today >= d1 && today <= d2) return("En cours")
      if (today > d2) return("Terminé")
      return(NA_character_)
    }
    # helper (évite l'opérateur rlang %||%)
    or_empty <- function(x) if (is.null(x) || length(x) == 0 || (is.character(x) && is.na(x))) "" else x
    
    fq_mar_table   <- paste0(schema_sqe,   ".", table_mar)
    fq_presta_tbl  <- paste0(schema_refer, ".", table_presta)
    
    # --- ID courant (NULL = mode création)
    current_id <- shiny::reactiveVal(NULL)
    
    # -------- LISTE PRESTATAIRES (pour selectInput unique) --------
    prestataires <- shiny::reactiveVal(data.frame(id = integer(), lib = character()))
    shiny::observe({
      sql <- sprintf("SELECT %s AS id, %s AS lib FROM %s ORDER BY %s;", presta_id, presta_label, fq_presta_tbl, presta_label)
      df <- tryCatch(DBI::dbGetQuery(pool, sql), error = function(e) {
        shiny::showNotification(paste("Erreur chargement prestataires:", e$message), type = "error")
        data.frame(id = integer(), lib = character())
      })
      prestataires(df)
      shiny::updateSelectInput(session, "titulaire_id", choices = setNames(df$id, df$lib), selected = NULL)
    })
    
    # -------- LISTE DES MARCHÉS (inclut nom long, montants et libellé titulaire) --------
    marches <- shiny::reactiveVal(
      data.frame(
        mar_id = integer(), mar_nom = character(), mar_nom_long = character(), mar_reference = character(),
        mar_datedebut = as.Date(character()), mar_datefin = as.Date(character()),
        mar_montantmin = numeric(), mar_montantmax = numeric(),
        mar_pre_id = integer(), titulaire_nom = character(),
        statut = character()
      )
    )
    
    load_marches <- function() {
      sql <- sprintf("
        SELECT
          m.mar_id,
          m.mar_nom,
          m.mar_nom_long,
          m.mar_reference,
          m.mar_datedebut,
          m.mar_datefin,
          m.mar_montantmin,
          m.mar_montantmax,
          m.mar_pre_id,
          p.%s AS titulaire_nom,
          COALESCE(m.mar_statut,
            CASE
              WHEN CURRENT_DATE < m.mar_datedebut THEN 'Prévu'
              WHEN CURRENT_DATE BETWEEN m.mar_datedebut AND m.mar_datefin THEN 'En cours'
              ELSE 'Terminé'
            END
          ) AS statut
        FROM %s m
        LEFT JOIN %s p ON p.%s = m.mar_pre_id
        ORDER BY m.mar_nom;",
                     presta_label, fq_mar_table, fq_presta_tbl, presta_id)
      
      df <- tryCatch(DBI::dbGetQuery(pool, sql), error = function(e) {
        shiny::showNotification(paste("Erreur chargement marchés:", e$message), type = "error")
        data.frame(
          mar_id = integer(), mar_nom = character(), mar_nom_long = character(), mar_reference = character(),
          mar_datedebut = as.Date(character()), mar_datefin = as.Date(character()),
          mar_montantmin = numeric(), mar_montantmax = numeric(),
          mar_pre_id = integer(), titulaire_nom = character(),
          statut = character()
        )
      })
      if (nrow(df) > 0) {
        df$mar_datedebut <- as.Date(df$mar_datedebut)
        df$mar_datefin   <- as.Date(df$mar_datefin)
      }
      marches(df)
    }
    load_marches()
    
    # -------- TABLEAU (filtré selon radio) --------
    filtered_marches <- shiny::reactive({
      df <- marches()
      if (identical(input$filtre_marches, "actifs")) {
        df <- subset(df, statut == "En cours")
      }
      df
    })
    
    output$tbl_marches <- DT::renderDT({
      df <- filtered_marches()
      
      # formatage optionnel des montants
      fmt <- function(x) ifelse(is.na(x), "", formatC(x, format = "f", digits = 2, big.mark = " ", decimal.mark = ","))
      if (nrow(df) > 0) {
        df$mar_montantmin <- fmt(df$mar_montantmin)
        df$mar_montantmax <- fmt(df$mar_montantmax)
      }
      
      # On n'affiche pas mar_pre_id (on montre le libellé titulaire)
      df_display <- df[, c("mar_id", "mar_nom", "mar_nom_long", "mar_reference",
                           "mar_datedebut", "mar_datefin", "mar_montantmin", "mar_montantmax",
                           "titulaire_nom", "statut")]
      
      DT::datatable(
        df_display,
        selection = "single",
        rownames  = FALSE,
        filter    = "top",
        options   = list(pageLength = 10, autoWidth = TRUE),
        colnames  = c(
          "ID", "Nom court", "Nom long", "Référence", "Début", "Fin",
          "Montant min TTC", "Montant max TTC", "Titulaire", "Statut"
        )
      )
    })
    tbl_proxy <- DT::dataTableProxy("tbl_marches")
    
    # -------- AFFICHAGE STATUT (du formulaire courant) --------
    output$statut_ui <- shiny::renderUI({
      s <- compute_statut(input$mar_datedebut, input$mar_datefin)
      shiny::tagList(shiny::p(shiny::strong("Statut (calculé) : "), ifelse(is.null(s) || is.na(s), "—", s)))
    })
    
    # -------- SÉLECTION D'UNE LIGNE DU TABLEAU --------
    
    shiny::observeEvent(input$tbl_marches_rows_selected, ignoreInit = TRUE, {
      idx <- input$tbl_marches_rows_selected
      if (length(idx) == 0) return()
      df  <- filtered_marches()
      rec <- df[idx, ]
      
      # Remplissage formulaire
      shiny::updateTextInput(session, "mar_nom",        value = if (isTRUE(nchar(rec$mar_nom) > 0)) rec$mar_nom else "")
      shiny::updateTextInput(session, "mar_nom_long",   value = if (isTRUE(nchar(rec$mar_nom_long) > 0)) rec$mar_nom_long else "")
      shiny::updateTextInput(session, "mar_reference",  value = if (isTRUE(nchar(rec$mar_reference) > 0)) rec$mar_reference else "")
      shiny::updateDateInput(session, "mar_datedebut",  value = as.Date(rec$mar_datedebut))
      shiny::updateDateInput(session, "mar_datefin",    value = as.Date(rec$mar_datefin))
      shiny::updateNumericInput(session, "mar_montantmin",
                                value = if (is.null(rec$mar_montantmin) || is.na(rec$mar_montantmin)) NA_real_ else as.numeric(rec$mar_montantmin))
      shiny::updateNumericInput(session, "mar_montantmax",
                                value = if (is.null(rec$mar_montantmax) || is.na(rec$mar_montantmax)) NA_real_ else as.numeric(rec$mar_montantmax))
      
      # Titulaire (sélection unique) — éviter ifelse() ici
      sel_titulaire <- if (is.null(rec$mar_pre_id) || length(rec$mar_pre_id) == 0 || is.na(rec$mar_pre_id)) {
        NULL
      } else {
        as.character(rec$mar_pre_id)
      }
      shiny::updateSelectInput(session, "titulaire_id", selected = sel_titulaire)
      
      current_id(as.integer(rec$mar_id))
    })
    
    
    # -------- NOUVEAU MARCHÉ --------
    shiny::observeEvent(input$new, {
      current_id(NULL)
      shiny::updateTextInput(session, "mar_nom",        value = "")
      shiny::updateTextInput(session, "mar_nom_long",   value = "")
      shiny::updateTextInput(session, "mar_reference",  value = "")
      shiny::updateDateInput(session, "mar_datedebut",  value = today)
      shiny::updateDateInput(session, "mar_datefin",    value = today + 365)
      shiny::updateNumericInput(session, "mar_montantmin", value = NA_real_)
      shiny::updateNumericInput(session, "mar_montantmax", value = NA_real_)
      shiny::updateSelectInput(session, "titulaire_id", selected = NULL)
      DT::selectRows(tbl_proxy, NULL)
    })
    
    # -------- VALIDATION DES CHAMPS --------
    validate_inputs <- function(is_update = FALSE, current_mar_id = NULL) {
      errs <- c()
      if (nchar(input$mar_nom)       == 0) errs <- c(errs, "Le nom du marché est requis.")
      if (nchar(input$mar_nom_long)  == 0) errs <- c(errs, "Le nom long du marché est requis.")   # requis
      if (nchar(input$mar_reference) == 0) errs <- c(errs, "La référence est requise.")
      if (isTRUE(is.na(input$mar_datedebut))) errs <- c(errs, "La date de début est requise.")
      if (isTRUE(is.na(input$mar_datefin)))   errs <- c(errs, "La date de fin est requise.")
      if (!is.na(input$mar_datedebut) && !is.na(input$mar_datefin) && input$mar_datefin < input$mar_datedebut)
        errs <- c(errs, "La fin doit être ≥ au début.")
      
      # Montants >= 0, et min <= max
      if (!is.na(input$mar_montantmin) && input$mar_montantmin < 0) errs <- c(errs, "Le montant minimum doit être ≥ 0.")
      if (!is.na(input$mar_montantmax) && input$mar_montantmax < 0) errs <- c(errs, "Le montant maximum doit être ≥ 0.")
      if (!is.na(input$mar_montantmin) && !is.na(input$mar_montantmax) && input$mar_montantmin > input$mar_montantmax)
        errs <- c(errs, "Le montant minimum doit être ≤ au montant maximum.")
      
      # Titulaire principal requis
      if (is.null(input$titulaire_id) || nchar(input$titulaire_id) == 0)
        errs <- c(errs, "Le titulaire principal du marché est requis.")
      
      # Unicité du nom (hors enregistrement édité)
      sql_check <- sprintf("SELECT mar_id FROM %s WHERE mar_nom = $1;", fq_mar_table)
      ex <- tryCatch(DBI::dbGetQuery(pool, sql_check, params = list(input$mar_nom)),
                     error = function(e) data.frame(mar_id = integer()))
      if (nrow(ex) > 0) {
        if (!is_update || (is_update && !ex$mar_id[[1]] %in% current_mar_id)) {
          errs <- c(errs, "Nom de marché déjà existant.")
        }
      }
      return(errs)
    }
    
    # -------- SAUVEGARDE (INSERT/UPDATE) --------
    shiny::observeEvent(input$save, {
      is_update      <- !is.null(current_id())
      current_id_val <- if (is_update) as.integer(current_id()) else NA_integer_
      
      errs <- validate_inputs(is_update = is_update, current_mar_id = current_id_val)
      if (length(errs) > 0) {
        shiny::showNotification(paste(errs, collapse = "\n"), type = "error", duration = 6)
        return()
      }
      
      statut       <- compute_statut(input$mar_datedebut, input$mar_datefin)
      titulaire_id <- if (is.null(input$titulaire_id) || nchar(input$titulaire_id) == 0) NA_integer_ else as.integer(input$titulaire_id)
      
      if (!is_update) {
        # INSERT + RETURNING mar_id
        sql_ins <- sprintf("
          INSERT INTO %s
            (mar_nom, mar_nom_long, mar_reference, mar_datedebut, mar_datefin,
             mar_statut, mar_pre_id, mar_montantmin, mar_montantmax)
          VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9)
          RETURNING mar_id;", fq_mar_table)
        
        new_id_df <- tryCatch(
          DBI::dbGetQuery(
            pool, sql_ins,
            params = list(
              input$mar_nom,
              or_empty(input$mar_nom_long),
              input$mar_reference,
              input$mar_datedebut,
              input$mar_datefin,
              statut,
              titulaire_id,
              if (is.na(input$mar_montantmin)) NULL else input$mar_montantmin,
              if (is.na(input$mar_montantmax)) NULL else input$mar_montantmax
            )
          ),
          error = function(e) { shiny::showNotification(paste("Échec création:", e$message), type = "error", duration = 8); NULL }
        )
        if (is.null(new_id_df) || nrow(new_id_df) == 0) return()
        current_id_val <- as.integer(new_id_df$mar_id[[1]])
        current_id(current_id_val)
        shiny::showNotification(sprintf("Marché créé (ID=%s).", current_id_val), type = "message")
        
      } else {
        # UPDATE
        sql_up <- sprintf("
          UPDATE %s
          SET mar_nom        = $1,
              mar_nom_long   = $2,
              mar_reference  = $3,
              mar_datedebut  = $4,
              mar_datefin    = $5,
              mar_statut     = $6,
              mar_pre_id     = $7,
              mar_montantmin = $8,
              mar_montantmax = $9
          WHERE mar_id       = $10;", fq_mar_table)
        
        ok <- tryCatch(
          DBI::dbExecute(
            pool, sql_up,
            params = list(
              input$mar_nom,
              or_empty(input$mar_nom_long),
              input$mar_reference,
              input$mar_datedebut,
              input$mar_datefin,
              statut,
              titulaire_id,
              if (is.na(input$mar_montantmin)) NULL else input$mar_montantmin,
              if (is.na(input$mar_montantmax)) NULL else input$mar_montantmax,
              current_id_val
            )
          ),
          error = function(e) { shiny::showNotification(paste("Échec mise à jour:", e$message), type = "error", duration = 8); 0L }
        )
        if (ok > 0) shiny::showNotification("Marché mis à jour.", type = "message")
      }
      
      # (multi-titulaires supprimé)
      
      # Recharge + re-sélection
      load_marches()
      df <- filtered_marches()
      if (!is.na(current_id_val)) {
        idx <- which(df$mar_id == current_id_val)
        if (length(idx) == 1) DT::selectRows(tbl_proxy, idx) else DT::selectRows(tbl_proxy, NULL)
      } else {
        DT::selectRows(tbl_proxy, NULL)
      }
    })
    
    # -------- SUPPRESSION --------
    shiny::observeEvent(input$delete, {
      if (is.null(current_id())) {
        shiny::showNotification("Aucun marché sélectionné.", type = "warning"); return()
      }
      mar_id <- as.integer(current_id())
      
      sql_del <- sprintf("DELETE FROM %s WHERE mar_id = $1;", fq_mar_table)
      ok <- tryCatch(DBI::dbExecute(pool, sql_del, params = list(mar_id)),
                     error = function(e) { shiny::showNotification(paste("Échec suppression:", e$message), type = "error"); 0L })
      if (ok > 0) {
        shiny::showNotification("Marché supprimé.", type = "message")
        current_id(NULL)
        load_marches()
        DT::selectRows(tbl_proxy, NULL)
        # Reset formulaire
        shiny::updateTextInput(session, "mar_nom",        value = "")
        shiny::updateTextInput(session, "mar_nom_long",   value = "")
        shiny::updateTextInput(session, "mar_reference",  value = "")
        shiny::updateDateInput(session, "mar_datedebut",  value = today)
        shiny::updateDateInput(session, "mar_datefin",    value = today + 365)
        shiny::updateNumericInput(session, "mar_montantmin", value = NA_real_)
        shiny::updateNumericInput(session, "mar_montantmax", value = NA_real_)
        shiny::updateSelectInput(session, "titulaire_id", selected = NULL)
      }
    })
    
    # -------- RESET (formulaire uniquement) --------
    shiny::observeEvent(input$reset, {
      shiny::updateTextInput(session, "mar_nom",        value = "")
      shiny::updateTextInput(session, "mar_nom_long",   value = "")
      shiny::updateTextInput(session, "mar_reference",  value = "")
      shiny::updateDateInput(session, "mar_datedebut",  value = today)
      shiny::updateDateInput(session, "mar_datefin",    value = today + 365)
      shiny::updateNumericInput(session, "mar_montantmin", value = NA_real_)
      shiny::updateNumericInput(session, "mar_montantmax", value = NA_real_)
      shiny::updateSelectInput(session, "titulaire_id", selected = NULL)
    })
    
    # -------- DEBUG --------
    output$debug <- shiny::renderPrint({
      list(
        mode        = if (is.null(current_id())) "new" else paste("edit", current_id()),
        nom_court   = input$mar_nom,
        nom_long    = input$mar_nom_long,
        ref         = input$mar_reference,
        debut       = input$mar_datedebut,
        fin         = input$mar_datefin,
        montant_min = input$mar_montantmin,
        montant_max = input$mar_montantmax,
        titulaire   = input$titulaire_id,
        statut      = compute_statut(input$mar_datedebut, input$mar_datefin)
      )
    })
  })
}
