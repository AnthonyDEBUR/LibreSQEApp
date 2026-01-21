
# modules/mod_perimetres_server.R
# Module serveur pour gérer la table refer.tr_perimetre_per
# - Clé primaire texte : per_nom
# - Colonnes optionnelles gérées dynamiquement : per_description, per_entite_gestionaire
# - Transactions via pool::poolWithTransaction()
# - Requêtes paramétrées via DBI::sqlInterpolate() (avec la connexion de transaction 'con')
# - Gestion des erreurs avec tryCatch + showNotification
# - Édition inline autorisée uniquement hors clé ; renommage via modal
# - Rafraîchissement du tableau via DT::dataTableProxy + DT::replaceData
# - Ré-application de la sélection après update/insert
# - Rétro-compatible DT (pas d'argument 'server' dans renderDT, reloadData() si dispo)

mod_perimetres_server <- function(id,
                                  pool,
                                  schema = "refer",
                                  table  = "tr_perimetre_per",
                                  id_col = "per_nom") {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # --- Helpers
    `%||%` <- function(x, y) if (is.null(x) || is.na(x)) y else x
    trim   <- function(x) if (is.null(x)) "" else trimws(as.character(x))
    notify_error <- function(e, prefix = "Erreur") {
      msg <- if (inherits(e, "condition")) e$message else as.character(e)
      shiny::showNotification(paste(prefix, ":", msg), type = "error", duration = 7)
    }
    notify_ok <- function(msg) shiny::showNotification(msg, type = "message", duration = 5)
    
    # Transactions compatibles 'pool'
    tx <- function(fun) {
      tryCatch(pool::poolWithTransaction(pool, fun),
               error = function(e) { notify_error(e); FALSE })
    }
    
    # --- État non réactif pour les colonnes (évite "Can't access reactive value ...")
    cols_available <- NULL  # ex. c("per_nom", "per_description", "per_entite_gestionaire")
    cols_editable  <- NULL  # colonnes éditables (toutes sauf la clé)
    
    # --- État réactif pour les données affichées
    rv <- shiny::reactiveValues(data = NULL)
    
    # Détection des colonnes disponibles dans la table
    detect_columns <- function(df) {
      present    <- names(df)
      # NB : orthographe exacte de ta base : 'per_entite_gestionaire' (1 seul 'n')
      candidates <- c(id_col, "per_description", "per_entite_gestionaire")
      available  <- intersect(candidates, present)
      if (!(id_col %in% available)) {
        stop(sprintf("La clé primaire '%s' est introuvable dans %s.%s.", id_col, schema, table))
      }
      editable <- setdiff(available, id_col)
      list(available = available, editable = editable)
    }
    
    # Chargement / rechargement des données
    load_data <- function() {
      tryCatch({
        df <- DBI::dbReadTable(pool, DBI::Id(schema = schema, table = table))
        info <- detect_columns(df)
        cols_available <<- info$available
        cols_editable  <<- info$editable
        df  <- df[, cols_available, drop = FALSE]
        # nécessite dplyr chargé par l'app principale
        df  <- dplyr::arrange(df, .data[[id_col]])
        rv$data <- df
      }, error = function(e) {
        notify_error(e, "Erreur de chargement des périmètres")
      })
    }
    load_data()
    
    # --- Tableau DT (édition inline autorisée uniquement hors clé)
    output$dt_per <- DT::renderDT({
      df <- rv$data
      if (is.null(df)) return(DT::datatable(data.frame()))
      disable_idx <- integer(0)
      key_idx <- which(names(df) == id_col)
      if (length(key_idx) == 1) disable_idx <- c(disable_idx, key_idx)
      
      DT::datatable(
        df,
        selection = "single",
        rownames  = FALSE,
        editable  = list(target = "cell", disable = list(columns = disable_idx)),
        options   = list(pageLength = 10, scrollX = TRUE)
        # Pas d'argument 'server' ici pour compatibilité avec anciennes versions de DT
      )
    })
    
    # --- Proxy DT (ID namespacé)
    proxy <- DT::dataTableProxy(ns("dt_per"))
    
    # Rafraîchit l'affichage sans recréer le tableau
    # - keep_page : conserver la pagination
    # - select_pk : re-sélectionner la ligne correspondant à cette clé
    refresh_dt <- function(keep_page = TRUE, select_pk = NULL) {
      load_data()
      df <- as.data.frame(rv$data)  # robustesse : data.frame simple
      
      # Effacer la sélection uniquement si on ne va pas re-sélectionner
      clear_sel <- is.null(select_pk)
      
      DT::replaceData(proxy, df,
                      resetPaging    = !keep_page,
                      clearSelection = clear_sel,
                      rownames       = FALSE)
      
      # Re-sélection par valeur de clé, si demandé
      if (!is.null(select_pk) && id_col %in% names(df)) {
        idx <- which(df[[id_col]] == select_pk)
        if (length(idx) >= 1) {
          DT::selectRows(proxy, idx[1])
        }
      }
      
      # Si la version de DT expose reloadData(), on force un redraw
      if ("reloadData" %in% getNamespaceExports("DT")) {
        DT::reloadData(proxy, resetPaging = !keep_page)
      }
    }
    
    # --- Édition inline d'une cellule (seulement si colonne éditable)
    shiny::observeEvent(input$dt_per_cell_edit, {
      info <- input$dt_per_cell_edit
      df   <- rv$data
      if (is.null(df) || nrow(df) == 0) return(invisible(NULL))
      
      col_name <- names(df)[info$col]
      if (!(col_name %in% cols_editable)) {
        shiny::showNotification("Cette colonne n'est pas éditable.", type = "warning")
        return(invisible(NULL))
      }
      new_val <- trim(info$value)
      pk_val  <- df[info$row, id_col, drop = TRUE]
      
      sql <- sprintf("UPDATE %s.%s SET %s = ?val WHERE %s = ?pk;", schema, table, col_name, id_col)
      
      ok <- tx(function(con) {
        qry <- DBI::sqlInterpolate(con, sql, val = new_val, pk = pk_val)
        DBI::dbExecute(con, qry)
      })
      if (isTRUE(ok) != FALSE) {
        # Re-sélectionner la même ligne après refresh
        refresh_dt(keep_page = TRUE, select_pk = pk_val)
        notify_ok("Valeur mise à jour.")
      }
    })
    
    # --- Modal d'édition (inputs dynamiques selon les colonnes existantes)
    shiny::observeEvent(input$btn_edit_row, {
      sel <- input$dt_per_rows_selected
      if (length(sel) != 1) {
        shiny::showNotification("Sélectionnez une seule ligne.", type = "warning")
        return(invisible(NULL))
      }
      row <- rv$data[sel, , drop = FALSE]
      
      inputs <- list(
        shiny::textInput(ns("edit_nom"), "Nom (clé primaire)", trim(row[[id_col]]) %||% "")
      )
      if ("per_description" %in% cols_available) {
        inputs <- c(inputs, list(
          shiny::textAreaInput(ns("edit_desc"), "Description", row$per_description %||% "", rows = 5)
        ))
      }
      if ("per_entite_gestionaire" %in% cols_available) {
        inputs <- c(inputs, list(
          shiny::textInput(ns("edit_gest"), "Entité gestionaire", row$per_entite_gestionaire %||% "")
        ))
      }
      
      shiny::showModal(
        shiny::modalDialog(
          title = "Éditer un périmètre",
          do.call(shiny::tagList, inputs),
          footer = shiny::tagList(
            shiny::modalButton("Annuler"),
            shiny::actionButton(ns("confirm_edit"), "Enregistrer", class = "btn-primary")
          ),
          easyClose = TRUE
        )
      )
      session$userData$edit_old_pk <- trim(row[[id_col]])
    })
    
    # --- Confirmation d'édition (renommage + MAJ des colonnes existantes)
    shiny::observeEvent(input$confirm_edit, {
      shiny::removeModal()
      old_pk <- session$userData$edit_old_pk
      new_pk <- trim(input$edit_nom)
      if (nchar(new_pk) == 0) {
        shiny::showNotification("Le nom (clé primaire) ne peut pas être vide.", type = "error")
        return(invisible(NULL))
      }
      
      # valeurs à écrire en fonction des colonnes présentes
      vals <- list()
      if ("per_description" %in% cols_available) {
        vals$per_description <- trim(input$edit_desc %||% "")
      }
      if ("per_entite_gestionaire" %in% cols_available) {
        vals$per_entite_gestionaire <- trim(input$edit_gest %||% "")
      }
      
      ok <- tx(function(con) {
        # Vérifier unicité si renommage
        if (!identical(new_pk, old_pk)) {
          exists_sql <- sprintf("SELECT 1 FROM %s.%s WHERE %s = ?pk LIMIT 1;", schema, table, id_col)
          exists_qry <- DBI::sqlInterpolate(con, exists_sql, pk = new_pk)
          exists     <- DBI::dbGetQuery(con, exists_qry)
          if (nrow(exists) > 0) stop(sprintf("Un périmètre nommé '%s' existe déjà.", new_pk))
        }
        
        # SET dynamique
        set_parts <- c(sprintf("%s = ?new_pk", id_col))
        bind_list <- list(new_pk = new_pk, old_pk = old_pk)
        if (length(vals)) {
          for (nm in names(vals)) {
            set_parts <- c(set_parts, sprintf("%s = ?%s", nm, nm))
            bind_list[[nm]] <- vals[[nm]]
          }
        }
        upd_sql <- sprintf("UPDATE %s.%s SET %s WHERE %s = ?old_pk;",
                           schema, table, paste(set_parts, collapse = ", "), id_col)
        
        # IMPORTANT : 'sql' (minuscule) pour sqlInterpolate
        upd_qry <- do.call(DBI::sqlInterpolate, c(list(conn = con, sql = upd_sql), bind_list))
        DBI::dbExecute(con, upd_qry)
      })
      
      if (isTRUE(ok) != FALSE) {
        # Re-sélectionner la ligne en fonction du nouveau PK
        refresh_dt(keep_page = TRUE, select_pk = new_pk)
        notify_ok("Périmètre mis à jour.")
      }
    })
    
    # --- Modal d'ajout (inputs dynamiques)
    shiny::observeEvent(input$btn_add_row, {
      inputs <- list(shiny::textInput(ns("add_nom"), "Nom (clé primaire)"))
      if ("per_description" %in% cols_available) {
        inputs <- c(inputs, list(shiny::textAreaInput(ns("add_desc"), "Description", rows = 5)))
      }
      if ("per_entite_gestionaire" %in% cols_available) {
        inputs <- c(inputs, list(shiny::textInput(ns("add_gest"), "Entité gestionaire")))
      }
      
      shiny::showModal(
        shiny::modalDialog(
          title = "Ajouter un périmètre",
          do.call(shiny::tagList, inputs),
          footer = shiny::tagList(
            shiny::modalButton("Annuler"),
            shiny::actionButton(ns("confirm_add"), "Ajouter", class = "btn-primary")
          ),
          easyClose = TRUE
        )
      )
    })
    
    # --- Confirmation d'ajout (INSERT dynamique + contrôle d'unicité)
    shiny::observeEvent(input$confirm_add, {
      new_pk <- trim(input$add_nom)
      if (nchar(new_pk) == 0) {
        shiny::showNotification("Le nom (clé primaire) ne peut pas être vide.", type = "error")
        return(invisible(NULL))
      }
      
      vals <- list()
      if ("per_description" %in% cols_available) {
        vals$per_description <- trim(input$add_desc %||% "")
      }
      if ("per_entite_gestionaire" %in% cols_available) {
        vals$per_entite_gestionaire <- trim(input$add_gest %||% "")
      }
      
      ok <- tx(function(con) {
        # Unicité
        exists_sql <- sprintf("SELECT 1 FROM %s.%s WHERE %s = ?pk LIMIT 1;", schema, table, id_col)
        exists_qry <- DBI::sqlInterpolate(con, exists_sql, pk = new_pk)
        exists     <- DBI::dbGetQuery(con, exists_qry)
        if (nrow(exists) > 0) stop(sprintf("Un périmètre nommé '%s' existe déjà.", new_pk))
        
        # INSERT dynamique
        cols <- c(id_col, names(vals))
        qms  <- paste(sprintf("?%s", c("pk", names(vals))), collapse = ", ")
        ins_sql <- sprintf("INSERT INTO %s.%s (%s) VALUES (%s);",
                           schema, table, paste(cols, collapse = ", "), qms)
        bind_list <- c(list(pk = new_pk), vals)
        
        # IMPORTANT : 'sql' (minuscule) pour sqlInterpolate
        ins_qry   <- do.call(DBI::sqlInterpolate, c(list(conn = con, sql = ins_sql), bind_list))
        DBI::dbExecute(con, ins_qry)
      })
      
      if (isTRUE(ok) != FALSE) {
        shiny::removeModal()
        # Sélectionner la nouvelle ligne
        refresh_dt(keep_page = TRUE, select_pk = new_pk)
        notify_ok("Nouveau périmètre ajouté.")
      }
    })
  })
}
