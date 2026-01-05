
# modules/mod_import_bpu_progtypes_server.R
mod_import_bpu_progtypes_server <- function(
    id,
    pool,
    schema_sqe    = "sqe",
    can_edit      = TRUE,   # à relier plus tard à l'authentification
    auto_on_valid = TRUE    # TRUE = import auto si conforme et base vide
) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # --- 1) Charger la liste des marchés (mar_id, mar_nom) ---
    shiny::observe({
      req(pool)
      marches <- DBI::dbGetQuery(
        pool,
        sprintf("SELECT mar_id, mar_nom FROM %s.t_marche_mar ORDER BY mar_nom;", schema_sqe)
      )
      shiny::updateSelectInput(
        session, "marche_id",
        choices = stats::setNames(marches$mar_id, marches$mar_nom)
      )
    })
    
    # --- 2) Réactifs globaux ---
    rv <- shiny::reactiveValues(
      valid = FALSE,
      missing_sheets = character(0),
      missing_cols = list(),
      sheets = list(),
      exist  = list(ppt = 0L, prs = 0L, prp = 0L, prr = 0L, run = 0L),
      import = list(done = FALSE, mode = NULL, error = NULL)
    )
    
    # --- 3) Dictionnaire statique (noms de colonnes attendus, insensible à la casse) ---
    expected_cols <- list(
      BPU = c(
        "id_prestation","label_prestation","nom_complet_prestation",
        "nature_prestation","unite_facturation","prix_unitaire_ht",
        "date_debut_validite_prix","date_fin_validite_prix",
        "nom_prestataire","siret_prestataire"
      ),
      cout_run_analytiques = c(
        "run_analytique","prix_unitaire_ht",
        "date_debut_validite_prix","date_fin_validite_prix"
      ),
      programmes_types = c(
        "programme","code_fraction_sandre","nom_fraction_sandre",
        "code_parametre_sandre","nom_parametre_sandre",
        "code_unite_sandre","nom_unite_sandre",
        "analyse_in_situ","code_methode_sandre","nom_methode_sandre",
        "run_analytique","prestataire_analyse","siret_prestataire_analyse",
        "limite_detection_garantie","limite_quantification_garantie","incertitude_garantie",
        "type_incertitude","accreditation","commentaires_parametre"
      )
    )
    
    # --- 4) Helpers ---
    safe_names <- function(df) {
      if (is.null(df)) return(character(0))
      nm <- names(df); if (is.null(nm)) return(character(0)); nm
    }
    
    check_already_in_db <- function(mar_id) {
      # robustifier les retours COUNT(*) -> entiers
      to_int <- function(x) { xi <- suppressWarnings(as.integer(x)); if (is.na(xi)) 0L else xi }
      mar_id_int <- to_int(mar_id)
      
      ppt_count <- to_int(DBI::dbGetQuery(
        pool,
        sprintf("SELECT COUNT(*) AS n FROM %s.t_parametreprogrammetype_ppt WHERE ppt_mar_id = %s;", schema_sqe, mar_id_int)
      )$n)
      
      prs_count <- to_int(DBI::dbGetQuery(
        pool,
        sprintf("SELECT COUNT(*) AS n FROM %s.t_prestation_prs WHERE prs_mar_id = %s;", schema_sqe, mar_id_int)
      )$n)
      
      prp_count <- to_int(DBI::dbGetQuery(
        pool,
        sprintf(
          "SELECT COUNT(*) AS n FROM %s.t_prixunitaireprestation_prp WHERE prp_prs_id IN (SELECT prs_id FROM %s.t_prestation_prs WHERE prs_mar_id = %s);",
          schema_sqe, schema_sqe, mar_id_int
        )
      )$n)
      
      prr_count <- to_int(DBI::dbGetQuery(
        pool,
        sprintf("SELECT COUNT(*) AS n FROM %s.t_prixunitairerunanalytique_prr WHERE prr_mar_id = %s;", schema_sqe, mar_id_int)
      )$n)
      
      run_count <- to_int(DBI::dbGetQuery(
        pool,
        sprintf(
          "SELECT COUNT(DISTINCT r.run_id) AS n FROM %s.t_runanalytique_run r JOIN %s.t_prixunitairerunanalytique_prr prr ON prr.prr_run_id = r.run_id WHERE prr.prr_mar_id = %s;",
          schema_sqe, schema_sqe, mar_id_int
        )
      )$n)
      
      rv$exist <- list(ppt = ppt_count, prs = prs_count, prp = prp_count, prr = prr_count, run = run_count)
    }
    
    purge_market_data <- function(mar_id) {
      conn <- pool::poolCheckout(pool)
      on.exit(pool::poolReturn(conn), add = TRUE)
      ok <- TRUE
      tryCatch({
        pool::dbBegin(conn)
        
        # Mémoriser les RUN liés au marché (via PRR) avant suppression des PRR
        run_ids <- DBI::dbGetQuery(
          conn,
          sprintf("SELECT DISTINCT prr_run_id AS run_id FROM %s.t_prixunitairerunanalytique_prr WHERE prr_mar_id = %s;", schema_sqe, mar_id)
        )$run_id
        
        # 1) Supprimer PRR du marché
        DBI::dbExecute(
          conn,
          sprintf("DELETE FROM %s.t_prixunitairerunanalytique_prr WHERE prr_mar_id = %s;", schema_sqe, mar_id)
        )
        
        # 2) Supprimer RUN spécifiques
        if (length(run_ids) > 0) {
          DBI::dbExecute(
            conn,
            sprintf("DELETE FROM %s.t_runanalytique_run WHERE run_id IN (%s);", schema_sqe, paste(run_ids, collapse = ","))
          )
        }
        
        # 3) Supprimer PRP liés aux PRS du marché
        DBI::dbExecute(
          conn,
          sprintf(
            "DELETE FROM %s.t_prixunitaireprestation_prp WHERE prp_prs_id IN (SELECT prs_id FROM %s.t_prestation_prs WHERE prs_mar_id = %s);",
            schema_sqe, schema_sqe, mar_id
          )
        )
        
        # 4) Supprimer PRS
        DBI::dbExecute(conn, sprintf("DELETE FROM %s.t_prestation_prs WHERE prs_mar_id = %s;", schema_sqe, mar_id))
        
        # 5) Supprimer PPT
        DBI::dbExecute(conn, sprintf("DELETE FROM %s.t_parametreprogrammetype_ppt WHERE ppt_mar_id = %s;", schema_sqe, mar_id))
        
        pool::dbCommit(conn)
      }, error = function(e) {
        pool::dbRollback(conn)
        shiny::showNotification(paste("Erreur suppression données marché :", e$message), type = "error", duration = 8)
        ok <<- FALSE
      })
      ok
    }
    
    do_import <- function(mar_id, path, mode_label = "Chargement") {
      output$log <- shiny::renderText(sprintf("%s du marché %s depuis %s ...", mode_label, mar_id, basename(path)))
      rv$import <- list(done = FALSE, mode = mode_label, error = NULL)
      tryCatch({
        shiny::withProgress(message = paste0(mode_label, " en cours"), value = 0, {
          shiny::incProgress(0.4)
          LibreSQE::func_charge_marche(
            fichier_prog = path,
            connexion    = pool,
            mar_id       = mar_id
          )
          shiny::incProgress(0.6)
        })
        rv$import$done <- TRUE
        shiny::showNotification(paste(mode_label, "terminé avec succès."), type = "message", duration = 5)
        check_already_in_db(mar_id)
      }, error = function(e) {
        rv$import$done <- FALSE
        rv$import$error <- e$message
        shiny::showNotification(paste("Erreur d'import :", e$message), type = "error", duration = 8)
      })
    }
    
    # --- 5) Analyse du fichier + logique auto ---
    shiny::observeEvent(input$analyser, {
      req(input$xlsx$datapath, input$marche_id)
      path   <- input$xlsx$datapath
      mar_id <- input$marche_id
      
      # Feuilles attendues
      expected_sheets <- c("BPU", "cout_run_analytiques", "programmes_types")
      present_sheets  <- readxl::excel_sheets(path)
      rv$missing_sheets <- setdiff(expected_sheets, present_sheets)
      
      # Lecture
      rv$sheets$BPU <- tryCatch(readxl::read_xlsx(path, sheet = "BPU"), error = function(e) NULL)
      rv$sheets$cout_run_analytiques <- tryCatch(readxl::read_xlsx(path, sheet = "cout_run_analytiques"), error = function(e) NULL)
      rv$sheets$programmes_types <- tryCatch(readxl::read_xlsx(path, sheet = "programmes_types"), error = function(e) NULL)
      
      # Vérifier colonnes (case-insensitive)
      rv$missing_cols <- list()
      for (sh in expected_sheets) {
        need <- tolower(expected_cols[[sh]])
        have <- tolower(safe_names(rv$sheets[[sh]]))
        miss <- setdiff(need, have)
        rv$missing_cols[[sh]] <- expected_cols[[sh]][tolower(expected_cols[[sh]]) %in% miss]
      }
      
      rv$valid <- (length(rv$missing_sheets) == 0) &&
        all(vapply(rv$missing_cols, length, integer(1)) == 0)
      
      # Rafraîchir compteurs
      check_already_in_db(mar_id)
      
      # Auto-import si conforme et vide ; sinon modal de confirmation
      if (isTRUE(rv$valid) && isTRUE(can_edit) && isTRUE(auto_on_valid)) {
        total_exist <- sum(unlist(rv$exist))
        if (total_exist == 0) {
          do_import(mar_id, path, mode_label = "Chargement")
        } else {
          shiny::showModal(
            shiny::modalDialog(
              title = "Des données existent déjà pour ce marché",
              easyClose = FALSE,
              footer = shiny::tagList(
                shiny::actionButton(ns("confirm_overwrite"), "Écraser & recharger", class = "btn btn-danger"),
                shiny::modalButton("Annuler")
              ),
              shiny::HTML(sprintf(
                "<p>Le marché <code>%s</code> contient déjà des données :</p>
                 <ul>
                   <li>Programmes types (ppt): <b>%s</b></li>
                   <li>Prestations (prs): <b>%s</b></li>
                   <li>Prix prestations (prp): <b>%s</b></li>
                   <li>Prix runs (prr): <b>%s</b></li>
                   <li>Runs analytiques (run): <b>%s</b></li>
                 </ul>
                 <p>Souhaitez-vous <b>écraser ces données</b> puis <b>recharger</b> le fichier <code>%s</code> ?</p>",
                mar_id, rv$exist$ppt, rv$exist$prs, rv$exist$prp, rv$exist$prr, rv$exist$run, basename(path)
              ))
            )
          )
        }
      }
    })
    
    # --- 6) Confirmation d'écrasement (modal) ---
    shiny::observeEvent(input$confirm_overwrite, {
      req(input$xlsx$datapath, input$marche_id)
      if (!isTRUE(can_edit)) {
        shiny::showNotification("Action non autorisée pour votre profil.", type = "warning")
        return(NULL)
      }
      shiny::removeModal()
      mar_id <- input$marche_id
      path   <- input$xlsx$datapath
      if (!purge_market_data(mar_id)) return(NULL)
      do_import(mar_id, path, mode_label = "Écrasement & rechargement")
    })
    
    # --- 7) Rapport de conformité ---
    output$rapport <- shiny::renderUI({
      req(input$xlsx)
      htmltools::tagList(
        htmltools::tags$h5("Rapport de conformité"),
        htmltools::tags$p(
          htmltools::tags$b("Fichier : "),
          htmltools::tags$code(basename(input$xlsx$name))
        ),
        if (length(rv$missing_sheets) > 0) {
          htmltools::tags$div(class = "text-danger fw-bold",
                              "Feuilles manquantes : ", paste(rv$missing_sheets, collapse = ", ")
          )
        } else {
          htmltools::tags$div(class = "text-success", "Toutes les feuilles attendues sont présentes.")
        },
        htmltools::tags$hr(),
        htmltools::tags$h6("Colonnes manquantes par onglet :"),
        htmltools::tags$ul(
          lapply(names(rv$missing_cols), function(nm) {
            miss <- rv$missing_cols[[nm]]
            htmltools::tags$li(
              htmltools::tags$b(nm), ": ",
              if (length(miss) == 0) {
                htmltools::tags$span(class = "text-success", "OK")
              } else {
                htmltools::tags$span(class = "text-danger", paste(miss, collapse = ", "))
              }
            )
          })
        ),
        htmltools::tags$hr(),
        htmltools::tags$h6("Présence de données en base (marché sélectionné) :"),
        htmltools::tags$p(sprintf(
          "Programmes types (ppt): %s | Prestations (prs): %s | Prix prestations (prp): %s | Prix runs (prr): %s | Runs analytiques (run): %s",
          rv$exist$ppt, rv$exist$prs, rv$exist$prp, rv$exist$prr, rv$exist$run
        )),
        if (isTRUE(rv$valid) && isTRUE(can_edit)) {
          if (isTRUE(rv$import$done)) {
            htmltools::tags$div(
              class = "alert alert-success",
              htmltools::tags$b(rv$import$mode), " : les données ont bien été importées."
            )
          } else if (!is.null(rv$import$error)) {
            htmltools::tags$div(
              class = "alert alert-danger",
              "Erreur lors de l'import : ", rv$import$error
            )
          } else {
            htmltools::tags$div(
              class = "alert alert-info",
              "Fichier conforme. ",
              if (sum(unlist(rv$exist)) == 0) "Aucune donnée en base (import automatique activé)."
              else "Des données existent : confirmez l'écrasement ou utilisez le bouton dédié."
            )
          }
        } else if (!isTRUE(rv$valid)) {
          htmltools::tags$div(
            class = "alert alert-danger",
            "Fichier non conforme : corrigez les feuilles/colonnes manquantes avant chargement."
          )
        } else {
          htmltools::tags$div(
            class = "alert alert-warning",
            "Actions d'import désactivées (can_edit = FALSE)."
          )
        }
      )
    })
    
    # --- 8) Prévisualisations ---
    output$prev_bpu <- DT::renderDT({
      req(rv$sheets$BPU)
      DT::datatable(rv$sheets$BPU, options = list(scrollX = TRUE))
    })
    
    output$prev_runs <- DT::renderDT({
      req(rv$sheets$cout_run_analytiques)
      DT::datatable(rv$sheets$cout_run_analytiques, options = list(scrollX = TRUE))
    })
    
    output$prev_progtypes <- DT::renderDT({
      req(rv$sheets$programmes_types)
      DT::datatable(rv$sheets$programmes_types, options = list(scrollX = TRUE))
    })
    
    # --- 9) Bouton manuel : Charger ---
    shiny::observeEvent(input$charger, {
      req(rv$valid, input$xlsx$datapath, input$marche_id)
      if (!isTRUE(can_edit)) {
        shiny::showNotification("Action non autorisée pour votre profil.", type = "warning")
        return(NULL)
      }
      if (sum(unlist(rv$exist)) > 0) {
        shiny::showNotification("Des données existent déjà pour ce marché. Utilisez 'Écraser & recharger'.", type = "warning")
        return(NULL)
      }
      do_import(input$marche_id, input$xlsx$datapath, mode_label = "Chargement")
    })
    
    # --- 10) Bouton manuel : Écraser & recharger ---
    shiny::observeEvent(input$ecraser, {
      req(rv$valid, input$xlsx$datapath, input$marche_id)
      if (!isTRUE(can_edit)) {
        shiny::showNotification("Action non autorisée pour votre profil.", type = "warning")
        return(NULL)
      }
      if (!purge_market_data(input$marche_id)) return(NULL)
      do_import(input$marche_id, input$xlsx$datapath, mode_label = "Écrasement & rechargement")
    })
  })
}
