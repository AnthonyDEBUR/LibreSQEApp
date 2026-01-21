
mod_import_prog_annuelle_server <- function(id, pool, schema_sqe = "sqe") {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # --- État interne réactif --------------------------------------------------
    rv <- reactiveValues(
      prog = NULL,           # data.frame "programme_annuel"
      cal  = NULL,           # data.frame "calendrier"
      conforme = FALSE,      # bool conformité
      rapport = NULL,        # texte du rapport
      etat_base = NULL,      # data.frame des compteurs en base
      file_path = NULL       # chemin du xlsx chargé
    )
    
    # --- Utilitaires -----------------------------------------------------------
    # Texte formaté + safe pour NULL
    .fmt <- function(x) if (is.null(x)) "<NULL>" else as.character(x)
    
    # Concatenation "belle" d'un vecteur de noms
    .join <- function(x) if (length(x)) paste(x, collapse = ", ") else "—"
    
    # Nombre de lignes/têtes pour le rapport
    .summary_table <- function(df, label) {
      if (is.null(df)) return(sprintf("%s : 0 ligne (non chargé)", label))
      paste0(
        sprintf("%s : %s ligne(s)", label, nrow(df)),
        if (nrow(df) > 0) {
          hdr <- utils::capture.output(utils::head(df, 5))
          paste0("\nAperçu (5 premières lignes) :\n", paste(hdr, collapse = "\n"))
        } else ""
      )
    }
    
    # --- Chargement de la liste des marchés -----------------------------------
    observe({
      conn <- pool::poolCheckout(pool); on.exit(pool::poolReturn(conn))
      sql <- glue::glue_sql("
        SELECT mar_id, mar_nom
        FROM {`schema_sqe`}.t_marche_mar
        ORDER BY mar_id
      ", .con = conn)
      
      res <- DBI::dbGetQuery(conn, sql)
      
      updateSelectInput(
        session, "select_marche",
        choices = setNames(res$mar_id, paste(res$mar_id, res$mar_nom, sep = " - "))
      )
    })
    
    # --- Upload du fichier -----------------------------------------------------
    observeEvent(input$file_xlsx, {
      
      # Afficher uniquement le bouton "Vérifier"
      shinyjs::show("zone_btn_verifier")
      shinyjs::hide("zone_btn_importer")
      shinyjs::hide("zone_btn_ecraser")
      
      rv$file_path <- input$file_xlsx$datapath
      
      # Lecture brute (on laisse readxl inférer, tu peux forcer si besoin)
      rv$prog <- readxl::read_xlsx(rv$file_path, sheet = "programme_annuel")
      rv$cal  <- readxl::read_xlsx(rv$file_path, sheet = "calendrier")
      
      # Cast des colonnes mois -> numeric (si présentes)
      mois <- c("janvier","fevrier","mars","avril","mai","juin",
                "juillet","aout","septembre","octobre","novembre","decembre")
      for (m in mois) {
        if (m %in% names(rv$cal)) {
          rv$cal[[m]] <- suppressWarnings(as.numeric(rv$cal[[m]]))
        }
      }
      
      # Prévisualisations
      output$dt_prog_annuel <- DT::renderDT(rv$prog)
      output$dt_calendrier  <- DT::renderDT(rv$cal)
      
      # Pré-rapport (info fichier)
      rv$rapport <- paste(
        sprintf("Fichier importé : %s", basename(.fmt(rv$file_path))),
        .summary_table(rv$prog, "programme_annuel"),
        .summary_table(rv$cal , "calendrier"),
        sep = "\n\n"
      )
      output$rapport_text <- renderText(rv$rapport)
    })
    
    # --- Vérification stricte --------------------------------------------------
    observeEvent(input$btn_verifier, {
      
      # On force la structure
      req(rv$prog, rv$cal)
      
      # On (re)cache les boutons d'action finaux, ils seront affichés après vérif
      shinyjs::hide("zone_btn_importer")
      shinyjs::hide("zone_btn_ecraser")
      
      # 1) Checks colonnes obligatoires
      
      # Colonnes autorisées selon dictionnaire_donnees
      REQ_PROG_OBL <- c("UG","perimetre_facturation","code_sandre_station",
                        "nom_station","type_station")
      
      REQ_PROG_OPT <- c("code_interne_station","commentaires","code_tmp")
      
      REQ_CAL_OBL  <- c("type_station","programme","janvier","fevrier","mars",
                        "avril","mai","juin","juillet","aout","septembre",
                        "octobre","novembre","decembre","rattachement_bdc", "stratégie")
      
      REQ_CAL_OPT  <- c("préleveur","total nb presta", "remarques")
      
      # Vérifications
      missing_prog <- setdiff(REQ_PROG_OBL, names(rv$prog))
      missing_cal  <- setdiff(REQ_CAL_OBL, names(rv$cal))
      
      # Colonnes superflues = colonnes non répertoriées du tout (ni obligatoires, ni facultatives)
      extra_prog <- setdiff(names(rv$prog), c(REQ_PROG_OBL, REQ_PROG_OPT))
      extra_cal  <- setdiff(names(rv$cal),  c(REQ_CAL_OBL , REQ_CAL_OPT ))
      
      
      issues <- c()
      if (length(missing_prog)) issues <- c(
        issues, paste0("programme_annuel - colonnes manquantes : ", .join(missing_prog))
      )
      if (length(extra_prog)) issues <- c(
        issues, paste0("programme_annuel - colonnes non attendues : ", .join(extra_prog))
      )
      if (length(missing_cal)) issues <- c(
        issues, paste0("calendrier - colonnes manquantes : ", .join(missing_cal))
      )
      if (length(extra_cal)) issues <- c(
        issues, paste0("calendrier - colonnes non attendues : ", .join(extra_cal))
      )
      
      # 2) (Optionnel) checks de types simples sur les mois (numériques)
      mois <- c("janvier","fevrier","mars","avril","mai","juin",
                "juillet","aout","septembre","octobre","novembre","decembre")
      mois_non_numeriques <- mois[mois %in% names(rv$cal) & !vapply(rv$cal[mois], is.numeric, TRUE)]
      if (length(mois_non_numeriques)) {
        issues <- c(issues, paste0(
          "calendrier - colonnes mois non numériques : ", .join(mois_non_numeriques)
        ))
      }
      
      # Bilan conformité
      rv$conforme <- length(issues) == 0
      
      # Construction du rapport détaillé (on conserve TOUT ce qui aide au debug)
      rapport_entete <- paste(
        sprintf("Fichier : %s", basename(.fmt(rv$file_path))),
        sprintf("Marché : %s", .fmt(input$select_marche)),
        sprintf("Année  : %s", .fmt(input$select_annee)),
        sep = "\n"
      )
      
      rapport_contenu <- if (rv$conforme) {
        "✔️ Conforme"
      } else {
        paste0(
          "❌ Non conforme :\n - ",
          paste(issues, collapse = "\n - ")
        )
      }
      
      # Info datasets (nombre de lignes + tête)
      rapport_datasets <- paste(
        .summary_table(rv$prog, "programme_annuel"),
        .summary_table(rv$cal , "calendrier"),
        sep = "\n\n"
      )
      
      rv$rapport <- paste(rapport_entete, rapport_contenu, rapport_datasets, sep = "\n\n")
      output$rapport_text <- renderText(rv$rapport)
      
      if (!rv$conforme) return()
      
      # --- Vérifications en base & résumé explicite ---------------------------
      conn <- pool::poolCheckout(pool); on.exit(pool::poolReturn(conn))
      
      an   <- as.integer(input$select_annee)
      mar  <- as.integer(input$select_marche)
      date_debut <- as.Date(sprintf("%04d-01-01", an))
      date_fin   <- as.Date(sprintf("%04d-01-01", an + 1))
      
      # CALENDRIER (comptage)
      sql_calendrier <- glue::glue_sql("
        SELECT COUNT(*) AS nb
        FROM {`schema_sqe`}.t_calendrierprog_cal
        WHERE cal_mar_id = {mar}
          AND cal_date   >= {date_debut}
          AND cal_date   <  {date_fin}
      ", .con = conn)
      nb_calendrier <- as.numeric(DBI::dbGetQuery(conn, sql_calendrier)$nb)
      
      # PROGRAMMATION ANNUELLE (comptage)
      sql_prog <- glue::glue_sql("
        SELECT COUNT(*) AS nb
        FROM {`schema_sqe`}.t_progannuelle_pga
        WHERE pga_mar_id       = {mar}
          AND pga_cal_refannee = {as.character(an)}
      ", .con = conn)
      nb_prog_annuelle <- as.numeric(DBI::dbGetQuery(conn, sql_prog)$nb)
      
      # BONS DE COMMANDE existants (distincts)
      sql_bco <- glue::glue_sql("
        SELECT DISTINCT bcp_bco_id
        FROM {`schema_sqe`}.t_boncommande_pgm_bcp
        JOIN {`schema_sqe`}.t_boncommande_bco ON bco_id = bcp_bco_id
        WHERE bco_mar_id     = {mar}
          AND bcp_dateinterv >= {date_debut}
          AND bcp_dateinterv <  {date_fin}
      ", .con = conn)
      bco_ids <- unique(DBI::dbGetQuery(conn, sql_bco)$bcp_bco_id)
      nb_bons_commande <- as.numeric(length(bco_ids))
      
      rv$etat_base <- data.frame(
        objet = c("Calendrier (CAL)", "Programmation annuelle (PGA)", "Bons de commande (BCO)"),
        nb    = c(nb_calendrier, nb_prog_annuelle, nb_bons_commande),
        stringsAsFactors = FALSE
      )
      output$dt_etat_base <- DT::renderDT(rv$etat_base)
      
      # Ajout au rapport (lisible, sans abréviations opaques)
      
      resume_base <- paste0(
        "État en base pour Marché ", mar, " / Année ", an, " :\n",
        sprintf(" - %.0f enregistrements dans t_calendrierprog_cal (période %s → %s)",
                nb_calendrier, as.character(date_debut), as.character(date_fin)), "\n",
        sprintf(" - %.0f enregistrements dans t_progannuelle_pga (ref année = %s)",
                nb_prog_annuelle, an), "\n",
        sprintf(" - %.0f bons de commande (distincts) sur la période", nb_bons_commande)
      )
      
      
      rv$rapport <- paste(rv$rapport, resume_base, sep = "\n\n")
      output$rapport_text <- renderText(rv$rapport)
      
      # Affichage du bon bouton en fonction de l'état base
      if (sum(rv$etat_base$nb) == 0) {
        shinyjs::show("zone_btn_importer")   # base vide -> import direct
      } else {
        shinyjs::show("zone_btn_ecraser")    # base non vide -> action risquée
      }
    })
    
    # --- Import simple ---------------------------------------------------------
    observeEvent(input$btn_importer, {
      shinyjs::hide("zone_btn_importer")
      shinyjs::hide("zone_btn_ecraser")
      
      LibreSQE::func_charge_prog_annuelle(
        fichier_prog = rv$file_path,
        connexion    = pool,
        mar_id       = as.integer(input$select_marche),
        annee        = as.integer(input$select_annee),
        frequence_bdc= input$select_freq_bdc,
        prefixe      = input$prefixe_bdc
      )
      
      showNotification("Import OK")
    })
    
    # --- Écraser + importer ----------------------------------------------------
    observeEvent(input$btn_ecraser_et_importer, {
      shinyjs::hide("zone_btn_importer")
      shinyjs::hide("zone_btn_ecraser")
      
      
      observeEvent(input$btn_ecraser_et_importer, {
        
        shinyjs::hide(ns("btn_importer"))
        shinyjs::hide(ns("btn_ecraser_et_importer"))
        shinyjs::disable(ns("btn_verifier"))
        
        withProgress(message="Écrasement en cours…", value=0, {
          conn <- pool::poolCheckout(pool); on.exit(pool::poolReturn(conn), add = TRUE)
          
          mar <- as.integer(input$select_marche)
          an  <- as.integer(input$select_annee)
          start_dt <- as.Date(sprintf("%04d-01-01", an))
          end_dt   <- as.Date(sprintf("%04d-01-01", an+1))
          
          DBI::dbBegin(conn)
          
          tryCatch({
            
            # Étape 0 : liste BCO
            incProgress(0.10, detail="Repérage des BCO…")
            sql_bco <- glue::glue_sql("
            SELECT DISTINCT bcp.bcp_bco_id AS bco_id
            FROM {`schema_sqe`}.t_boncommande_pgm_bcp bcp
            JOIN {`schema_sqe`}.t_boncommande_bco bco
                 ON bco.bco_id = bcp.bcp_bco_id
            WHERE bco.bco_mar_id = {mar}
              AND bcp.bcp_dateinterv >= {start_dt}
              AND bcp.bcp_dateinterv <  {end_dt};
          ", .con = conn)
            bco_ids <- DBI::dbGetQuery(conn, sql_bco)$bco_id |> unique()
            
            # Étape 1 : BCP
            if (length(bco_ids)>0) {
              incProgress(0.25, detail="Suppression BCP…")
              sql1 <- glue::glue_sql("
              DELETE FROM {`schema_sqe`}.t_boncommande_pgm_bcp
              WHERE bcp_bco_id IN ({bco_ids*});
            ", .con = conn)
              DBI::dbExecute(conn, sql1)
            }
            
            # Étape 2 : REA
            if (length(bco_ids)>0) {
              incProgress(0.40, detail="Suppression REA…")
              sql2 <- glue::glue_sql("
              DELETE FROM {`schema_sqe`}.t_resultatanalyse_rea
              WHERE res_bco_id IN ({bco_ids*});
            ", .con = conn)
              DBI::dbExecute(conn, sql2)
            }
            
            # Étape 3 : CAL
            incProgress(0.55, detail="Suppression CAL…")
            sql3 <- glue::glue_sql("
            DELETE FROM {`schema_sqe`}.t_calendrierprog_cal
            WHERE cal_mar_id = {mar}
              AND cal_date   >= {start_dt}
              AND cal_date   <  {end_dt};
          ", .con = conn)
            DBI::dbExecute(conn, sql3)
            
            # Étape 4 : PGA
            incProgress(0.70, detail="Suppression PGA…")
            sql4 <- glue::glue_sql("
            DELETE FROM {`schema_sqe`}.t_progannuelle_pga
            WHERE pga_mar_id = {mar}
              AND pga_cal_refannee = {as.character(an)};
          ", .con = conn)
            DBI::dbExecute(conn, sql4)
            
            # Étape 5 : RESULTAT_RES
            if (length(bco_ids)>0) {
              incProgress(0.82, detail="Suppression RESULTAT_RES…")
              sql5 <- glue::glue_sql("
              DELETE FROM {`schema_sqe`}.t_resultat_res
              WHERE res_bco_id IN ({bco_ids*});
            ", .con = conn)
              DBI::dbExecute(conn, sql5)
            }
            
            # Étape 6 : BCO
            if (length(bco_ids)>0) {
              incProgress(0.90, detail="Suppression BCO…")
              sql6 <- glue::glue_sql("
              DELETE FROM {`schema_sqe`}.t_boncommande_bco
              WHERE bco_id IN ({bco_ids*});
            ", .con = conn)
              DBI::dbExecute(conn, sql6)
            }
            
            # Import final
            incProgress(0.95, detail="Import…")
            LibreSQE::func_charge_prog_annuelle(
              fichier_prog   = rv$file_path,
              connexion      = pool,
              mar_id         = mar,
              annee          = an,
              frequence_bdc  = input$select_freq_bdc,
              prefixe        = input$prefixe_bdc
            )
            
            DBI::dbCommit(conn)
            incProgress(1)
            showNotification("Écrasement + import OK.")
            
          }, error = function(e) {
            DBI::dbRollback(conn)
            showNotification(paste("Erreur :", e$message), type="error")
          })
        })
      })
      
      
      showNotification("Écrasement + import OK")
    })
    
    # --- Téléchargement du rapport --------------------------------------------
    output$btn_dl_rapport <- downloadHandler(
      filename = function() {
        sprintf("rapport_%s_%s.txt", input$select_marche, input$select_annee)
      },
      content = function(file) {
        writeLines(rv$rapport %||% "Aucun rapport disponible.", file, useBytes = TRUE)
      }
    )
  })
}

# petit utilitaire pour éviter un NULL non géré
`%||%` <- function(x, y) if (is.null(x)) y else x
