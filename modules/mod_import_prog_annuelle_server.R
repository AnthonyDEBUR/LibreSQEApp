
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
      rv$prog_types <- readxl::read_xlsx(rv$file_path, sheet = "programmes_types")
      rv$bpu <- readxl::read_xlsx(rv$file_path, sheet = "BPU")
      rv$programmes_sans_analyses<- readxl::read_xlsx(rv$file_path, sheet = "programmes_sans_analyses")
      
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
      
      
      # Début : création du vecteur de problèmes
      issues <- c()
      
      # --- Vérification cohérence PROGRAMMES ------------------------------------
      
      # Programmes du calendrier
      prog_cal <- unique(trimws(rv$cal$programme))
      
      # programmes sans analyses
      prog_sans_analyses<-unique(rv$programmes_sans_analyses$programme)

      
      # Programmes dans programmes_types
      if (!is.null(rv$prog_types) && "programme" %in% names(rv$prog_types)) {
        prog_types <- unique(trimws(rv$prog_types$programme))
      } else {
        prog_types <- character(0)
      }
      
      # Programmes dans BPU
      if (!is.null(rv$bpu) && "label_prestation" %in% names(rv$bpu)) {
        prog_bpu <- unique(trimws(rv$bpu$label_prestation))
      } else {
        prog_bpu <- character(0)
      }
      
      # 1) Programmes du calendrier absents de programmes_types
      missing_in_types <- setdiff(prog_cal, prog_types)
      missing_in_types <- setdiff(missing_in_types, prog_sans_analyses)
      
      
      if (length(missing_in_types) > 0) {
        issues <- c(
          issues,
          paste0(
            "Le(s) programme(s) suivant(s) sont présents dans l’onglet 'calendrier' ",
            "mais absents de 'programmes_types' : ",
            paste(missing_in_types, collapse = ", ")
          )
        )
      }
      
      # 2) Programmes du calendrier absents du BPU (label_prestation)
      missing_in_bpu <- setdiff(prog_cal, prog_bpu)
      missing_in_bpu <- setdiff(missing_in_bpu, prog_sans_analyses)
      
      if (length(missing_in_bpu) > 0) {
        issues <- c(
          issues,
          paste0(
            "Le(s) programme(s) suivant(s) ne figurent pas dans la colonne ",
            "'label_prestation' du BPU : ",
            paste(missing_in_bpu, collapse = ", ")
          )
        )
      }
      

      
      # --- Vérification stations manquantes dans le référentiel ----------------------
      
      # Récupération des codes internes station
      prog_local <- rv$prog
      prog_local$code_interne_station <- trimws(prog_local$code_interne_station)
      need_fallback <- is.na(prog_local$code_interne_station) | prog_local$code_interne_station == ""
      prog_local$code_interne_station[need_fallback] <- trimws(prog_local$code_sandre_station[need_fallback])
      
      # Neutralisation placeholders
      placeholders <- c("sans_objet","sans objet","transport","TRANSPORT","", NA)
      prog_local$code_interne_station[
        prog_local$code_interne_station %in% placeholders
      ] <- NA_character_
      
      sta_file <- unique(na.omit(prog_local$code_interne_station))
      
      # Récupération référentiel des stations
      conn <- pool::poolCheckout(pool); on.exit(pool::poolReturn(conn))
      sta_ref <- DBI::dbGetQuery(
        conn,
        glue::glue_sql("SELECT stm_cdstationmesureinterne FROM refer.tr_stationmesure_stm;", .con = conn)
      )
      
      sta_missing <- setdiff(sta_file, sta_ref$stm_cdstationmesureinterne)
      
      if (length(sta_missing)) {
        issues <- c(
          issues,
          paste0(
            "Stations INTERNES absentes du référentiel (refer.tr_stationmesure_stm) : ",
            paste(sta_missing, collapse=" ; ")
          )
        )
      }
      
      
      
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
      
      # --- Vérification cohérence TYPE_STATION programme_annuel → calendrier ----
      
      # Types présents dans programme_annuel
      types_prog <- unique(trimws(rv$prog$type_station))
      
      # Types présents dans calendrier
      types_cal <- unique(trimws(rv$cal$type_station))
      
      # Tous les type_station du programme_annuel doivent être présents dans calendrier
      missing_types_from_cal <- setdiff(types_prog, types_cal)
      
      if (length(missing_types_from_cal) > 0) {
        issues <- c(
          issues,
          paste0(
            "Incohérence : les type_station suivants apparaissent dans 'programme_annuel' ",
            "mais ne figurent dans aucune ligne de l'onglet 'calendrier' : ",
            paste(missing_types_from_cal, collapse = ", ")
          )
        )
      }
      
      # --- Cohérence programmes pour les type_station réellement utilisés ----
      
      # 1) Type_station réellement utilisés dans programme_annuel
      types_utilises <- unique(trimws(rv$prog$type_station))
      types_utilises <- types_utilises[types_utilises != "" & !is.na(types_utilises)]
      
      # 2) On limite le calendrier uniquement aux types_station utilisés
      #   (ce qui évite que des programmes "hors périmètre annuel" soient contrôlés)
      cal_reel <- rv$cal[
        trimws(rv$cal$type_station) %in% types_utilises,
        ,
        drop = FALSE
      ]
      
      # 3) Extraction des programmes uniquement sur ces types_station
      prog_cal_reel <- unique(trimws(cal_reel$programme))
      prog_cal_reel <- prog_cal_reel[prog_cal_reel != "" & !is.na(prog_cal_reel)]
      
      # 4) Programmes existants dans programmes_types
      prog_types <- unique(trimws(rv$prog_types$programme))
      
      # 5) Programmes existants dans le BPU
      prog_bpu <- unique(trimws(rv$bpu$label_prestation))
      
      # --- Vérification manquants dans programmes_types
      missing_prog_types <- setdiff(prog_cal_reel, prog_types)
      missing_prog_types <- setdiff(missing_prog_types, prog_sans_analyses)
      
      if (length(missing_prog_types) > 0) {
        issues <- c(
          issues,
          paste0(
            "Les programmes suivants sont utilisés dans le calendrier pour des type_station ",
            "présents dans 'programme_annuel', mais n'existent pas dans 'programmes_types' : ",
            paste(missing_prog_types, collapse = ", ")
          )
        )
      }
      
      # --- Vérification manquants dans le BPU
      missing_prog_bpu <- setdiff(prog_cal_reel, prog_bpu)
      
      if (length(missing_prog_bpu) > 0) {
        issues <- c(
          issues,
          paste0(
            "Les programmes suivants sont utilisés dans le calendrier pour des type_station ",
            "présents dans 'programme_annuel', mais ne figurent pas dans 'label_prestation' du BPU : ",
            paste(missing_prog_bpu, collapse = ", ")
          )
        )
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
      
      shinyjs::delay(50, {
        updateTabsetPanel(
          session,
          inputId = ns("tabs_principaux"),
          selected = "rapport_conf"
        )
      })
      
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
            
            progress_bar<-0.1
            
            # Étape 0 : liste BCO
            incProgress(progress_bar, detail="Repérage des BCO…")
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
            
            print(bco_ids)
            
            
            # 2) Purge des tables dépendantes de BCO
            tables_bco <- list(
              "t_boncommande_quantitatif_bcq" = "bcq_bco_id",
              "t_resultat_res"                = "res_bco_id",
              "t_boncommande_pgm_bcp"         = "bcp_bco_id",
              "t_resultatanalyse_rea"         = "res_bco_id",
              "t_resultatoperation_reo"       = "res_bco_id",
              "t_resultatcondenvir_rec"       = "res_bco_id",
              "t_resultatcondreceptechantillon_ree" = "res_bco_id",
              "t_boncommande_bco"= "bco_id"
                )
            

            for (tbl in names(tables_bco)) {
              progress_bar<-progress_bar+0.1
              incProgress(progress_bar, detail=paste0("Suppression des données dans "),tbl)
              col <- tables_bco[[tbl]]
              # Si aucun BCO → on saute le delete proprement
              if (length(bco_ids) > 0) {
                
                sql_delete <- glue::glue_sql(
                  "DELETE FROM sqe.{`tbl`} WHERE {`col`} IN ({vals*})",
                  vals = bco_ids,
                  .con = conn
                )
                
                dbExecute(conn, sql_delete)
              }
              rv$log <- c(rv$log, glue::glue("[{Sys.time()}] Purge {tbl} OK"))
            }
            
            
            # 3) Purge du calendrier & programme annuel (marche + année)
            req_cal <- glue::glue_sql("
    DELETE FROM sqe.t_calendrierprog_cal
    WHERE cal_mar_id = {mar}
    AND cal_refannee = {as.character(an)};
", .con = conn)
            
            req_pga <- glue::glue_sql("
    DELETE FROM sqe.t_progannuelle_pga
    WHERE pga_mar_id = {mar}
    AND pga_cal_refannee = {as.character(an)};
", .con = conn)
            
            
            
            progress_bar<-progress_bar+0.1
            incProgress(progress_bar, detail=paste0("Suppression des données dans t_calendrierprog_cal"))
            
            dbExecute(conn, req_cal)
            rv$log <- c(rv$log, glue::glue("[{Sys.time()}] Purge calendrier OK"))
            
            progress_bar<-progress_bar+0.1
            incProgress(progress_bar, detail=paste0("Suppression des données dans t_progannuelle_pga"))
            
            dbExecute(conn, req_pga)
            rv$log <- c(rv$log, glue::glue("[{Sys.time()}] Purge programme annuel OK"))
            
            
            # # 4) Purge des tables TEMP (clean complet)
            # 
            # tables_temp <- c(
            #   "t_boncommande_bco_temp",
            #   "t_boncommande_pgm_bcp_temp",
            #   "t_boncommande_quantitatif_bcq_temp",
            #   "t_calendrierprog_cal_temp",
            #   "t_parametreprogrammetype_ppt_temp",
            #   "t_prestation_prs_temp",
            #   "t_prestation_id_temp",
            #   "t_prixunitaire_pru_temp",
            #   "t_prixunitaireprestation_prp_temp",
            #   "t_prixunitairerunanalytique_prr_temp",
            #   "t_progannuelle_pga_temp",
            #   "t_resultatanalyse_rea_temp",
            #   "t_runanalytique_run_temp"
            # )
            # 
            # for (tbl in tables_temp) {
            #   dbExecute(conn, glue::glue("TRUNCATE temp.{tbl};"))
            #   rv$log <- c(rv$log, glue::glue("[{Sys.time()}] TRUNCATE temp.{tbl} OK"))
            # }
            
            showNotification(
              glue::glue("Purge terminée : {length(bco_ids)} bons de commande nettoyés."),
              type = "message"
            )
            
            
            
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
