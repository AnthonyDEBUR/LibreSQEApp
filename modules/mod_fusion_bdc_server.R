
# modules/mod_fusion_bdc_server.R
# --------------------------------------------
# Module serveur : Fusionner des bons de commandes (BCO)
# - Filtres : Marché + Périmètre + Année(s)
# - Tableau des BCO filtrés
# - Contrainte : même périmètre (bco_per_nom)
# - Transaction :
#     * remplace toutes les références *bco_id* (sur BASE TABLEs uniquement)
#     * recalcul res_codeprel avec suffixe en cas de collisions pour le même paramètre
#     * option : suppression des BCO fusionnés
# - Résumé : lignes mises à jour + objets ignorés (vues, etc.)
# --------------------------------------------

mod_fusion_bdc_server <- function(id, pool, schema_sqe = "sqe") {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    rv <- reactiveValues(
      bco_tab = NULL,
      sel_ids = integer(0),
      resume = ""
    )
    
    # --- 1) Liste des marchés
    observe({
      conn <- pool::poolCheckout(pool); on.exit(pool::poolReturn(conn), add = TRUE)
      sql <- glue::glue_sql(
        "SELECT mar_id, mar_nom FROM {`schema_sqe`}.t_marche_mar ORDER BY mar_id;", .con = conn
      )
      marches <- DBI::dbGetQuery(conn, sql)
      updateSelectInput(
        session, "select_marche",
        choices = setNames(marches$mar_id, paste0(marches$mar_id, " - ", marches$mar_nom))
      )
    })
    
    # --- 2) Périmètres disponibles pour le marché (via BCO)
    observeEvent(input$select_marche, {
      req(input$select_marche)
      conn <- pool::poolCheckout(pool); on.exit(pool::poolReturn(conn), add = TRUE)
      sql_per <- glue::glue_sql(
        "SELECT DISTINCT bco_per_nom
           FROM {`schema_sqe`}.t_boncommande_bco
          WHERE bco_mar_id = {as.integer(input$select_marche)}
          ORDER BY 1;", .con = conn
      )
      perims <- DBI::dbGetQuery(conn, sql_per)$bco_per_nom
      updateSelectInput(session, "select_perimetre", choices = perims, selected = if (length(perims)) perims[1] else character(0))
    })
    
    # --- 3) Années disponibles selon marché + périmètre (via BCP)
    observeEvent(list(input$select_marche, input$select_perimetre), {
      req(input$select_marche, input$select_perimetre)
      conn <- pool::poolCheckout(pool); on.exit(pool::poolReturn(conn), add = TRUE)
      sql_years <- glue::glue_sql(
        "SELECT DISTINCT EXTRACT(YEAR FROM bcp.bcp_dateinterv) AS annee
           FROM {`schema_sqe`}.t_boncommande_pgm_bcp bcp
           JOIN {`schema_sqe`}.t_boncommande_bco bco
             ON bco.bco_id = bcp.bcp_bco_id
          WHERE bco.bco_mar_id = {as.integer(input$select_marche)}
            AND bco.bco_per_nom = {input$select_perimetre}
          ORDER BY 1;", .con = conn
      )
      years <- DBI::dbGetQuery(conn, sql_years)$annee
      updateSelectInput(session, "select_annees", choices = as.character(years), selected = as.character(years))
    })
    
    # --- 4) Tableau des BCO filtré sur marché + périmètre + année(s)
    observeEvent(list(input$select_marche, input$select_perimetre, input$select_annees), {
      req(input$select_marche, input$select_perimetre, input$select_annees)
      conn <- pool::poolCheckout(pool); on.exit(pool::poolReturn(conn), add = TRUE)
      
      # BCO du marché ET du périmètre
      sql_bco <- glue::glue_sql(
        "SELECT bco_id, bco_refcommande, bco_per_nom, bco_stp_nom
           FROM {`schema_sqe`}.t_boncommande_bco
          WHERE bco_mar_id = {as.integer(input$select_marche)}
            AND bco_per_nom = {input$select_perimetre};", .con = conn
      )
      df_bco <- DBI::dbGetQuery(conn, sql_bco)
      
      if (nrow(df_bco) == 0) {
        rv$bco_tab <- data.frame(Message = "Aucun BCO pour ce marché et ce périmètre.")
      } else {
        # Années BCP par BCO
        sql_bcp_year <- glue::glue_sql(
          "SELECT bcp_bco_id AS bco_id, EXTRACT(YEAR FROM bcp_dateinterv) AS annee
             FROM {`schema_sqe`}.t_boncommande_pgm_bcp
            WHERE bcp_bco_id IN ({vals*});",
          vals = df_bco$bco_id, .con = conn
        )
        df_years <- DBI::dbGetQuery(conn, sql_bcp_year)
        
        # Agréger années par BCO
        agg <- df_years |>
          dplyr::group_by(bco_id) |>
          dplyr::summarise(annees_bcp = paste(sort(unique(annee)), collapse = ", "), .groups = "drop")
        
        tab <- dplyr::left_join(df_bco, agg, by = "bco_id")
        sel_years <- as.integer(input$select_annees)
        
        # Un BCO sans BCP reste affiché (utile pour prestations sans dates)
        tab <- tab |>
          dplyr::mutate(
            match_sel = ifelse(is.na(annees_bcp), TRUE,
                               any(as.integer(strsplit(annees_bcp, ",\\s*")[[1]]) %in% sel_years))
          )
        
        tab <- tab[tab$match_sel, c("bco_id", "bco_refcommande", "bco_per_nom", "bco_stp_nom", "annees_bcp")]
        rv$bco_tab <- tab[order(tab$bco_id), ]
      }
      
      output$dt_bco <- DT::renderDT({
        DT::datatable(rv$bco_tab, rownames = FALSE, selection = "multiple",
                      options = list(pageLength = 15, dom = "tip", order = list(list(0, 'asc'))))
      })
    }, ignoreInit = TRUE)
    
    # --- 5) Suivi sélection
    observeEvent(input$dt_bco_rows_selected, {
      sel <- input$dt_bco_rows_selected
      if (is.null(sel) || length(sel) == 0 || is.null(rv$bco_tab)) {
        rv$sel_ids <- integer(0); rv$resume <- ""
      } else {
        rv$sel_ids <- rv$bco_tab$bco_id[sel]
        rv$resume <- sprintf("Sélection : %s", paste(rv$bco_tab$bco_refcommande[sel], collapse = " ; "))
      }
      output$txt_resume <- renderText(rv$resume)
    })
    
    # --- 6) Demande de fusion : vérifs + modal
    observeEvent(input$btn_fusion, {
      req(rv$sel_ids); validate(need(length(rv$sel_ids) >= 2, "Sélectionnez au moins deux bons de commandes."))
      
      # Contrainte : tous les BCO sélectionnés doivent partager le même périmètre (et égal à celui choisi)
      idx <- match(rv$sel_ids, rv$bco_tab$bco_id)
      perims <- unique(rv$bco_tab$bco_per_nom[idx])
      validate(need(length(perims) == 1, "Tous les BCO sélectionnés doivent appartenir au même périmètre (bco_per_nom)."))
      validate(need(perims[1] == input$select_perimetre, "Le périmètre des BCO doit correspondre au périmètre sélectionné."))
      
      target_id <- as.integer(rv$sel_ids[1])
      others_id <- as.integer(rv$sel_ids[-1])
      
      showModal(modalDialog(
        title = "Confirmer la fusion des BCO",
        div(
          p(HTML(sprintf("<b>Périmètre</b> : %s<br/><b>BCO cible</b> : %d<br/><b>BCO à fusionner</b> : %s",
                         input$select_perimetre, target_id, paste(others_id, collapse = ", ")))),
          if (isTRUE(input$chk_delete_bco))
            p("Les BCO fusionnés seront supprimés après mise à jour des références.")
        ),
        footer = tagList(modalButton("Annuler"),
                         actionButton(ns("confirm_fusion"), "Confirmer", class = "btn btn-danger")),
        easyClose = TRUE
      ))
    })
    
    # --- 7) Fusion (transaction) + recalcul res_codeprel (avec suffixe par paramètre) + suppression optionnelle
    observeEvent(input$confirm_fusion, {
      removeModal()
      req(input$select_marche, input$select_perimetre, rv$sel_ids)
      
      target_id <- as.integer(rv$sel_ids[1])
      others_id <- as.integer(rv$sel_ids[-1])
      
      conn <- pool::poolCheckout(pool); on.exit(pool::poolReturn(conn), add = TRUE)
      
      tryCatch({
        DBI::dbBegin(conn)
        
        # 7.1) Lister colonnes 'bco_id' dans les BASE TABLE du schéma
        sql_cols_all <- glue::glue_sql(
          "SELECT c.table_name, c.column_name, t.table_type
             FROM information_schema.columns c
             JOIN information_schema.tables t
               ON t.table_schema = c.table_schema AND t.table_name = c.table_name
            WHERE c.table_schema = {schema_sqe}
              AND c.column_name ILIKE '%bco_id%';", .con = conn
        )
        objs_all <- DBI::dbGetQuery(conn, sql_cols_all)
        
        # BASE TABLE uniquement (ignore VIEW, etc.)
        cols <- subset(objs_all, table_type == "BASE TABLE")
        if (nrow(cols) == 0) stop("Aucune colonne 'bco_id' dans une table de base du schéma.")
        cols <- cols[!(cols$table_name %in% c("t_boncommande_bco")), ]  # ne pas toucher à la PK BCO
        
        ignored <- subset(objs_all, table_type != "BASE TABLE")
        ignored$why <- "Ignoré (non BASE TABLE)"
        
        res_summary <- data.frame(table = character(0), column = character(0), rows_updated = integer(0), stringsAsFactors = FALSE)
        
        # 7.2) Update des références vers le BCO cible
        if (length(others_id) > 0 && nrow(cols) > 0) {
          for (i in seq_len(nrow(cols))) {
            tbl <- cols$table_name[i]
            col <- cols$column_name[i]
            q <- glue::glue(
              "UPDATE {schema_sqe}.{tbl}
                 SET {col} = {target_id}
               WHERE {col} IN ({paste(others_id, collapse = ',')});"
            )
            n <- DBI::dbExecute(conn, q)
            res_summary <- rbind(res_summary, data.frame(table = tbl, column = col, rows_updated = n))
          }
        }
        
        # 7.3) Recalcul res_codeprel (collision par paramètre)
        sql_pk <- glue::glue_sql(
          "SELECT a.attname
             FROM pg_index i
             JOIN pg_class c ON c.oid = i.indrelid
             JOIN pg_attribute a ON a.attrelid = i.indrelid AND a.attnum = ANY(i.indkey)
             JOIN pg_namespace n ON n.oid = c.relnamespace
            WHERE i.indisprimary
              AND n.nspname = {schema_sqe}
              AND c.relname = 't_resultatanalyse_rea';", .con = conn
        )
        pk_cols <- DBI::dbGetQuery(conn, sql_pk)$attname
        
        if (length(pk_cols) > 0) {
          pk_select <- paste(pk_cols, collapse = ", ")
          pk_join   <- paste(sprintf("r.%s = b.%s", pk_cols, pk_cols), collapse = " AND ")
          
          recalc_q <- glue::glue(
            "WITH base AS (
               SELECT {pk_select},
                      res_bco_id, rea_dateprel_prev, res_stm_cdstationmesureinterne, rea_par_cdparametre,
                      res_bco_id || '*' || rea_dateprel_prev || '*' || res_stm_cdstationmesureinterne AS base_code,
                      COUNT(*) OVER (
                        PARTITION BY res_bco_id, rea_dateprel_prev, res_stm_cdstationmesureinterne, rea_par_cdparametre
                      ) AS cnt,
                      ROW_NUMBER() OVER (
                        PARTITION BY res_bco_id, rea_dateprel_prev, res_stm_cdstationmesureinterne, rea_par_cdparametre
                        ORDER BY COALESCE(rea_cdfractionanalysee::text,''),
                                 COALESCE(rea_cdunitemesure::text,'')
                      ) AS rn
               FROM {schema_sqe}.t_resultatanalyse_rea
               WHERE res_bco_id = {target_id}
            )
            UPDATE {schema_sqe}.t_resultatanalyse_rea AS r
               SET res_codeprel = CASE WHEN b.cnt > 1
                                       THEN b.base_code || '*' || b.rn::text
                                       ELSE b.base_code
                                  END
              FROM base b
             WHERE {pk_join};"
          )
          n_recalc <- DBI::dbExecute(conn, recalc_q)
          
        } else {
          # Fallback via ctid
          recalc_q <- glue::glue(
            "WITH base AS (
               SELECT ctid,
                      res_bco_id, rea_dateprel_prev, res_stm_cdstationmesureinterne, rea_par_cdparametre,
                      res_bco_id || '*' || rea_dateprel_prev || '*' || res_stm_cdstationmesureinterne AS base_code,
                      COUNT(*) OVER (
                        PARTITION BY res_bco_id, rea_dateprel_prev, res_stm_cdstationmesureinterne, rea_par_cdparametre
                      ) AS cnt,
                      ROW_NUMBER() OVER (
                        PARTITION BY res_bco_id, rea_dateprel_prev, res_stm_cdstationmesureinterne, rea_par_cdparametre
                        ORDER BY COALESCE(rea_cdfractionanalysee::text,''),
                                 COALESCE(rea_cdunitemesure::text,'')
                      ) AS rn
               FROM {schema_sqe}.t_resultatanalyse_rea
               WHERE res_bco_id = {target_id}
            )
            UPDATE {schema_sqe}.t_resultatanalyse_rea AS r
               SET res_codeprel = CASE WHEN b.cnt > 1
                                       THEN b.base_code || '*' || b.rn::text
                                       ELSE b.base_code
                                  END
              FROM base b
             WHERE r.ctid = b.ctid;"
          )
          n_recalc <- DBI::dbExecute(conn, recalc_q)
        }
        
        res_summary <- rbind(res_summary, data.frame(
          table = "t_resultatanalyse_rea", column = "res_codeprel (recalc+suffix, par paramètre)", rows_updated = n_recalc
        ))
        
        # 7.4) Option : suppression des BCO fusionnés
        if (isTRUE(input$chk_delete_bco) && length(others_id) > 0) {
          del_q <- glue::glue(
            "DELETE FROM {schema_sqe}.t_boncommande_bco WHERE bco_id IN ({paste(others_id, collapse = ',')});"
          )
          n_del <- DBI::dbExecute(conn, del_q)
          res_summary <- rbind(res_summary, data.frame(
            table = "t_boncommande_bco", column = "bco_id (delete)", rows_updated = n_del
          ))
        }
        
        DBI::dbCommit(conn)
        
        # 7.5) Résumé + notification
        resume_ignored <- if (nrow(ignored) > 0) {
          paste0(
            "\n\nObjets ignorés (non BASE TABLE) :\n",
            paste(sprintf("- %s (%s)", ignored$table_name, ignored$table_type), collapse = "\n")
          )
        } else ""
        
        rv$resume <- paste0(
          "Fusion terminée.\nMarché : ", input$select_marche,
          " | Périmètre : ", input$select_perimetre,
          "\nBCO cible : ", target_id,
          "\nBCO fusionnés : ", ifelse(length(others_id)>0, paste(others_id, collapse = ", "), "aucun"),
          "\n\nMises à jour par table/colonne :\n",
          paste(sprintf("- %s.%s : %d ligne(s)", res_summary$table, res_summary$column, res_summary$rows_updated), collapse = "\n"),
          resume_ignored
        )
        output$txt_resume <- renderText(rv$resume)
        showNotification("Fusion des bons de commandes effectuée avec succès.", type = "message")
        
      }, error = function(e) {
        DBI::dbRollback(conn)
        showNotification(paste("Erreur pendant la fusion :", e$message), type = "error", duration = 12)
      })
    })
  })
}
