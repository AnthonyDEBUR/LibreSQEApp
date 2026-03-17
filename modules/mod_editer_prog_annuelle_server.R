
# =====================================================================
# MODULE : Edition programmation annuelle (SERVER)
# Fichier : mod_editer_prog_annuelle_server.R
# PARTIE 1 / 2
# =====================================================================

mod_editer_prog_annuelle_server <- function(
    id,
    pool,
    schema_sqe = "sqe",
    can_edit = TRUE   # Option A — gestion admin / non-admin
){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # =====================================================================
    # 1) HELPERS ROBUSTES
    # =====================================================================
    
    # ----- Wrapper glue_sql robuste avec propagation d'environnement -----
    sql <- function(..., .envir = parent.frame()){
      glue::glue_sql(..., .con = pool, .envir = .envir)
    }
    
    # ----- Wrapper de connexion (checkout / return) -----
    with_conn <- function(expr){
      conn <- pool::poolCheckout(pool)
      on.exit(pool::poolReturn(conn), add = TRUE)
      
      eval(
        substitute(expr),
        envir  = list(conn = conn),
        enclos = parent.frame()
      )
    }
    
    notify_err <- function(e){
      showNotification(paste("Erreur :", e$message), type="error", duration = 7)
    }
    
    # ----- Mode admin / non-admin -----
    editable <- reactive({ can_edit && isTRUE(input$toggle_edit) })
    
    # =====================================================================
    # 2) CHARGEMENT DES LISTES MARCHE / ANNEE
    # =====================================================================
    
    observe({
      reqs <- with_conn({
        DBI::dbGetQuery(conn, sql("
        SELECT mar_id, mar_nom
        FROM {`schema_sqe`}.t_marche_mar
        ORDER BY mar_id
      "))
      })
      
      updateSelectInput(
        session, "select_marche",
        choices = stats::setNames(reqs$mar_id, paste(reqs$mar_id, reqs$mar_nom))
      )
    })
    
    observeEvent(input$select_marche, {
      req(input$select_marche)
      
      yrs <- with_conn({
        DBI::dbGetQuery(conn, sql("
        SELECT DISTINCT pga_cal_refannee AS an
        FROM {`schema_sqe`}.t_progannuelle_pga
        WHERE pga_mar_id = {as.integer(input$select_marche)}
        ORDER BY an
      "))$an
      })
      
      if (length(yrs) == 0)
        yrs <- as.character(format(Sys.Date(), "%Y"))
      
      updateSelectInput(session, "select_annee",
                        choices = yrs,
                        selected = tail(yrs, 1))
    })
    
    # ----- Contexte clé -----
    key_ctx <- reactive({
      list(
        mar = as.integer(req(input$select_marche)),
        an  = as.character(req(input$select_annee))   # stocké texte en BDD !
      )
    })
    
    # =====================================================================
    # 3) DONNÉES RÉACTIVES (PGA / CAL / BCO / BCQ / BCP / REA)
    # =====================================================================
    
    pga_df <- reactiveVal(NULL)
    cal_df <- reactiveVal(NULL)
    bco_df <- reactiveVal(NULL)
    bcq_df <- reactiveVal(NULL)
    bcp_df <- reactiveVal(NULL)
    rea_df <- reactiveVal(NULL)
    
    # =====================================================================
    # 4) FONCTION DE CHARGEMENT CENTRALISÉ
    # =====================================================================
    
    load_all <- function(){
      
      ctx <- key_ctx()
      
      dat <- with_conn({
        
        list(
          pga = DBI::dbGetQuery(conn, sql("
          SELECT *
          FROM {`schema_sqe`}.t_progannuelle_pga
          WHERE pga_mar_id = {ctx$mar}
            AND pga_cal_refannee::text = {ctx$an}
          ORDER BY pga_per_nom, pga_cal_typestation, pga_stm_cdstationmesureinterne
        ")),
          
          cal = DBI::dbGetQuery(conn, sql("
          SELECT *
          FROM {`schema_sqe`}.t_calendrierprog_cal
          WHERE cal_mar_id = {ctx$mar}
            AND cal_refannee::text = {ctx$an}
          ORDER BY cal_typestation, cal_prs_id, cal_date
        ")),
          
          bco = DBI::dbGetQuery(conn, sql("
          SELECT *
          FROM {`schema_sqe`}.t_boncommande_bco
          WHERE bco_mar_id = {ctx$mar}
          ORDER BY bco_id
        ")),
          
          bcq = DBI::dbGetQuery(conn, sql("
          SELECT *
          FROM {`schema_sqe`}.t_boncommande_quantitatif_bcq
          WHERE bcq_bco_id IN (
            SELECT bco_id
            FROM {`schema_sqe`}.t_boncommande_bco
            WHERE bco_mar_id = {ctx$mar}
          )
          ORDER BY bcq_bco_id, bcq_prs_id
        ")),
          
          bcp = DBI::dbGetQuery(conn, sql("
          SELECT *
          FROM {`schema_sqe`}.t_boncommande_pgm_bcp
          WHERE bcp_bco_id IN (
            SELECT bco_id
            FROM {`schema_sqe`}.t_boncommande_bco
            WHERE bco_mar_id = {ctx$mar}
          )
          ORDER BY bcp_bco_id, bcp_prs_id, bcp_dateinterv
        ")),
          
          rea = DBI::dbGetQuery(conn, sql("
          SELECT *
          FROM {`schema_sqe`}.t_resultatanalyse_rea
          WHERE res_bco_id IN (
            SELECT bco_id
            FROM {`schema_sqe`}.t_boncommande_bco
            WHERE bco_mar_id = {ctx$mar}
          )
          ORDER BY res_bco_id, rea_dateprel_prev, res_stm_cdstationmesureinterne
        "))
        )
      })
      
      pga_df(dat$pga)
      cal_df(dat$cal)
      bco_df(dat$bco)
      bcq_df(dat$bcq)
      bcp_df(dat$bcp)
      rea_df(dat$rea)
    }
    
    observeEvent(key_ctx(), load_all(), ignoreInit = FALSE)
    observeEvent(input$btn_refresh, load_all())
    
    # =====================================================================
    # 5) APPLICATION DES FILTRES INTELLIGENTS
    # =====================================================================
    
    # Cette fonction applique les filtres sur un data.frame donné
    apply_filters <- function(df){
      
      if (is.null(df) || nrow(df) == 0) return(df)
      
      per  <- input$flt_perimetre
      typ  <- input$flt_typestation
      stm  <- input$flt_station
      prs  <- input$flt_presta
      bco  <- input$flt_bco
      dmin <- input$flt_date_min
      dmax <- input$flt_date_max
      
      out <- df
      
      if (!is.null(out$pga_per_nom) && per != "Tous")
        out <- out[out$pga_per_nom == per, , drop=FALSE]
      
      if (!is.null(out$cal_typestation) && typ != "Tous")
        out <- out[out$cal_typestation == typ, , drop=FALSE]
      
      if (!is.null(out$pga_stm_cdstationmesureinterne) && stm != "Toutes")
        out <- out[out$pga_stm_cdstationmesureinterne == stm, , drop=FALSE]
      
      if (!is.null(out$cal_prs_id) && prs != "Toutes")
        out <- out[out$cal_prs_id == prs, , drop=FALSE]
      
      if (!is.null(out$bcp_bco_id) && bco != "Tous")
        out <- out[out$bcp_bco_id == bco, , drop=FALSE]
      
      if (!is.null(out$cal_date) && !is.na(dmin))
        out <- out[out$cal_date >= dmin, , drop=FALSE]
      
      if (!is.null(out$cal_date) && !is.na(dmax))
        out <- out[out$cal_date <= dmax, , drop=FALSE]
      
      out
    }
    
    # =====================================================================
    # 6) RENDU DES DATATABLES
    # =====================================================================
    
    make_dt <- function(df){
      DT::datatable(
        df,
        selection = "multiple",
        filter = "top",
        editable = if(editable()) "cell" else "none",
        options = list(
          scrollX = TRUE,
          pageLength = 20,
          autoWidth = TRUE
        )
      )
    }
    
    output$dt_pga <- DT::renderDT(make_dt(apply_filters(pga_df())))
    output$dt_cal <- DT::renderDT(make_dt(apply_filters(cal_df())))
    output$dt_bco <- DT::renderDT(make_dt(apply_filters(bco_df())))
    output$dt_bcq <- DT::renderDT(make_dt(apply_filters(bcq_df())))
    output$dt_bcp <- DT::renderDT(make_dt(apply_filters(bcp_df())))
    output$dt_rea <- DT::renderDT(make_dt(apply_filters(rea_df())))
    
    observeEvent(input$btn_reset_filtres, {
      updateSelectInput(session,"flt_perimetre",  selected="Tous")
      updateSelectInput(session,"flt_typestation",selected="Tous")
      updateSelectInput(session,"flt_station",    selected="Toutes")
      updateSelectInput(session,"flt_presta",     selected="Toutes")
      updateSelectInput(session,"flt_bco",        selected="Tous")
      updateDateInput(session,"flt_date_min",     value=NA)
      updateDateInput(session,"flt_date_max",     value=NA)
    })
    
    
    # =====================================================================
    # 7) EDITION INLINE (UPDATE)
    # =====================================================================
    
    # Fonction générique UPDATE
    perform_update <- function(table, key_names, row_values, col_name, new_val){
      with_conn({
        
        where_sql <- paste(
          vapply(names(key_names), function(k){
            as.character(sql("{`k`} = {row_values[[k]]}"))
          }, character(1)),
          collapse = " AND "
        )
        
        q <- sprintf(
          "UPDATE %s.%s SET %s = $val WHERE %s",
          schema_sqe, table, col_name, where_sql
        )
        
        DBI::dbExecute(conn, q, params = list(val = new_val))
      })
    }
    
    handle_edit <- function(info, df, table, key_names){
      
      tryCatch({
        
        row <- df()[info$row, , drop=FALSE]
        col <- colnames(df())[info$col]
        new_value <- info$value
        
        key_values <- as.list(row[, key_names, drop=TRUE])
        
        perform_update(table, key_names, key_values, col, new_value)
        
        tmp <- df()
        tmp[info$row, info$col] <- DT::coerceValue(new_value, tmp[info$row, info$col])
        df(tmp)
        
      }, error = notify_err)
    }
    
    # ---- Handlers édition ----
    observeEvent(input$dt_pga_cell_edit, {
      if (editable())
        handle_edit(input$dt_pga_cell_edit, pga_df, "t_progannuelle_pga",
                    c("pga_mar_id","pga_cal_refannee","pga_per_nom","pga_cal_typestation","pga_stm_cdstationmesureinterne"))
    })
    
    observeEvent(input$dt_cal_cell_edit, {
      if (editable())
        handle_edit(input$dt_cal_cell_edit, cal_df, "t_calendrierprog_cal",
                    c("cal_mar_id","cal_refannee","cal_typestation","cal_prs_id","cal_date"))
    })
    
    observeEvent(input$dt_bco_cell_edit, {
      if (editable())
        handle_edit(input$dt_bco_cell_edit, bco_df, "t_boncommande_bco",
                    c("bco_id"))
    })
    
    observeEvent(input$dt_bcq_cell_edit, {
      if (editable())
        handle_edit(input$dt_bcq_cell_edit, bcq_df, "t_boncommande_quantitatif_bcq",
                    c("bcq_bco_id","bcq_prs_id"))
    })
    
    observeEvent(input$dt_bcp_cell_edit, {
      if (editable())
        handle_edit(input$dt_bcp_cell_edit, bcp_df, "t_boncommande_pgm_bcp",
                    c("bcp_bco_id","bcp_prs_id","bcp_dateinterv","bcp_stm_cdstationmesureinterne"))
    })
    
    observeEvent(input$dt_rea_cell_edit, {
      if (editable())
        handle_edit(input$dt_rea_cell_edit, rea_df, "t_resultatanalyse_rea",
                    c("res_codeprel"))
    })
    
    
    # =====================================================================
    # 8) SUPPRESSION (DELETE) AVEC CASCADE SÉCURISÉE
    # =====================================================================
    
    delete_rows <- function(df, selected, table, key_names){
      rows <- df()[selected, , drop=FALSE]
      if (nrow(rows) == 0) return()
      
      with_conn({
        DBI::dbBegin(conn)
        
        tryCatch({
          
          for(i in seq_len(nrow(rows))){
            
            r <- rows[i, ]
            
            where_sql <- paste(
              vapply(key_names, function(k){
                as.character(sql("{`k`} = {r[[k]]}"))
              }, character(1)),
              collapse = " AND "
            )
            
            DBI::dbExecute(conn, sprintf(
              "DELETE FROM %s.%s WHERE %s",
              schema_sqe, table, where_sql
            ))
          }
          
          DBI::dbCommit(conn)
          
        }, error=function(e){
          DBI::dbRollback(conn)
          stop(e)
        })
      })
      
      load_all()
    }
    
    # ---- Bouton suppression ----
    observeEvent(input$btn_del, {
      req(editable())
      
      act <- input$tabs
      
      if (act == "PGA")
        delete_rows(pga_df, input$dt_pga_rows_selected,
                    "t_progannuelle_pga",
                    c("pga_mar_id","pga_cal_refannee","pga_per_nom","pga_cal_typestation","pga_stm_cdstationmesureinterne"))
      
      else if (act == "CAL")
        delete_rows(cal_df, input$dt_cal_rows_selected,
                    "t_calendrierprog_cal",
                    c("cal_mar_id","cal_refannee","cal_typestation","cal_prs_id","cal_date"))
      
      else if (act == "BCO"){
        sel <- input$dt_bco_rows_selected
        req(length(sel)>0)
        
        rows <- bco_df()[sel, , drop=FALSE]
        
        with_conn({
          DBI::dbBegin(conn)
          tryCatch({
            
            for (i in seq_len(nrow(rows))){
              
              bco_id <- rows$bco_id[i]
              
              # cascade BCP / BCQ / REA
              DBI::dbExecute(conn, sql("
              DELETE FROM {`schema_sqe`}.t_boncommande_pgm_bcp
              WHERE bcp_bco_id = {bco_id}
            "))
              
              DBI::dbExecute(conn, sql("
              DELETE FROM {`schema_sqe`}.t_boncommande_quantitatif_bcq
              WHERE bcq_bco_id = {bco_id}
            "))
              
              DBI::dbExecute(conn, sql("
              DELETE FROM {`schema_sqe`}.t_resultatanalyse_rea
              WHERE res_bco_id = {bco_id}
            "))
              
              DBI::dbExecute(conn, sql("
              DELETE FROM {`schema_sqe`}.t_boncommande_bco
              WHERE bco_id = {bco_id}
            "))
            }
            
            DBI::dbCommit(conn)
            
          }, error=function(e){
            DBI::dbRollback(conn)
            notify_err(e)
          })
        })
        
        load_all()
      }
      
      else if (act == "BCQ")
        delete_rows(bcq_df, input$dt_bcq_rows_selected,
                    "t_boncommande_quantitatif_bcq",
                    c("bcq_bco_id","bcq_prs_id"))
      
      else if (act == "BCP")
        delete_rows(bcp_df, input$dt_bcp_rows_selected,
                    "t_boncommande_pgm_bcp",
                    c("bcp_bco_id","bcp_prs_id","bcp_dateinterv","bcp_stm_cdstationmesureinterne"))
      
      else if (act == "REA")
        delete_rows(rea_df, input$dt_rea_rows_selected,
                    "t_resultatanalyse_rea",
                    c("res_codeprel"))
    })
    
    # =====================================================================
    # ====== FIN PARTIE 1 — LA SUITE DANS MESSAGE 3 / 4 ======
    # =====================================================================
    


# =====================================================================
# PARTIE 2 / 2 — SUITE DU SERVEUR
# (3A) : Filtres -> choix dynamiques, boutons contexte, INSERT (modales)
# =====================================================================

# -------------------------------------------------------------------
# 9) MISE À JOUR DES CHOIX DES FILTRES (après chaque load_all)
# -------------------------------------------------------------------
update_filter_choices <- function(){
  ctx <- key_ctx()
  
  # Valeurs par défaut
  per_choices  <- "Tous"
  typ_choices  <- "Tous"
  stm_choices  <- "Toutes"
  prs_choices  <- "Toutes"
  bco_choices  <- "Tous"
  
  # Périmètres (PGA)
  if (!is.null(pga_df()) && nrow(pga_df())>0) {
    per_choices <- c("Tous", sort(unique(pga_df()$pga_per_nom)))
  }
  
  # Type station (CAL + PGA)
  typ_vec <- c()
  if (!is.null(cal_df()) && nrow(cal_df())>0) typ_vec <- c(typ_vec, cal_df()$cal_typestation)
  if (!is.null(pga_df()) && nrow(pga_df())>0) typ_vec <- c(typ_vec, pga_df()$pga_cal_typestation)
  if (length(typ_vec)) typ_choices <- c("Tous", sort(unique(typ_vec)))
  
  # Stations internes (PGA)
  if (!is.null(pga_df()) && nrow(pga_df())>0) {
    stm_choices <- c("Toutes", sort(unique(pga_df()$pga_stm_cdstationmesureinterne)))
  }
  
  # Prestations (depuis CAL via prs_id, mais on affiche libellé si dispo)
  if (!is.null(cal_df()) && nrow(cal_df())>0) {
    prs_ids <- sort(unique(cal_df()$cal_prs_id))
    if (length(prs_ids)>0) {
      # On tente de récupérer les labels
      prs_map <- with_conn({
        DBI::dbGetQuery(conn, sql("
            SELECT prs_id, prs_label_prestation
            FROM {`schema_sqe`}.t_prestation_prs
            WHERE prs_mar_id = {ctx$mar}
              AND prs_id IN ({prs_ids*})
            ORDER BY prs_label_prestation
          "))
      })
      if (!is.null(prs_map) && nrow(prs_map)>0) {
        prs_choices <- c("Toutes", stats::setNames(prs_map$prs_id, prs_map$prs_label_prestation))
      } else {
        prs_choices <- c("Toutes", prs_ids)
      }
    }
  }
  
  # BCO
  if (!is.null(bco_df()) && nrow(bco_df())>0) {
    # on propose la liste "id - refcommande"
    bco_choices <- c("Tous", stats::setNames(bco_df()$bco_id, paste0("#", bco_df()$bco_id, " - ", bco_df()$bco_refcommande)))
  }
  
  # Mise à jour des inputs
  updateSelectInput(session, "flt_perimetre",  choices = per_choices)
  updateSelectInput(session, "flt_typestation",choices = typ_choices)
  updateSelectInput(session, "flt_station",    choices = stm_choices)
  updateSelectInput(session, "flt_presta",     choices = prs_choices)
  updateSelectInput(session, "flt_bco",        choices = bco_choices)
}

# Au chargement des données -> mettre à jour les filtres
observeEvent(list(pga_df(), cal_df(), bco_df()), {
  update_filter_choices()
}, ignoreInit = TRUE)

# -------------------------------------------------------------------
# 10) BOUTONS CONTEXTUELS (Ajouter / Supprimer)
# -------------------------------------------------------------------
output$ctx_buttons <- renderUI({
  # NB : même si non admin -> on affiche les boutons, mais la logique serveur
  # vérifie editable() pour autoriser l'action.
  act <- input$tabs
  tagList(
    actionButton(ns("btn_add"), paste0("Ajouter dans ", act), icon = icon("plus")),
    actionButton(ns("btn_del"), paste0("Supprimer sélection dans ", act), icon = icon("trash"))
  )
})

# -------------------------------------------------------------------
# 11) INSERT : Modales + Insertion (PGA, CAL, BCO, BCQ, BCP, REA)
# -------------------------------------------------------------------

show_add_modal <- function(){
  req(editable())
  act <- input$tabs
  ctx <- key_ctx()
  
  # Référentiels nécessaires
  per <- with_conn(DBI::dbGetQuery(conn,
                                   sql("SELECT per_nom FROM {`'refer'`}.tr_perimetre_per ORDER BY per_nom")))
  prs <- with_conn(DBI::dbGetQuery(conn,
                                   sql("SELECT prs_id, prs_label_prestation
           FROM {`schema_sqe`}.t_prestation_prs
           WHERE prs_mar_id = {ctx$mar}
           ORDER BY prs_label_prestation")))
  stm <- with_conn(DBI::dbGetQuery(conn,
                                   sql("SELECT stm_cdstationmesureinterne FROM {`'refer'`}.tr_stationmesure_stm
           ORDER BY stm_cdstationmesureinterne")))
  
  # Quelques choix dérivés
  types_pga <- if(!is.null(pga_df()) && nrow(pga_df())>0) sort(unique(pga_df()$pga_cal_typestation)) else character(0)
  types_cal <- if(!is.null(cal_df()) && nrow(cal_df())>0) sort(unique(cal_df()$cal_typestation)) else character(0)
  types_all <- sort(unique(c(types_pga, types_cal)))
  
  if (act == "PGA") {
    showModal(modalDialog(
      title = "Ajouter une ligne dans PGA",
      selectInput(ns("add_pga_per"), "Périmètre", choices = per$per_nom),
      selectInput(ns("add_pga_typ"), "Type station", choices = types_all),
      selectInput(ns("add_pga_stm"), "Code station interne", choices = stm$stm_cdstationmesureinterne),
      footer = tagList(
        modalButton("Annuler"),
        actionButton(ns("do_add_pga"), "Ajouter", class = "btn btn-primary")
      ),
      easyClose = TRUE
    ))
    
  } else if (act == "CAL") {
    showModal(modalDialog(
      title = "Ajouter une intervention dans CAL",
      selectInput(ns("add_cal_typ"), "Type station", choices = types_all),
      selectInput(ns("add_cal_prs"), "Prestation",
                  choices = stats::setNames(prs$prs_id, prs$prs_label_prestation)),
      dateInput(ns("add_cal_date"), "Date d’intervention",
                value = as.Date(paste0(ctx$an, "-01-01"))),
      textInput(ns("add_cal_rat"), "Rattachement BDC", ""),
      footer = tagList(
        modalButton("Annuler"),
        actionButton(ns("do_add_cal"), "Ajouter", class = "btn btn-primary")
      ),
      easyClose = TRUE
    ))
    
  } else if (act == "BCO") {
    showModal(modalDialog(
      title = "Créer un bon de commande (BCO)",
      textInput(ns("add_bco_ref"), "Référence commande"),
      selectInput(ns("add_bco_per"), "Périmètre", choices = per$per_nom),
      selectInput(ns("add_bco_stp"), "Statut", choices = c("1-projet","2-en cours","3-clôturé")),
      footer = tagList(
        modalButton("Annuler"),
        actionButton(ns("do_add_bco"), "Créer", class = "btn btn-primary")
      ),
      easyClose = TRUE
    ))
    
  } else if (act == "BCQ") {
    bco <- bco_df()
    showModal(modalDialog(
      title = "Ajouter un quantitatif (BCQ)",
      selectInput(ns("add_bcq_bco"), "BCO",
                  choices = stats::setNames(bco$bco_id, paste0("#",bco$bco_id," - ",bco$bco_refcommande))),
      selectInput(ns("add_bcq_prs"), "Prestation",
                  choices = stats::setNames(prs$prs_id, prs$prs_label_prestation)),
      numericInput(ns("add_bcq_nb"), "Nb prestations", value = 1, min = 1, step = 1),
      footer = tagList(
        modalButton("Annuler"),
        actionButton(ns("do_add_bcq"), "Ajouter", class = "btn btn-primary")
      ),
      easyClose = TRUE
    ))
    
  } else if (act == "BCP") {
    bco <- bco_df()
    showModal(modalDialog(
      title = "Ajouter une ligne plan daté (BCP)",
      selectInput(ns("add_bcp_bco"), "BCO",
                  choices = stats::setNames(bco$bco_id, paste0("#",bco$bco_id," - ",bco$bco_refcommande))),
      selectInput(ns("add_bcp_prs"), "Prestation",
                  choices = stats::setNames(prs$prs_id, prs$prs_label_prestation)),
      dateInput(ns("add_bcp_date"), "Date intervention", value = as.Date(paste0(ctx$an,"-01-01"))),
      selectInput(ns("add_bcp_stm"), "Station interne", stm$stm_cdstationmesureinterne),
      footer = tagList(
        modalButton("Annuler"),
        actionButton(ns("do_add_bcp"), "Ajouter", class = "btn btn-primary")
      ),
      easyClose = TRUE
    ))
    
  } else if (act == "REA") {
    bco <- bco_df()
    showModal(modalDialog(
      title = "Créer un résultat attendu (REA)",
      selectInput(ns("add_rea_bco"), "BCO",
                  choices = stats::setNames(bco$bco_id, paste0("#",bco$bco_id," - ",bco$bco_refcommande))),
      selectInput(ns("add_rea_stm"), "Station interne", stm$stm_cdstationmesureinterne),
      dateInput(ns("add_rea_date"), "Date prévue", value = as.Date(paste0(ctx$an,"-01-01"))),
      textInput(ns("add_rea_par"), "Code paramètre (optionnel)", ""),
      footer = tagList(
        modalButton("Annuler"),
        actionButton(ns("do_add_rea"), "Ajouter", class = "btn btn-primary")
      ),
      easyClose = TRUE
    ))
  }
}

observeEvent(input$btn_add, {
  show_add_modal()
})

# ------------------------
# INSERT HANDLERS
# ------------------------

observeEvent(input$do_add_pga, {
  req(editable())
  ctx <- key_ctx()
  tryCatch({
    with_conn({
      DBI::dbExecute(conn, sql("
          INSERT INTO {`schema_sqe`}.t_progannuelle_pga
          (pga_cal_refannee, pga_mar_id, pga_per_nom, pga_cal_typestation,
           pga_stm_cdstationmesureauxsurface, pga_stm_cdstationmesureinterne)
          VALUES ({ctx$an}, {ctx$mar}, {input$add_pga_per}, {input$add_pga_typ},
                  NULL, {input$add_pga_stm})
        "))
    })
    removeModal(); load_all()
  }, error = notify_err)
})

observeEvent(input$do_add_cal, {
  req(editable())
  ctx <- key_ctx()
  tryCatch({
    with_conn({
      DBI::dbExecute(conn, sql("
          INSERT INTO {`schema_sqe`}.t_calendrierprog_cal
          (cal_refannee, cal_mar_id, cal_typestation, cal_date, cal_prs_id, cal_rattachement_bdc)
          VALUES ({ctx$an}, {ctx$mar}, {input$add_cal_typ}, {as.Date(input$add_cal_date)},
                  {as.integer(input$add_cal_prs)}, {input$add_cal_rat})
        "))
    })
    removeModal(); load_all()
  }, error = notify_err)
})

observeEvent(input$do_add_bco, {
  req(editable())
  ctx <- key_ctx()
  tryCatch({
    with_conn({
      DBI::dbExecute(conn, sql("
          INSERT INTO {`schema_sqe`}.t_boncommande_bco
          (bco_refcommande, bco_stp_nom, bco_mar_id, bco_per_nom)
          VALUES ({input$add_bco_ref}, {input$add_bco_stp}, {ctx$mar}, {input$add_bco_per})
        "))
    })
    removeModal(); load_all()
  }, error = notify_err)
})

observeEvent(input$do_add_bcq, {
  req(editable())
  tryCatch({
    with_conn({
      DBI::dbExecute(conn, sql("
          INSERT INTO {`schema_sqe`}.t_boncommande_quantitatif_bcq
          (bcq_bco_id, bcq_prs_id, bcq_nbprestacom)
          VALUES ({as.integer(input$add_bcq_bco)}, {as.integer(input$add_bcq_prs)}, {as.integer(input$add_bcq_nb)})
        "))
    })
    removeModal(); load_all()
  }, error = notify_err)
})

observeEvent(input$do_add_bcp, {
  req(editable())
  tryCatch({
    with_conn({
      DBI::dbExecute(conn, sql("
          INSERT INTO {`schema_sqe`}.t_boncommande_pgm_bcp
          (bcp_bco_id, bcp_prs_id, bcp_dateinterv, bcp_stm_cdstationmesureinterne)
          VALUES ({as.integer(input$add_bcp_bco)}, {as.integer(input$add_bcp_prs)},
                  {as.Date(input$add_bcp_date)}, {input$add_bcp_stm})
        "))
    })
    removeModal(); load_all()
  }, error = notify_err)
})

observeEvent(input$do_add_rea, {
  req(editable())
  tryCatch({
    res_code <- paste0(input$add_rea_bco, "*", as.character(input$add_rea_date), "*", input$add_rea_stm)
    with_conn({
      DBI::dbExecute(conn, sql("
          INSERT INTO {`schema_sqe`}.t_resultatanalyse_rea
          (res_codeprel, res_bco_id, res_stm_cdstationmesureinterne,
           rea_dateprel_prev, rea_par_cdparametre)
          VALUES ({res_code}, {as.integer(input$add_rea_bco)}, {input$add_rea_stm},
                  {as.Date(input$add_rea_date)}, NULLIF({input$add_rea_par}, ''))
        "))
    })
    removeModal(); load_all()
  }, error = notify_err)
})

# =====================================================================
# ====== FIN PARTIE 2 (3A) — LA SUITE DANS MESSAGE 3B / 4 ======
# =====================================================================

# =====================================================================
# PARTIE 2 / 2 — SUITE (3B) : Recalcul XLSX, Export Excel, Diag (server),
# navigation, mode lecture seule.
# =====================================================================

# -------------------------------------------------------------------
# 12) RECALCUL COMPLET DE LA PROGRAMMATION DEPUIS XLSX (ADMIN)
# -------------------------------------------------------------------

# -- Pré-état base pour le marché/année courant --
get_existing_state <- function(mar, an){
  with_conn({
    date_debut <- as.Date(sprintf("%s-01-01", an))
    date_fin   <- as.Date(sprintf("%s-01-01", as.integer(an) + 1))
    
    nb_cal <- DBI::dbGetQuery(conn, sql("
        SELECT COUNT(*) AS nb
        FROM {`schema_sqe`}.t_calendrierprog_cal
        WHERE cal_mar_id = {mar} AND cal_refannee::text = {an}
      "))$nb
    
    nb_pga <- DBI::dbGetQuery(conn, sql("
        SELECT COUNT(*) AS nb
        FROM {`schema_sqe`}.t_progannuelle_pga
        WHERE pga_mar_id = {mar} AND pga_cal_refannee::text = {an}
      "))$nb
    
    bco_ids <- DBI::dbGetQuery(conn, sql("
        SELECT DISTINCT bcp_bco_id AS bco_id
        FROM {`schema_sqe`}.t_boncommande_pgm_bcp bcp
        JOIN {`schema_sqe`}.t_boncommande_bco bco ON bco.bco_id = bcp.bco_id
        WHERE bco.bco_mar_id = {mar}
          AND bcp.bcp_dateinterv >= {date_debut}
          AND bcp.bcp_dateinterv <  {date_fin}
      "))
    nb_bco <- length(unique(bco_ids$bco_id))
    
    data.frame(objet = c("CAL","PGA","BCO"), nb = c(nb_cal, nb_pga, nb_bco), stringsAsFactors = FALSE)
  })
}

# -- Suppression sécurisée marché/année (cascade) --
wipe_year_market <- function(mar, an){
  with_conn({
    DBI::dbBegin(conn)
    date_debut <- as.Date(sprintf("%s-01-01", an))
    date_fin   <- as.Date(sprintf("%s-01-01", as.integer(an) + 1))
    
    tryCatch({
      
      # Liste BCO période
      bco_ids <- DBI::dbGetQuery(conn, sql("
          SELECT DISTINCT bco.bco_id
          FROM {`schema_sqe`}.t_boncommande_pgm_bcp bcp
          JOIN {`schema_sqe`}.t_boncommande_bco bco ON bco.bco_id = bcp.bco_id
          WHERE bco.bco_mar_id = {mar}
            AND bcp.bcp_dateinterv >= {date_debut}
            AND bcp.bcp_dateinterv <  {date_fin}
        "))$bco_id
      
      # Enfants des BCO (ordre : BCP, REA, BCQ), puis BCO
      if (length(bco_ids) > 0) {
        DBI::dbExecute(conn, sql("
            DELETE FROM {`schema_sqe`}.t_boncommande_pgm_bcp
            WHERE bcp_bco_id IN ({bco_ids*})
          "))
        DBI::dbExecute(conn, sql("
            DELETE FROM {`schema_sqe`}.t_resultatanalyse_rea
            WHERE res_bco_id IN ({bco_ids*})
          "))
        DBI::dbExecute(conn, sql("
            DELETE FROM {`schema_sqe`}.t_boncommande_quantitatif_bcq
            WHERE bcq_bco_id IN ({bco_ids*})
          "))
        DBI::dbExecute(conn, sql("
            DELETE FROM {`schema_sqe`}.t_boncommande_bco
            WHERE bco_id IN ({bco_ids*})
          "))
      }
      
      # CAL de l'année
      DBI::dbExecute(conn, sql("
          DELETE FROM {`schema_sqe`}.t_calendrierprog_cal
          WHERE cal_mar_id = {mar}
            AND cal_refannee::text = {an}
        "))
      
      # PGA de l'année
      DBI::dbExecute(conn, sql("
          DELETE FROM {`schema_sqe`}.t_progannuelle_pga
          WHERE pga_mar_id = {mar}
            AND pga_cal_refannee::text = {an}
        "))
      
      DBI::dbCommit(conn)
    }, error=function(e){
      DBI::dbRollback(conn)
      stop(e)
    })
  })
}

# -- Bouton Recalcul --
observeEvent(input$btn_recalc, {
  if (!editable()){
    showNotification("Mode lecture seule : le recalcul est réservé aux administrateurs.", type="warning")
    return(invisible())
  }
  req(input$xlsx_recalc)
  ctx <- key_ctx()
  
  # État existant
  etat <- get_existing_state(ctx$mar, ctx$an)
  
  # Demande de confirmation si des données existent
  if (sum(etat$nb) > 0){
    showModal(modalDialog(
      title = "Recalcul : confirmation d'écrasement",
      tagList(
        p("Des données existent déjà pour ce marché et cette année :"),
        DT::datatable(etat, options = list(dom='t', paging=FALSE), rownames = FALSE),
        p("Voulez-vous **écraser** ces données puis **recalculer** depuis le fichier XLSX ?")
      ),
      footer = tagList(
        modalButton("Annuler"),
        actionButton(ns("do_confirm_recalc"), "Écraser et recalculer", class = "btn btn-danger")
      ),
      easyClose = TRUE
    ))
  } else {
    # Pas de données -> recalcul direct
    session$sendCustomMessage(type = "busy", message = TRUE)
    withProgress(message = "Recalcul en cours…", value = 0, {
      incProgress(0.1, detail="Import Excel → Base")
      tryCatch({
        LibreSQE::func_charge_prog_annuelle(
          fichier_prog = input$xlsx_recalc$datapath,
          connexion    = pool,
          mar_id       = ctx$mar,
          annee        = as.integer(ctx$an),
          frequence_bdc= input$xlsx_freq,
          prefixe      = input$xlsx_prefix
        )
        incProgress(0.8, detail="Rechargement des tables")
        load_all()
        incProgress(1)
        showNotification("Recalcul terminé avec succès.", type="message")
      }, error = notify_err, finally = {
        session$sendCustomMessage(type = "busy", message = FALSE)
      })
    })
  }
})

# -- Confirmation d'écrasement + recalcul --
observeEvent(input$do_confirm_recalc, {
  removeModal()
  req(input$xlsx_recalc)
  if (!editable()) return(invisible())
  
  ctx <- key_ctx()
  session$sendCustomMessage(type = "busy", message = TRUE)
  
  withProgress(message = "Écrasement + recalcul…", value = 0, {
    incProgress(0.1, detail="Suppression des données existantes")
    tryCatch({
      
      wipe_year_market(ctx$mar, ctx$an)
      
      incProgress(0.5, detail="Import Excel → Base")
      LibreSQE::func_charge_prog_annuelle(
        fichier_prog = input$xlsx_recalc$datapath,
        connexion    = pool,
        mar_id       = ctx$mar,
        annee        = as.integer(ctx$an),
        frequence_bdc= input$xlsx_freq,
        prefixe      = input$xlsx_prefix
      )
      
      incProgress(0.8, detail="Rechargement des tables")
      load_all()
      incProgress(1)
      
      showNotification("Écrasement + recalcul : succès.", type="message")
    }, error = notify_err, finally = {
      session$sendCustomMessage(type = "busy", message = FALSE)
    })
  })
})


# -------------------------------------------------------------------
# 13) EXPORT EXCEL (.xlsx) : PGA, CAL, BCO, BCQ, BCP, REA, DIAGNOSTIC
# -------------------------------------------------------------------

output$btn_export_excel <- downloadHandler(
  filename = function(){
    ctx <- key_ctx()
    sprintf("prog_annuelle_M%d_A%s.xlsx", ctx$mar, ctx$an)
  },
  content = function(file){
    
    # Données (après filtres)
    pga <- apply_filters(pga_df())
    cal <- apply_filters(cal_df())
    bco <- apply_filters(bco_df())
    bcq <- apply_filters(bcq_df())
    bcp <- apply_filters(bcp_df())
    rea <- apply_filters(rea_df())
    
    # Tables diag depuis le module diagnostic (si disponible)
    diag_list <- NULL
    if (exists("diag_api") && !is.null(diag_api) && is.list(diag_api)){
      if (!is.null(diag_api$get_all_tables)){
        diag_list <- diag_api$get_all_tables()
      }
    }
    
    # Ecriture Excel via openxlsx si possible, sinon writexl
    if (requireNamespace("openxlsx", quietly = TRUE)) {
      wb <- openxlsx::createWorkbook()
      
      add_sheet_df <- function(wb, sheet, df){
        openxlsx::addWorksheet(wb, sheetName = sheet)
        if (!is.null(df) && nrow(df)>0) {
          openxlsx::writeData(wb, sheet = sheet, x = df)
          openxlsx::setColWidths(wb, sheet, cols = 1:ncol(df), widths = "auto")
        } else {
          openxlsx::writeData(wb, sheet = sheet, x = data.frame(message = "Aucune donnée (filtrée)"))
        }
      }
      
      add_sheet_df(wb, "PGA", pga)
      add_sheet_df(wb, "CAL", cal)
      add_sheet_df(wb, "BCO", bco)
      add_sheet_df(wb, "BCQ", bcq)
      add_sheet_df(wb, "BCP", bcp)
      add_sheet_df(wb, "REA", rea)
      
      # Onglets DIAGNOSTIC si fournis par le module diagnostic
      if (!is.null(diag_list)) {
        # diag_list attendu : list(
        #   resume = df, pga_cal = df, cal_bco = df, bco_bcq = df,
        #   bco_bcp = df, bcp_rea = df, rea_pga = df
        # )
        for (nm in names(diag_list)) {
          add_sheet_df(wb, paste0("DIAG_", toupper(nm)), diag_list[[nm]])
        }
      }
      
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      
    } else if (requireNamespace("writexl", quietly = TRUE)) {
      
      out <- list(
        PGA = pga, CAL = cal, BCO = bco, BCQ = bcq, BCP = bcp, REA = rea
      )
      
      if (!is.null(diag_list)) {
        for (nm in names(diag_list)) {
          out[[paste0("DIAG_", toupper(nm))]] <- diag_list[[nm]]
        }
      }
      
      writexl::write_xlsx(out, path = file)
      
    } else {
      # dernier recours : CSV zippés (fallback)
      td <- tempdir()
      readr::write_csv(pga, file.path(td, "PGA.csv"))
      readr::write_csv(cal, file.path(td, "CAL.csv"))
      readr::write_csv(bco, file.path(td, "BCO.csv"))
      readr::write_csv(bcq, file.path(td, "BCQ.csv"))
      readr::write_csv(bcp, file.path(td, "BCP.csv"))
      readr::write_csv(rea, file.path(td, "REA.csv"))
      if (!is.null(diag_list)) {
        for (nm in names(diag_list)) {
          readr::write_csv(diag_list[[nm]], file.path(td, paste0("DIAG_", toupper(nm), ".csv")))
        }
      }
      old <- setwd(td); on.exit(setwd(old), add = TRUE)
      utils::zip(file, files = list.files(td, pattern = "^(PGA|CAL|BCO|BCQ|BCP|REA|DIAG_).*\\.csv$"))
    }
  }
)


# -------------------------------------------------------------------
# 14) MODULE DIAGNOSTIC (SERVER) + Navigation
# -------------------------------------------------------------------

# Le module diagnostic reçoit les RV des 6 tables + contexte + accès DB
diag_api <- mod_editer_prog_annuelle_diag_server(
  id = "diag",
  pool = pool,
  schema_sqe = schema_sqe,
  key_ctx = key_ctx,
  pga = pga_df,
  cal = cal_df,
  bco = bco_df,
  bcq = bcq_df,
  bcp = bcp_df,
  rea = rea_df
)

# Bouton qui bascule sur l’onglet "Diagnostic"
observeEvent(input$btn_show_diag, {
  updateTabsetPanel(session, inputId = "tabs", selected = "Diagnostic")
})


# -------------------------------------------------------------------
# 15) MODE LECTURE SEULE SI NON-ADMIN
# -------------------------------------------------------------------

observe({
  # can_edit est un argument d’entrée du module ; editable() combine can_edit + toggle UI
  if (!can_edit) {
    shinyjs::disable("toggle_edit")
    updateCheckboxInput(session, "toggle_edit", value = FALSE)
    
    shinyjs::disable("btn_add")
    shinyjs::disable("btn_del")
    shinyjs::disable("btn_recalc")
  } else {
    # can_edit = TRUE -> l’utilisateur peut activer/désactiver l’édition
    # On synchronise l’état des boutons avec editable()
    if (isTRUE(input$toggle_edit)) {
      shinyjs::enable("btn_add")
      shinyjs::enable("btn_del")
      shinyjs::enable("btn_recalc")
    } else {
      shinyjs::disable("btn_add")
      shinyjs::disable("btn_del")
      shinyjs::disable("btn_recalc")
    }
  }
})


# =====================================================================
# ====== FIN PARTIE 2 — FERMETURE DU MODULE (server) ======
# =====================================================================
})
}
