
# modules/mod_prestataires_server.R
prestatairesServer <- function(id, pool) {
  moduleServer(id, function(input, output, session) {
    
    # --- Lecture tableau prestataires ---
    fetch_prestataires <- function() {
      DBI::dbGetQuery(
        pool,
        "SELECT pre_id, pre_nom, pre_siret, pre_isa_codesandre
         FROM refer.tr_prestataire_pre
         ORDER BY pre_nom"
      )
    }
    data_prestataires <- reactiveVal(fetch_prestataires())
    
    output$dt_prestataires <- DT::renderDT({
      DT::datatable(
        data_prestataires()[, c("pre_nom","pre_siret","pre_isa_codesandre")],
        rownames = FALSE,
        filter = "top",
        options = list(pageLength = 10, autoWidth = TRUE)
      )
    })
    
    # --- Recherche dans refer.tr_intervenantsandre_isa ---
    build_search_sql <- function(mode, text) {
      stopifnot(nzchar(text))
      if (mode == "code") {
        list(
          sql = "SELECT isa_codesandre, isa_nom, isa_mnemo
                 FROM refer.tr_intervenantsandre_isa
                 WHERE isa_codesandre = $1
                 ORDER BY isa_nom LIMIT 50",
          params = list(text)
        )
      } else if (mode == "nom") {
        list(
          sql = "SELECT isa_codesandre, isa_nom, isa_mnemo
                 FROM refer.tr_intervenantsandre_isa
                 WHERE isa_nom ILIKE $1
                 ORDER BY isa_nom LIMIT 50",
          params = list(paste0('%', text, '%'))
        )
      } else { # mnemo
        list(
          sql = "SELECT isa_codesandre, isa_nom, isa_mnemo
                 FROM refer.tr_intervenantsandre_isa
                 WHERE isa_mnemo ILIKE $1
                 ORDER BY isa_nom LIMIT 50",
          params = list(paste0('%', text, '%'))
        )
      }
    }
    
    candidates <- reactiveVal(data.frame())
    
    observeEvent(input$btn_search, {
      req(nzchar(input$search_text))
      q <- build_search_sql(input$search_mode, input$search_text)
      res <- DBI::dbGetQuery(pool, q$sql, params = q$params)
      candidates(res)
    })
    
    output$dt_candidates <- DT::renderDT({
      DT::datatable(
        candidates(),
        selection = "single",
        rownames = FALSE,
        options = list(pageLength = 5)
      )
    })
    
    # --- Pré-remplissage du formulaire depuis un candidat sélectionné ---
    observeEvent(input$dt_candidates_rows_selected, {
      idx <- input$dt_candidates_rows_selected
      req(idx, nrow(candidates()) >= idx)
      row <- candidates()[idx, ]
      updateTextInput(session, "pre_nom", value = row$isa_nom)
      updateTextInput(session, "pre_isa_codesandre", value = row$isa_codesandre)
      updateTextInput(session, "pre_siret", value = row$isa_codesandre) # défaut = code SANDRE
    })
    
    # --- Ajout d'un prestataire avec contrôle d'unicité sur pre_siret ---
    output$add_status <- renderUI(NULL)
    
    observeEvent(input$btn_add, {
      # validations
      validate(
        need(nzchar(input$pre_nom), "Le nom du prestataire est requis."),
        need(nzchar(input$pre_isa_codesandre), "Le code SANDRE est requis.")
      )
      
      pre_nom   <- trimws(input$pre_nom)
      pre_code  <- trimws(input$pre_isa_codesandre)
      pre_siret <- trimws(ifelse(nzchar(input$pre_siret), input$pre_siret, pre_code))
      
      # 1) Unicité stricte du SIRET
      siret_exists <- DBI::dbGetQuery(
        pool,
        "SELECT COUNT(*) AS n
         FROM refer.tr_prestataire_pre
         WHERE pre_siret = $1",
        params = list(pre_siret)
      )$n[1]
      
      if (!is.na(siret_exists) && siret_exists > 0) {
        output$add_status <- renderUI({
          tags$div(
            class = "alert alert-warning",
            tags$strong("SIRET déjà présent : "),
            sprintf("Le SIRET « %s » existe déjà dans refer.tr_prestataire_pre. Aucun enregistrement ajouté.", pre_siret)
          )
        })
        return(invisible(NULL))
      }
      
      # 2) (optionnel) éviter doublons par code SANDRE ou nom - avertissement
      exists <- DBI::dbGetQuery(
        pool,
        "SELECT COUNT(*) AS n
         FROM refer.tr_prestataire_pre
         WHERE pre_isa_codesandre = $1 OR lower(pre_nom) = lower($2)",
        params = list(pre_code, pre_nom)
      )$n[1]
      
      if (!is.na(exists) && exists > 0) {
        output$add_status <- renderUI({
          tags$div(
            class = "alert alert-warning",
            tags$strong("Doublon potentiel : "),
            "Un prestataire avec ce code SANDRE ou ce nom existe déjà."
          )
        })
        return(invisible(NULL))
      }
      
      # 3) Insertion
      DBI::dbExecute(
        pool,
        "INSERT INTO refer.tr_prestataire_pre (pre_nom, pre_siret, pre_isa_codesandre)
         VALUES ($1, $2, $3)",
        params = list(pre_nom, pre_siret, pre_code)
      )
      
      # 4) Rafraîchissement + message
      data_prestataires(fetch_prestataires())
      output$add_status <- renderUI({
        tags$div(
          class = "alert alert-success",
          tags$strong("Prestataire ajouté : "),
          sprintf("« %s » (SIRET: %s, SANDRE: %s) a été ajouté.", pre_nom, pre_siret, pre_code)
        )
      })
      
      # 5) Reset
      updateTextInput(session, "pre_nom", value = "")
      updateTextInput(session, "pre_siret", value = "")
      updateTextInput(session, "pre_isa_codesandre", value = "")
    })
  })
}
