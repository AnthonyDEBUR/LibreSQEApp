mod_import_livrable_server <- function(id, pool) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # -------------------------------------------
    # 🔵 1. Charger la liste des marchés
    # -------------------------------------------
    observe({
      req(pool)
      marches <- DBI::dbGetQuery(pool,
                                 "SELECT mar_id, mar_nom 
         FROM sqe.t_marche_mar 
         ORDER BY mar_nom"
      )
      updateSelectInput(session, "marche",
                        choices = setNames(marches$mar_id, marches$mar_nom))
    })
    
    
    # -------------------------------------------
    # 🔵 2. Charger la liste filtrée des BDC selon marché
    # -------------------------------------------
    observeEvent(input$marche, {
      req(input$marche)
      
      req(pool)
      bdc <- DBI::dbGetQuery(pool,
                             sprintf(
                               "SELECT bco_id, bco_refcommande
           FROM sqe.t_boncommande_bco
           WHERE bco_mar_id = %s
           ORDER BY bco_id DESC",
                               input$marche
                             )
      )
      
      updateSelectInput(session, "bco",
                        choices = setNames(bdc$bco_id, bdc$bco_refcommande))
    })
    
    
    # -------------------------------------------
    # 🔵 3. Lancer le test
    # -------------------------------------------
    resultats <- reactiveVal(NULL)
    
    observeEvent(input$lancer_test, {
      req(input$fichier, input$bco)
      
      showModal(modalDialog("Analyse en cours...", footer = NULL))
      
      out <- tryCatch(
        {
          run_tests_quesu(
            fichier = input$fichier$datapath,
            bco_id  = input$bco,
            pool    = pool
          )
        },
        error = function(e) {
          removeModal()
          showNotification(paste("Erreur :", e$message),
                           type = "error", duration = NULL)
          
          updateTabsetPanel(session, "tabs_test", selected = "Non-conformités")
          
          return(NULL)
        }
      )
      
      removeModal()
      
      if (!is.null(out)) {
        resultats(out)
        showNotification("Test terminé avec succès", type = "message")
        updateTabsetPanel(session, "tabs_test", selected = "Résumé")
      }
    })
    
    
    # -------------------------------------------
    # 🔵 4. Résumé
    # -------------------------------------------
    output$resume <- renderPrint({
      req(resultats())
      r <- resultats()
      list(
        fichier          = r$Rapport$fichier,
        bon_de_commande  = r$Rapport$bon_de_commande,
        nb_lignes        = r$Rapport$nb_lignes
      )
    })
    
    
    # -------------------------------------------
    # 🔵 5. Construction dynamique des onglets
    # -------------------------------------------
    output$ui_nonconf <- renderUI({
      req(resultats())
      r <- resultats()$Rapport
      
      onglets <- lapply(names(r), function(nom) {
        df <- r[[nom]]
        if (is.data.frame(df) && nrow(df) > 0) {
          tabPanel(nom, DT::dataTableOutput(ns(paste0("tbl_", nom))))
        } else {
          NULL
        }
      })
      
      do.call(tabsetPanel, onglets)
    })
    
    
    observe({
      req(resultats())
      r <- resultats()$Rapport
      
      for (nom in names(r)) {
        if (is.data.frame(r[[nom]]) && nrow(r[[nom]]) > 0) {
          local({
            nom_loc <- nom
            output[[paste0("tbl_", nom_loc)]] <- DT::renderDataTable({
              DT::datatable(r[[nom_loc]], options = list(pageLength = 10))
            })
          })
        }
      }
    })
    
    
    # -------------------------------------------
    # 🔵 6. Table analyses
    # -------------------------------------------
    output$table_analyses <- DT::renderDataTable({
      req(resultats())
      DT::datatable(resultats()$Analyses, options = list(pageLength = 10))
    })
    
    
    # -------------------------------------------
    # 🔵 7. Export Excel
    # -------------------------------------------
    output$dl_rapport <- downloadHandler(
      filename = function() {
        paste0("Rapport_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        r <- resultats()$Rapport
        wb <- openxlsx::createWorkbook()
        
        for (i in names(r)) {
          if (is.data.frame(r[[i]]) && nrow(r[[i]]) > 0) {
            openxlsx::addWorksheet(wb, i)
            openxlsx::writeData(wb, i, r[[i]])
          }
        }
        
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      }
    )
    
  })
}




