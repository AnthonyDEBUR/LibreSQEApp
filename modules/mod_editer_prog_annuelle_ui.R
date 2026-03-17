
# ============================================================
# MODULE : Edition programmation annuelle (UI)
# Fichier : mod_editer_prog_annuelle_ui.R
# ============================================================

mod_editer_prog_annuelle_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    
    shinyjs::useShinyjs(),
    
    # --------------------------------------------------------
    # BARRE DE CONTROLE : Marché, Année, Filtres, Mode admin
    # --------------------------------------------------------
    fluidRow(
      column(
        width = 3,
        
        h3("Programmation annuelle"),
        tags$hr(),
        
        # ---- Sélection marché ----
        selectInput(ns("select_marche"), "Marché :", choices = NULL),
        
        # ---- Sélection année ----
        selectInput(ns("select_annee"), "Année :", choices = NULL),
        
        tags$hr(),
        
        # ---- Mode admin ----
        checkboxInput(
          ns("toggle_edit"),
          "Activer l’édition (réservé admin)",
          value = FALSE
        ),
        
        tags$hr(),
        
        # ---- Filtres intelligents ----
        h4("Filtres"),
        selectInput(ns("flt_perimetre"), "Périmètre", choices = "Tous", selected = "Tous"),
        selectInput(ns("flt_typestation"), "Type station", choices = "Tous", selected = "Tous"),
        selectInput(ns("flt_station"), "Station interne", choices = "Toutes", selected = "Toutes"),
        selectInput(ns("flt_presta"), "Prestation (PRS)", choices = "Toutes", selected = "Toutes"),
        selectInput(ns("flt_bco"), "Bon de commande", choices = "Tous", selected = "Tous"),
        dateInput(ns("flt_date_min"), "Date min", value = NA),
        dateInput(ns("flt_date_max"), "Date max", value = NA),        
        
        actionButton(ns("btn_reset_filtres"), "Réinitialiser filtres", icon = icon("rotate")),
        
        tags$hr(),
        
        # ---- Rechargement & Recalcul ----
        h4("Actions"),
        actionButton(ns("btn_refresh"), "Rafraîchir", icon = icon("rotate")),
        br(), br(),
        
        # ---- Recalcul complet depuis XLSX ----
        fileInput(
          ns("xlsx_recalc"),
          "Recalculer depuis un fichier XLSX",
          accept = c(".xlsx")
        ),
        selectInput(
          ns("xlsx_freq"),
          "Fréquence BDC",
          choices = c("mensuelle", "bimestrielle", "trimestrielle", "semestrielle", "annuelle")
        ),
        textInput(ns("xlsx_prefix"), "Préfixe BDC", value = ""),
        actionButton(ns("btn_recalc"), "Recalculer (admin)", icon = icon("bolt")),
        
        tags$hr(),
        
        # ---- Export Excel ----
        downloadButton(ns("btn_export_excel"), "Exporter en Excel (.xlsx)"),
        
        tags$hr(),
        
        # ---- Ajout / suppression ----
        h4("Édition"),
        uiOutput(ns("ctx_buttons")),
        
        tags$hr(),
        
        # ---- Diagnostic ----
        h4("Diagnostic"),
        actionButton(ns("btn_show_diag"), "Afficher le diagnostic", icon = icon("stethoscope"))
      ),
      
      # --------------------------------------------------------
      # PANNEAU PRINCIPAL : Tables
      # --------------------------------------------------------
      column(
        width = 9,
        
        tabsetPanel(
          id = ns("tabs"),
          
          tabPanel(
            "PGA",
            h4("Programme annuel (PGA)"),
            DT::DTOutput(ns("dt_pga"))
          ),
          
          tabPanel(
            "CAL",
            h4("Calendrier (CAL)"),
            DT::DTOutput(ns("dt_cal"))
          ),
          
          tabPanel(
            "BCO",
            h4("Bons de commande (BCO)"),
            DT::DTOutput(ns("dt_bco"))
          ),
          
          tabPanel(
            "BCQ",
            h4("Quantitatif (BCQ)"),
            DT::DTOutput(ns("dt_bcq"))
          ),
          
          tabPanel(
            "BCP",
            h4("Plan daté (BCP)"),
            DT::DTOutput(ns("dt_bcp"))
          ),
          
          tabPanel(
            "REA",
            h4("Résultats attendus (REA)"),
            DT::DTOutput(ns("dt_rea"))
          ),
          
          tabPanel(
            "Diagnostic",
            h4("Diagnostic complet de cohérence"),
            mod_editer_prog_annuelle_diag_ui(ns("diag"))
          )
        )
      )
    ),
    
    # --------------------------------------------------------
    # MODALES D’INSERTION (gestion dynamique par le serveur)
    # --------------------------------------------------------
    uiOutput(ns("modal_insert"))
  )
}

# ============================================================
# MODULE DIAGNOSTIC (UI WRAPPER)
# ============================================================

mod_editer_prog_annuelle_diag_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    h3("Diagnostic de cohérence"),
    p("Cet outil compare automatiquement les tables PGA, CAL, BCO, BCQ, BCP et REA."),
    br(),
    
    # Résumé global
    h4("Résumé"),
    DT::DTOutput(ns("resume_global")),
    
    hr(),
    
    # Sections détaillées
    h4("1. Cohérence PGA ↔ CAL"),
    DT::DTOutput(ns("diag_pga_cal")),
    hr(),
    
    h4("2. Cohérence CAL ↔ BCO"),
    DT::DTOutput(ns("diag_cal_bco")),
    hr(),
    
    h4("3. Cohérence BCO ↔ BCQ"),
    DT::DTOutput(ns("diag_bco_bcq")),
    hr(),
    
    h4("4. Cohérence BCO ↔ BCP"),
    DT::DTOutput(ns("diag_bco_bcp")),
    hr(),
    
    h4("5. Cohérence BCP ↔ REA"),
    DT::DTOutput(ns("diag_bcp_rea")),
    hr(),
    
    h4("6. Cohérence REA ↔ PGA"),
    DT::DTOutput(ns("diag_rea_pga"))
  )
}
