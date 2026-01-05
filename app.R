# Liste des packages CRAN nécessaires
cran_packages <- c("shiny", "leaflet", "lubridate", "dplyr", "yaml",
                   "RPostgres", "DBI", "sf", "DT", "readxl", "tidyverse",
                   "pool", "bslib")

# Fonction pour installer les packages CRAN manquants
install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

# Installation et chargement des packages CRAN
invisible(lapply(cran_packages, install_if_missing))

# Vérifie si remotes est installé pour installer depuis GitHub
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
library(remotes)

# Installation et chargement de meteo4Vilaine depuis GitHub si nécessaire
if (!requireNamespace("LibreSQE", quietly = TRUE)) {
  remotes::install_github("AnthonyDEBUR/LibreSQE")
}
library(LibreSQE)
library(shiny)
library(DBI)
library(pool)
library(DT)
library(readxl)
library(dplyr)
library(tidyverse)

# Charger la configuration
config <- yaml::read_yaml("yaml//config.yml")


connexion <- pool::dbPool(
  drv      = RPostgres::Postgres(),
  host     = config$host,
  port     = config$port,
  user     = config$user,
  password = config$password,
  dbname   = config$dbname,
  # (facultatif) paramètres de pool :
  minSize  = 1,     # nombre mini de connexions gardées chaudes
  maxSize  = 10,    # nombre maxi (à adapter à ta charge et aux limites serveur)
  idleTimeout  = 60 * 20,  # (sec) délai avant fermeture d'une connexion idle
  validationInterval = 60  # (sec) fréquence de validation des connexions
)

# --- Fermeture propre du pool à l'arrêt de l'app ---
onStop(function() {
  pool::poolClose(connexion)
})


##### Chargement des modules #####
# MARCHES - gestion des marchés
source("modules/mod_edition_marche_ui.R", local = TRUE)
source("modules/mod_edition_marche_server.R", local = TRUE)

# MARCHES - Import BPU & Programmes types
source("modules/mod_import_bpu_progtypes_ui.R",     local = TRUE)
source("modules/mod_import_bpu_progtypes_server.R", local = TRUE)


# REFERENTIELS
source("modules/mod_maj_referentiels_sandre.R", local = TRUE)
source("modules/mod_station_view.R", local = TRUE)

# REFERENTIELS - prestataires
source("modules/mod_prestataires_ui.R", local = TRUE)
source("modules/mod_prestataires_server.R", local = TRUE)




# temporaire

tableau_per_gest <-
  DBI::dbReadTable(connexion, DBI::Id(schema = "refer",
                                      table = "tr_perimetre_per"))


fichier_prog <-
  "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\R_Anthony\\libreSQE\\dev\\v2 prog EPTB Est_Ouest 2022 - commande_3 derniers trimestres_ajout suivis Captages_version dev libreSQE.xlsx"


# param_perimetre_facturation <- "UGVE"
# param_rattachement_bdc <- "pluie"
# param_mois <- "aout"

prog_annuelle <- read_xlsx(fichier_prog,
                           sheet = "programme_annuel",
                           col_types = "text")


calendrier <- read_xlsx(fichier_prog,
                        sheet = "calendrier")


programmes_types <- read_xlsx(fichier_prog,
                              sheet = "programmes_types")


BPU <- read_xlsx(fichier_prog,
                 sheet = "BPU")


cout_run_analytiques <- read_xlsx(fichier_prog,
                                  sheet = "cout_run_analytiques")


prog_previsionnelle <-
  left_join(prog_annuelle,
            calendrier,
            by = c("Type station" = "type de station"))


prog_previsionnelle <- pivot_longer(prog_previsionnelle,
                                    janvier:décembre,
                                    names_to = "mois",
                                    values_to = "quantite_commandee")

prog_previsionnelle <-
  prog_previsionnelle %>% subset(quantite_commandee > 0)


table_stat_analyses <-
  readRDS(
    "C:/workspace/LibreSQE/dev/prototype_interface_libreSQE/data/table_stat_analyses.rds"
  )
table_stat_analyses_toutes_staq <-
  readRDS(
    "C:/workspace/LibreSQE/dev/prototype_interface_libreSQE/data/table_stat_analyses_toutes_staq.rds"
  )


tableau_maj_ref <-
  DBI::dbReadTable(connexion, DBI::Id(schema = "sqe",
                                      table = "ts_suivi_maj_refer"))



# Define UI for application that draws a histogram
ui <- navbarPage(
  title=div(img(src="favicon.ico"), "Prototype de l'interface de libreSQE"),
  theme = bslib::bs_theme(
    version = 5,
    primary = "#00218f",
    success = "#33b5ff",
    info = "#00C9C4",
    warning = "#ffb428",
    base_font = "Segoe UI Symbol",
    heading_font = "Georgia",
    font_scale = NULL,
    `enable-gradients` = TRUE,
    bootswatch = "cerulean"
  ),
  tabPanel("Synthèse"),
  navbarMenu(
    ##### UI ANALYSES #####
    "Analyses",
    tabPanel("Mes analyses à qualifier",
             fluidRow(
               column(
                 2,
                 "Sélection du jeu de données",
                 selectInput(
                   "select_marche_prog_annuelle_a_importer",
                   "Sélectionnez le marché concerné",
                   choices = c(
                     "Marché labo n°1 - 2022",
                     "Marché labo n°2 - 2022",
                     "Marché labo n°1 - 2023-2025",
                     "Marché labo n°3 - 2023"
                   )
                 ),
                 selectInput(
                   "select_bdc",
                   "Sélectionnez le bon de commande concerné",
                   choices = c(
                     "2022-3_UGVO_pluie_avril_2022",
                     "2022-3_UGVO_calendaire_avril_2022"
                   )
                 ),
                 selectInput(
                   "select_depot",
                   "Sélectionnez le dépôt concerné",
                   choices = c("2022-12-01_dépôt1_carso")
                 ),
                 checkboxGroupInput(
                   "check_statut_depot_a_afficher",
                   label = "Statut des dépôts à afficher",
                   choices = c("en cours", "clôs"),
                   selected = 'en cours'
                 ),
                 checkboxGroupInput(
                   "check_qualification_analyses_a_afficher",
                   label = "Qualification des analyses à afficher",
                   choices = c("Correcte",
                               "Incorrecte",
                               "Incertaine",
                               "Non qualifié"),
                   selected = c("Incorrecte", "Incertaine")
                 ),
                 checkboxGroupInput(
                   "check_statut_analyses_a_afficher",
                   label = "Statut des analyses à afficher",
                   choices = c("Donnée brute",
                               "Niveau 1",
                               "Niveau 2")
                 )
               ),
               column(
                 8,
                 DTOutput("dt_analyses_a_qualifier"),
                 actionButton(
                   "btn_valide_selection",
                   "Valider l'ensemble des données affichées"
                 ),
                 actionButton(
                   "btn_invalide_selection",
                   "Invalider l'ensemble des données affichées"
                 ),
                 p(
                   "On sélectionne les lignes à requalifier à l'aide des filtres du tableau dt_analyses_a_qualifier.
          Si on clique sur une ligne alors bouton apparait pour affichage d'un graph avec le paramètre et son historique sur la station,
          un graph (boite à moustache avec ligne verticale ccorrespondant à la valeur d'analyse)
          avec la situation du résultat d'analyse dans la distribution à la station,
          à la station pour le même mois +/-1,
          pour toutes les stations,
          pour toutes les stations sur le même mois +/-1,
          "
                 ),
                 p(
                   "S'affiche également un bouton pour affichage du % de quantif pour le
          paramètre dans le dépôt vs reste des données (avec des classes par taux de quantif)"
                 ),
                 p(
                   "bouton pour afficher également les débits et données météo entre j-5 et j+2 par rapport à l'analyse
          (données geosas https://geosas.fr/geonetwork/srv/fre/catalog.search#/metadata/643fcbf3-d890-4836-bf62-1204c043bc81 et https://geosas.fr/simfen/"
                 ),
                 p(
                   "boutons pour accéder aux graphs précompilés de la station concernée de vilaine explorer
          sur PC, pesticides (1 et 2), autres polluants PC, polluants spé"
                 )
                 
               )
             )),
    tabPanel("Rechercher / éditer des analyses",
             fluidRow(
               column(
                 2,
                 "Sélection du jeu de données",
                 p(
                   "Constructeur de requête SQL avec choix (ou pas si non renseigné)
                        sur code station, code paramètre, code fraction, code unité,
                        date début, date fin prélèvement, dispositif de collecte, laboratoire,
                        préleveur, qualification, ..."
                 )
               ),
               column(
                 8,
                 DTOutput("dt_analyses_a_qualifier2"),
                 p(
                   "possibilité d'éditer certaines valeurs (résultat d'analyses, LQ, LD, code unité, ... dans le DT)"
                 ),
                 actionButton(
                   "btn_valide_selection",
                   "Valider l'ensemble des données affichées"
                 ),
                 actionButton(
                   "btn_invalide_selection",
                   "Invalider l'ensemble des données affichées"
                 ),
                 p(
                   "On sélectionne les lignes à requalifier à l'aide des filtres du tableau dt_analyses_a_qualifier.
          Si on clique sur une ligne alors bouton apparait pour affichage d'un graph avec le paramètre et son historique sur la station,
          un graph (boite à moustache avec ligne verticale ccorrespondant à la valeur d'analyse)
          avec la situation du résultat d'analyse dans la distribution à la station,
          à la station pour le même mois +/-1,
          pour toutes les stations,
          pour toutes les stations sur le même mois +/-1,
          "
                 ),
                 p(
                   "S'affiche également un bouton pour affichage du % de quantif pour le
          paramètre dans le dépôt vs reste des données (avec des classes par taux de quantif)"
                 ),
                 p(
                   "bouton pour afficher également les débits et données météo entre j-5 et j+2 par rapport à l'analyse
          (données geosas https://geosas.fr/geonetwork/srv/fre/catalog.search#/metadata/643fcbf3-d890-4836-bf62-1204c043bc81 et https://geosas.fr/simfen/"
                 ),
                 p(
                   "boutons pour accéder aux graphs précompilés de la station concernée de vilaine explorer
          sur PC, pesticides (1 et 2), autres polluants PC, polluants spé"
                 )
                 
               )
             ))
  ),
  ##### UI LIVRABLES #####
  navbarMenu(
    "Livrables",
    tabPanel(
      "Déposer un fichier xml / QUESU / EDILABO",
      selectInput(
        "select_bdc",
        "Sélectionnez le bon de commande concerné",
        choices = c(
          "2022-3_UGVO_pluie_avril_2022",
          "2022-3_UGVO_calendaire_avril_2022"
        )
      ),
      selectInput(
        "select_prestataire_emetteur_xml",
        "Emetteur xml",
        choices = c(
          "Prestataire en charge du marché 1",
          "Prestataire en charge du marché 2"
        )
      ),
      selectInput("select_mois",
                  "provisoire : mois à afficher",
                  choices = c(unique(
                    prog_previsionnelle$mois
                  ))),
      selectInput(
        "select_rattachement",
        "provisoire : rattachement devis",
        choices = c(unique(prog_previsionnelle$rattachement_devis))
      ),
      selectInput(
        "select_perimetre_fact",
        "provisoire : perimetre facturation",
        choices = c(unique(
          prog_previsionnelle$perimetre_facturation
        ))
      ),
      fileInput("import_xml",
                label = "Sélectionner fichier xml de résultats"),
      actionButton("btn_import_xml", "Déposer le fichier sélectionné sur le serveur"),
      actionButton("btn_actualise_bdc", "Actualiser traitement avec nlle réference bon de commande"),
      p(
        "formats acceptés : EDILABO, QUESU v2, QUESU v3, QUESU v3.1.
               Une fois importé le fichier fait l'objet des scripts de tests"
      )
    ),
    tabPanel(
      "Rapport de dépôt",
      selectInput(
        "select_bdc",
        "Sélectionnez le bon de commande concerné",
        choices = c(
          "2022-3_UGVO_pluie_avril_2022",
          "2022-3_UGVO_calendaire_avril_2022"
        )
      ),
      selectInput(
        "select_depot",
        "Sélectionnez le dépôt concerné",
        choices = c("2022-12-01_dépôt1_carso")
      ),
      h1("Synthèse du dépôt"),
      p(
        "Nb de lignes déposées, nb de stations concernées, tableau du nb de résultat classé correct, incertain , incorrect,
               nb de station manquant, nb de run analytiques incomplets, ..."
      ),
      h1("onglets qui détaillent les non conformités"),
      actionButton("btn_emettre_rapport_depot", "Emettre rapport de dépôt"),
      p(
        "boutton qui actionne la génèse d'un fichier excel qui liste les non conformités"
      ),
      actionButton("btn_clore_depot", "Clôtre le dépôt"),
      p(
        "Clore le dépôt ouvre une fenêtre qui indique qu'en clôturant le
               dépôt toutes les analyses qualifiée en niveau 1 (automatique)
               basculent en niveau 2. S'il reste des analyses incertaines,
               fenêtre d'alerte demandant de confirmer la clôture"
      ),
      tabsetPanel(
        tabPanel("nb données par station",
                 DTOutput("tbl_data_par_staq")),
        tabPanel("stations manquantes",
                 DTOutput("tbl_staq_missing")),
        tabPanel(
          "stations manquantes (cond env)",
          DTOutput("tbl_staq_missing_cond_env")
        ),
        tabPanel(
          "stations manquantes (opér)",
          DTOutput("tbl_staq_missing_oper")
        ),
        
        tabPanel(
          "Résultats analyses manquants",
          DTOutput("tbl_result_missing")
        ),
        tabPanel("Résultats analyses en +",
                 DTOutput("tbl_result_en_trop")),
        tabPanel(
          "Vérif du code dispositif de collecte",
          DTOutput("tbl_verif_rdd")
        ),
        tabPanel("Vérif du respect des LQ",
                 DTOutput("tbl_LQ_ko")),
        tabPanel("Vérif des accréditations",
                 DTOutput("tbl_accred_ko")),
        tabPanel("Vérif des incertitudes",
                 DTOutput("tbl_incert_ko")),
        tabPanel("Vérif des méthodes",
                 DTOutput("tbl_methode_ko"))
      )
      
    )
  ),
  ###### UI BDC #####
  navbarMenu(
    "Commandes",
    tabPanel(
      "Créer / éditer un bon de commande / émettre fichier EDILABO",
      selectInput(
        "select_marche_prog_annuelle_a_importer",
        "Sélectionnez le marché concerné",
        choices = c(
          "Marché labo n°1 - 2022",
          "Marché labo n°2 - 2022",
          "Marché labo n°1 - 2023-2025",
          "Marché labo n°3 - 2023"
        )
      ),
      selectInput(
        "select_perimetre_fact",
        "Sélectionnez le périmètre de facturation concerné",
        choices = c("UGVO",
                    "UGVE",
                    "UGVE-Pont Billon",
                    "UGVE Vallière")
      ),
      selectInput(
        "select_annee_facturation",
        "Sélectionnez l'année",
        choices = c("2022",
                    "2023",
                    "2024"),
        multiple = FALSE
      ),
      selectInput(
        "select_mois_facturation",
        "Sélectionnez le ou les mois concernés",
        choices = c(
          "janvier",
          "février",
          "mars",
          "avril",
          "mai",
          "juin",
          "juillet",
          "août",
          "septembre",
          "octobre",
          "novembre",
          "décembre"
        ),
        multiple = TRUE
      ),
      actionButton("btn_generer_bdc", "Générer le bon de commande"),
      p(
        "générer le bon de commande regarde si le bon de commande n'est pas préexistant.
               S'il n'est pas préexistant alors un tableau de synthèse est créé qui
               liste les prestations et le nombre attendu sur le bon de commande ainsi qu'un
               tableau détaillé qui liste par station chaque prélèvement attendu
               avec une date prévisionnelle (fixée au 1er du mois si une presta / mois,
               au 1er et au 15 si 2 prestas, au 1er, au 10 et au 20 si 3 prestas,
               au 1er, 7, 14 et 28 si 4 prestas, ...
               "
      ),
      h1("tableau de synthèse"),
      p(
        "En plus des informations affichées, le bon de commande comprends également le montant des prestations correspondantes.
               En dessous est affiché le montant total."
      ),
      DTOutput("DT_synthese_bdc"),
      p(
        "Si besoin on peut ajouter des prestations non directement validables : la fourniture de flacons,
               les réunions, les prélèvements d'eau calendaires ou pluie, ..."
      ),
      actionButton(
        "btn_ajouter_presta",
        "Ajouter des prestataions au bon de commande"
      ),
      h1("détail par station"),
      DTOutput("DT_detail_bdc"),
      actionButton(
        "btn_éditer_tableau de prestation",
        "Editer tableau de prestations"
      ),
      p(
        "le bouton permet d'éditer le tableau de prestation s'il faut ajouter ou supprimer une ou des stations,
               modifier des dates, ...
               En cas d'activation, la prog annuelle est également modifiée en conséquent"
      ),
      actionButton("btn_générer_bdc_pdf", "Générer le bon de commande en pdf"),
      actionButton(
        "btn_générer_demande_edilabo",
        "Générer demande prestation EDILABO"
      )
    ),
    tabPanel(
      "Avancement du bon de commande / facturation",
      selectInput(
        "select_marche_prog_annuelle_a_importer",
        "Sélectionnez le marché concerné",
        choices = c(
          "Marché labo n°1 - 2022",
          "Marché labo n°2 - 2022",
          "Marché labo n°1 - 2023-2025",
          "Marché labo n°3 - 2023"
        )
      ),
      p(
        "les choix du sélecteur de bon de commande affiche déjà les bdc non clôs puis ceux programmés et enfin ceux clôs "
      ),
      selectInput(
        "select_marche_bdc_a_importer",
        "Sélectionnez le bon de commande concerné",
        choices = c(
          "UGVE - Pont-Billon - Pluie - août 2022 (en cours)",
          "UGVE - Calendaire - avril 2022 (en cours)",
          "UGVE - Calendaire - mai 2022 (en cours)",
          "UGVE - Calendaire juin 2022 (programmé)",
          "UGVE - Calendaire janvier 2022 (clôs)"
        )
      ),
      checkboxGroupInput(
        "check_statut_bdc_a_afficher",
        label = "Statut des bons de commande à afficher",
        choices = c("programmé", "en cours", "clôs")
      ),
      p("Sortie texte qui affiche le statut du bon de commande"),
      actionButton("modifie_statut", "Modifier le statut du bon de commande"),
      textInput("commentaire_sur_bdc", "Commentaire lu dans la base bdc"),
      actionButton(
        "modifie_commentaire",
        "Modifier le commentaire du bon de commande"
      ),
      p(
        "tableau avec l'avancement du bon de commande par prestation (1 ligne par station / prestation avec un avancement)"
      ),
      p(
        "tableau de synthèse par prestation avec nombre commandé / en cours / validé"
      )
      
    )
  ),
  ###### UI Marchés #######
  navbarMenu(
    "Marchés",
    tabPanel(
      "Créer / éditer un marché",
      p(
        "Edition accessible seulement aux administrateurs. Pour les autres on ne peut que visualiser"
      ),
      mod_edition_marche_ui("marche")
    ),
    tabPanel(
      "Importer BPU et programmes types",
      mod_import_bpu_progtypes_ui("import_marche")
    ),
    tabPanel(
      "Modifier BPU",
      p(
        "Edition accessible seulement aux administrateurs. Pour les autres on ne peut que visualiser"
      ),
      selectInput(
        "select_marche_bpu_a_editer",
        "Sélectionnez le marché concerné",
        choices = c(
          "Marché labo n°1 - 2022",
          "Marché labo n°2 - 2022",
          "Marché labo n°1 - 2023-2025",
          "Marché labo n°3 - 2023"
        )
      ),
      selectInput(
        "select_annee_bpu_a_editer",
        "Sélectionnez l'année concernée (choix parmi les années entre le début et la fin du marché)",
        choices = c("2022")
      ),
      p(
        "Affichage les 3 tableaux BPU, BPU des run analytiques et programmes types.
        Possibilité d'éditer certains champs (et en cascade de mettre  à jour les tables correspondantes) :
        code / nom paramètres, dates, nb de presta commandées, ..."
      ),
      DTOutput("DT_bpu"),
      DTOutput("DT_bpu_run_analytiques"),
      DTOutput("DT_prog_types")
    ),
    tabPanel(
      "Importer programmation annuelle",
      p(
        "Edition accessible seulement aux administrateurs. Pour les autres on ne peut que visualiser"
      ),
      p(
        "la programmation annuelle comporte 2 onglets Excel : un avec la liste des stations et le code de leur programme annuel,
               et un avec le code des programmes annueles et les prestations et dates d'intervention correspondant"
      ),
      selectInput(
        "select_marche_prog_annuelle_a_importer",
        "Sélectionnez le marché concerné",
        choices = c(
          "Marché labo n°1 - 2022",
          "Marché labo n°2 - 2022",
          "Marché labo n°1 - 2023-2025",
          "Marché labo n°3 - 2023"
        )
      ),
      fileInput("import_pgm_annuelle",
                label = "Importer fichier xlsx programmation annuelle"),
      actionButton("btn_import_pgm_annuelle", "Importer le fichier sélectionné"),
      p(
        "une fois importé faire un rapport d'import qui indique si le fichier
               a bien été importé ou bien, s'il n'est pas conforme, les raisons de sa non conformité."
      )
    ),
    tabPanel(
      "Editer programmation annuelle",
      p(
        "Edition accessible seulement aux administrateurs. Pour les autres on ne peut que visualiser"
      ),
      selectInput(
        "select_marche_prog_annuelle_a_editer",
        "Sélectionnez le marché concerné",
        choices = c(
          "Marché labo n°1 - 2022",
          "Marché labo n°2 - 2022",
          "Marché labo n°1 - 2023-2025",
          "Marché labo n°3 - 2023"
        )
      ),
      selectInput(
        "select_annee_prog_a_editer",
        "Sélectionnez l'année concernée (choix parmi les années entre le début et la fin du marché)",
        choices = c("2022")
      ),
      p("Affichage de la prog annuelle"),
      DTOutput("DT_prog_annuelle_stations"),
      DTOutput("DT_prog_annuelle_programmation")
    ),
    tabPanel(
      "Avancement du marché",
      selectInput(
        "select_marche_avancement",
        "Sélectionnez le marché concerné",
        choices = c(
          "Marché labo n°1 - 2022",
          "Marché labo n°2 - 2022",
          "Marché labo n°1 - 2023-2025",
          "Marché labo n°3 - 2023"
        )
      ),
      p(
        "graph en barres avec montant max marché, montant prévu, montant commandé, montant payé"
      ),
      plotOutput("graph_avancement_financier", width = "650px"),
      p(
        "Graph avec avancement des prélèvements (nb commandé, nb réalisé, nb annulé assec, ..."
      ),
      plotOutput("graph_avancement_prelevement", width = "650px")
    ),
    tabPanel(
      "Editer un jeu de fiches stations",
      selectInput(
        "select_marche_fiches_stations",
        "Sélectionnez le marché concerné",
        choices = c(
          "Marché labo n°1 - 2022",
          "Marché labo n°2 - 2022",
          "Marché labo n°1 - 2023-2025",
          "Marché labo n°3 - 2023"
        )
      ),
      selectizeInput(
        "select_liste_stations_marche",
        "Liste des stations dont il faut éditer les fiches (choix parmi celles du marché actif)",
        choices = c(
          "Station 1 du marché actif",
          "Station 2 du marché actif",
          "Station 3 du marché actif",
          "Station 4 du marché actif"
        ),
        multiple = TRUE
      ),
      actionButton(
        "select_liste_stations_marche_all",
        "Sélectionner toutes les stations du marché"
      ),
      actionButton(
        "generer_fiche_stations_de_la_liste",
        "Générer les fiches des stations sélectionnées en pdf (1 pdf par station)"
      ),
      actionButton(
        "generer_fiche_stations_de_la_liste_tout_1_coup",
        "Générer les fiches des stations sélectionnées en pdf (1 pdf pour ensemble des stations)"
      )
      
    )
  ),
  ##### UI REFERENTIELS #####
  navbarMenu(
    "Référentiels",
    tabPanel(
      "Référentiels SANDRE",
      majrefSANDRE_UI("majrefs")
    ),
    tabPanel(
      "Visualiser / éditer fiches stations de mesures",
      stationViewUI("station"),
      h1("accès station"),
      verbatimTextOutput("text_acces_station"),
      actionButton(
        "btn_editer_text_acces",
        "Editer texte accès station",
        icon = icon("check")
      ),
      h1("Précisions sur prélèvement"),
      verbatimTextOutput("text_prelevement"),
      actionButton(
        "btn_editer_prelevement",
        "Editer précisions prélèvement",
        icon = icon("check")
      ),
      h1("Commentaires généraux sur la station"),
      verbatimTextOutput("text_commentaire_station"),
      actionButton(
        "btn_editer_commentaires",
        "Editer commentaires",
        icon = icon("check")
      ),
      h1("Photos"),
      p("1 photo + 1 légende par photo de la station"),
      actionButton(
        "btn_ajout_rempl_photo",
        "ajouter/ remplacer photo",
        icon = icon("check")
      ),
      p(
        "Si on appuie sur le bouton ajout / remplacer photo, ouverture d'une fenetre pour :
        - charger photo (jpg ou png),
        - ajouter légende,
        - indiquer photo à remplacer le cas échéant (liste déroulante + mini image),
        - bouton valider / annuler
        "
      )
    ),
    tabPanel(
      "Créer une nouvelle fiche stations de mesures",
      h3("Cours d'eau"),
      h3("Positionnement"),
      p("Selon le positionnement, afficher commune de localisation"),
      h3("Précisions sur accès"),
      h3('Précisions sur modes de prélèvements'),
      h3("Finalité de la station / autres commentaires"),
      h3("photos"),
      h3("Valider / annuler / Editer fiche en pdf"),
      h3("Préparer fiche demande création station AELB")
    ),
    tabPanel(
      "Périmètres de gestion",
      tabPanel(
        p(
          "Page visible uniquement si la personne connectée a le statut administrateur de marché."
        ),
        h1("Périmètres de gestion"),
        p(
          "Prévoir d'ajouter à la table un statut du périmètre (actif / inactif).
                 Seuls les périmètres actifs pourront être intégrés à un nouveau bon de commande."
        ),
        DTOutput("table_perimetres_de_gestion"),
        actionButton(
          "btn_maj_perimetre_gestion",
          "Modifier un périmètre de gestion",
          icon = icon("check")
        ),
        actionButton(
          "btn_ajouter_per_gest",
          "Ajouter un périmètre de gestion",
          icon = icon("check")
        ),
        actionButton(
          "btn_changer_statut_per_gest",
          "Changer statut périmètre de gestion",
          icon = icon("check")
        ),
      )
    ),
    tabPanel(
      "Rôles des intervenants",
      prestatairesUI("prestataires")
    )
   ),
  tabPanel(
    "Connexion",
    textInput("identifiant", "Saisir votre identifiant"),
    passwordInput("password", "Saisir votre mot de passe"),
    actionButton("btn_valide_mdp", "Valider", icon =
                   icon("check")),
    p(
      "En cas d'oubli du mot de passe, envoyer un mail à anthony.deburghrave@eaux-et-vilaine.fr"
    ),
    p(
      "Si l'opérateur est connecté, masquer les éléments de connexion et afficher les éléments suivants"
    ),
    verbatimTextOutput("text_connecte_comme"),
    actionButton("btn_deconnecter", "Se déconnecter", icon =
                   icon("xmark")),
  )
  
)


# Define server logic required to draw a histogram
server <- function(input, output) {

    
  
  ##### MARCHES #####
  
  mod_edition_marche_server(
    id = "marche",
    pool = connexion,
    schema_sqe   = "sqe",
    table_mar    = "t_marche_mar",
    schema_refer = "refer",
    table_presta = "tr_prestataire_pre",
    presta_id    = "pre_id",    # adapte si différent
    presta_label = "pre_nom",   # adapte si différent
    allow_multi  = FALSE,        # TRUE pour autoriser multi-titulaires dans l'UI
    pivot_table  = NULL         # mets par ex. "sqe.t_marche_titulaires" si tu as une table pivot (mar_id, pre_id)
  )
  
  
  
  
  mod_import_bpu_progtypes_server(
    id           = "import_marche",
    pool         = connexion,
    schema_sqe   = "sqe",
    can_edit     = TRUE, # authentification avec utilisateur autorisé à éditer les BPU
    auto_on_valid = TRUE  # TRUE = import auto si conforme et base vide
  )
  

  
  ##### REFERENTIELS #####
  
  majrefSANDRE_server("majrefs", connexion) # referentiels SANDRE
  stationViewServer("station", con = connexion, crs_xy = 2154)  # FICHE STATION
  prestatairesServer("prestataires", pool = connexion) # prestataires
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
