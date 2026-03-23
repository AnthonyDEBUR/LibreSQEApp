##### Fonctions pour tester les xml #####

simple_test_function <- function(fichier, bon_de_commande_id, connexion) {
  library(LibreSQE)
  library(openxlsx)
  library(stringi) # pour comparer les noms de stations sans prendre en compte les accents
  library(readr) # pour lire csv conversion unites
  
  #ajouter tests cohérence NGL / NH4 / NO2 / NO3 / NTK
  # ajouter tests sur valeurs numériques des paramètres environnementaux (pression atmo)
  # en cas de correction d'unités (ex. fichiers EPH), il faut réenregistrer un nouvel xml en remplacement de celui d'origine
  
    table_stat_analyses <- readRDS("data/table_stat_analyses.rds")
    
    table_stat_analyses_toutes_staq <- readRDS("data/table_stat_analyses_toutes_staq.rds")
    
   conversion_unites <-
           readr::read_delim("data/conversion_unites.csv",
                             delim = ";",
                             col_types = readr::cols(
                               CdUniteIn  = readr::col_character(),
                               CdUniteOut = readr::col_character()
                             ))
    
  
  ##### chargement des analyses attendues pour le bon de commande #####
  # Écrire une requête SQL qui utilise la clause IN pour filtrer les résultats
  requete <-
    paste(
      "SELECT * FROM sqe.t_resultatanalyse_rea WHERE res_bco_id IN (",
      bon_de_commande_id,
      ")",
      sep = ""
    )
  
  # Exécuter la requête en utilisant la connexion à la base de données
  analyses_attendues <- DBI::dbGetQuery(connexion, requete)
  
  analyses_attendues <- analyses_attendues %>%
    subset(res_stm_cdstationmesureinterne != "sans_objet")
  
  analyses_attendues$nomparametre <- func_ajoute_nom_sandre(connexion,
                                                            code = analyses_attendues$rea_par_cdparametre,
                                                            out = "nom_long_parametre")
  
  analyses_attendues$nomfraction <- func_ajoute_nom_sandre(connexion,
                                                           code = analyses_attendues$rea_cdfractionanalysee,
                                                           out = "nom_fraction")
  
  analyses_attendues$nompreleveur <- func_ajoute_nom_sandre(connexion,
                                                            code = analyses_attendues$rea_cdpreleveur,
                                                            out = "nom_intervenant")
  
  analyses_attendues$nomlabo <- func_ajoute_nom_sandre(connexion,
                                                       code = analyses_attendues$rea_cdlaboratoire,
                                                       out = "nom_intervenant")
  
  analyses_attendues$insitu <- func_ajoute_nom_sandre(connexion,
                                                      code = analyses_attendues$rea_cdlaboratoire,
                                                      out = "insitu")
  
  ##### référence bon de commande #####
  # Écrire une requête SQL qui utilise la clause IN pour filtrer les résultats
  requete <-
    paste(
      "SELECT bco_refcommande FROM sqe.t_boncommande_bco WHERE bco_id IN (",
      bon_de_commande_id,
      ")",
      sep = ""
    )
  
  # Exécuter la requête en utilisant la connexion à la base de données
  ref_bon_de_commande <- DBI::dbGetQuery(connexion, requete)
  
  
  ##### initialisation du rapport d'import #####
  Rapport <- list()
  
  ##### Import et mise en forme du fichier à tester #####
  test <- func_importe_quesu(fichier)
  Analyses <- test$Analyses
  Stations <- Analyses %>%
    dplyr::select(CdStationMesureEauxSurface, LbStationMesureEauxSurface) %>%
    unique()
  Res_env <- test$Res_env
  Echantillon <- test$Echantillon
  Operation <- test$Operation
  rm(test)
  
  
  ##### Synthèse rapport #####
  Rapport$fichier <- fichier
  Rapport$bon_de_commande <- ref_bon_de_commande
  Rapport$nb_lignes <- data.frame(c("Analyses", "Stations", "ResEnv"),
                                  c(nrow(Analyses), nrow(Stations), nrow(Res_env)))
  
  
  ###### conversion des données ######
  # Analyses
  Analyses <- dplyr::rename(
    Analyses,
    cdstationmesureinterne = CdStationMesureEauxSurface,
    codeprel = CdPrelevement,
    dateprel = DatePrel,
    cdsupport = CdSupport,
    cdfractionanalysee = CdFractionAnalysee,
    heureprel = HeurePrel,
    dateana = DateAna,
    heureana = HeureAna,
    cdparametre = CdParametre,
    rsana = RsAna,
    cdunitemesure = CdUniteMesure,
    cdrqana = CdRqAna,
    cdinsituana = CdInsituAna,
    profondeurpre = ProfondeurPrel,
    ldana = LDAna,
    lqana = LQAna,
    lsana = LSAna,
    cdmetfractionnement = CdMetFractionnement,
    cdmethode = CdMethode,
    rdtextraction = RdtExtraction,
    cdmethodeextraction = CdMethodeExtraction,
    cdaccreana = CdAccreAna,
    agreana = AgreAna,
    incertitude = IncertAna,
    commentairesana = CommentairesAna,
    cdrdd = CdRdd,
    cdpreleveur = CdPreleveur,
    cdlaboratoire = CdLaboratoire,
    LbParametre = LbParametre,
    LbFraction = LbFractionAnalysee,
    LbSupport = LbSupport,
    LbMethode = LbMethode,
    SymUnite = LbUniteMesure,
    cdproducteur = CdProducteur
  )
  
  
  Analyses$rsana <- as.numeric(Analyses$rsana)
  Analyses$lqana <- as.numeric(Analyses$lqana)
  Analyses$lsana <- as.numeric(Analyses$lsana)
  Analyses$ldana <- as.numeric(Analyses$ldana)
  Analyses$incertitude <- as.numeric(Analyses$incertitude)
  
  
  
  # Ajout des noms paramètres, fraction, symboles unités, nom labo, noms Rdd
  Analyses$nomparametre <-
    func_ajoute_nom_sandre(connexion = connexion,
                           code = Analyses$cdparametre,
                           out = "nom_parametre")
  
  Analyses$nomlongparametre <-
    func_ajoute_nom_sandre(connexion = connexion,
                           code = Analyses$cdparametre,
                           out = "nom_long_parametre")
  
  Analyses$unite <- func_ajoute_nom_sandre(connexion = connexion,
                                           code = Analyses$cdunitemesure,
                                           out = "nom_unite")
  
  Analyses$nomfraction <- func_ajoute_nom_sandre(
    connexion = connexion,
    code = Analyses$cdfractionanalysee,
    out = "nom_fraction"
  )
  
  Analyses$nomlabo <- func_ajoute_nom_sandre(connexion = connexion,
                                             code = Analyses$cdlaboratoire,
                                             out = "nom_long_intervenant")
  
  Analyses$nompreleveur <-
    func_ajoute_nom_sandre(connexion = connexion,
                           code = Analyses$cdpreleveur,
                           out = "nom_long_intervenant")
  
  Analyses$insitu <- func_ajoute_nom_sandre(connexion = connexion,
                                            code = Analyses$cdinsituana,
                                            out = "insitu")
  
  Analyses$station <- func_ajoute_nom_sandre(
    connexion = connexion,
    code = Analyses$cdstationmesureinterne,
    out = "nom_station"
  )
  
  
  func_noms_codes <- function(codesrdd_vecteur) {
    # Divise chaque élément du vecteur en entrée en un sous-vecteur de codes
    codes_separe <- strsplit(codesrdd_vecteur, "/")
    
    # Applique la fonction func_ajoute_nom_sandre à chaque sous-vecteur de codes
    noms_codes <-
      lapply(codes_separe, function(x)
        func_ajoute_nom_sandre(
          connexion = connexion,
          code =
            x,
          out =
            "nom_rdd"
        ))
    
    # Concatène les noms de code séparés par des slashs à nouveau
    noms_codes_concatenes <- sapply(noms_codes, paste, collapse = "/")
    
    return(noms_codes_concatenes)
  }
  
  Analyses$nomrdd <- func_noms_codes(Analyses$cdrdd)
  
  
  # Res_env
  Res_env <- dplyr::rename(
    Res_env,
    cdstationmesureinterne = CdStationMesureEauxSurface,
    codeprel = CdPrelevement,
    dateprel = DateParEnv,
    cdparametre = CdParametreEnv,
    rsparenv = RsParEnv,
    cdunitemesure = CdUniteMesure,
    cdrqparenv = CdRqParEn,
    dateparenv = DateParEnv,
    met_code = CdMethodeParEnv,
    cdpreleveur = CdPreleveur,
    cdproducteur = CdProducteur,
    LbParametre = LbParametreEnv
  )
  
  ##### création tableau intervenants #####
  Intervenants_anal<-Analyses%>%
    dplyr::select(cdproducteur,
                  LbProducteur,
                  cdpreleveur,
                  LbPreleveur,
                  cdlaboratoire,
                  LbLaboratoire)%>%
    unique()
  
  # Création des trois data.frames en regroupant les colonnes
  df_producteur <- Intervenants_anal %>%
    dplyr::select(cdproducteur, LbProducteur) %>%
    dplyr::rename(CdIntervenant = cdproducteur, NomIntervenant = LbProducteur) %>%
    dplyr::mutate(role = "producteur")
  
  df_preleveur <- Intervenants_anal %>%
    dplyr::select(cdpreleveur, LbPreleveur) %>%
    dplyr::rename(CdIntervenant = cdpreleveur, NomIntervenant = LbPreleveur) %>%
    dplyr::mutate(role = "preleveur")
  
  df_laboratoire <- Intervenants_anal %>%
    dplyr::select(cdlaboratoire, LbLaboratoire) %>%
    dplyr::rename(CdIntervenant = cdlaboratoire, NomIntervenant = LbLaboratoire) %>%
    dplyr::mutate(role = "laboratoire")
  
  # Empilement des trois data.frames
  Intervenants <- dplyr::bind_rows(df_producteur, df_preleveur, df_laboratoire)
  
  rm(df_producteur, df_preleveur, df_laboratoire, Intervenants_anal)
  
  Intervenants_Operation<-Operation%>%
    dplyr::select(CdProducteur,
                  LbProducteur,
                  CdPreleveur,
                  LbPreleveur)%>%
    unique()
  
  df_producteur <- Intervenants_Operation %>%
    dplyr::select(CdProducteur, LbProducteur) %>%
    dplyr::rename(CdIntervenant = CdProducteur, NomIntervenant = LbProducteur) %>%
    dplyr::mutate(role = "producteur")
  
  df_preleveur <- Intervenants_Operation %>%
    dplyr::select(CdPreleveur, LbPreleveur) %>%
    dplyr::rename(CdIntervenant = CdPreleveur, NomIntervenant = LbPreleveur) %>%
    dplyr::mutate(role = "preleveur")
  
  # Empilement des data.frames
  Intervenants <- dplyr::bind_rows(df_producteur, df_preleveur, Intervenants)%>%unique()
  rm(df_producteur, df_preleveur, Intervenants_Operation)
  
  
  
  
  ##### test cohérence code / nom station #####
  # on ne tient pas compte de la casse ni des accents
  
  normalize_string <- function(string) {
    string %>%
      tolower() %>%  # Convertir en minuscules
      stringi::stri_trans_general("Latin-ASCII") %>%  # Supprimer les accents
      gsub("-", "", .)  # Supprimer les tirets
  }
  
  Stations$LbStationMesureEauxSurface_attendu <-
    func_ajoute_nom_sandre(connexion ,
                           code = Stations$CdStationMesureEauxSurface,
                           out = "nom_station")
  Stations$LbStationConforme <-
    ifelse(
      normalize_string(Stations$LbStationMesureEauxSurface) == normalize_string(Stations$LbStationMesureEauxSurface_attendu),
      TRUE,
      FALSE
    )
  Stations <- dplyr::rename(
    Stations,
    cdstationmesureinterne = CdStationMesureEauxSurface,
    rendu_dans_xml = LbStationMesureEauxSurface,
    attendu = LbStationMesureEauxSurface_attendu
  )
  
  
  Rapport$nom_cd_station <- unique(
    Stations %>%
      subset(
        LbStationConforme == FALSE &
          !is.na(rendu_dans_xml) & rendu_dans_xml != ""
      ) %>%
      dplyr::select(cdstationmesureinterne,
                    rendu_dans_xml,
                    attendu)
  )
  
  ##### test de renseignement des codes préleveurs, code laboratoire et code producteur #####
  CdProducteurManquant <- Analyses %>%
    subset(is.na(cdproducteur) | cdproducteur=="")
  
  if (nrow(CdProducteurManquant) > 0) {
    Rapport$CdProducteurManquant <- CdProducteurManquant
  }
  
  CdPreleveurManquant <- Analyses %>%
    subset(is.na(cdpreleveur) | cdpreleveur=="")
  
  if (nrow(CdPreleveurManquant) > 0) {
    Rapport$CdPreleveurManquant <- CdPreleveurManquant
  }
  
  CdLaboManquant <- Analyses %>%
    subset(is.na(cdlaboratoire) | cdlaboratoire=="")
  
  if (nrow(CdLaboManquant) > 0) {
    Rapport$CdLaboManquant <- CdLaboManquant
  }
  
  
  
  ##### test du bon renseignement de la LqAna (sauf T° réception échantillon) #####
  LqManquante <- Analyses %>%
    subset(cdrqana %in% c("1", "10", "7") &
             !cdparametre == "8727" &
             is.na(lqana) & !cdinsituana == "1")
  
  if (nrow(LqManquante) > 0) {
    Rapport$LqManquante <- LqManquante
  }
  
  ##### test du bon renseignement de la RsAna #####
  RsAnaManquants <- Analyses %>%
    subset(cdrqana %in% c("1", "10", "7") & is.na(rsana))
  
  if (nrow(RsAnaManquants) > 0) {
    Rapport$RsAnaManquants <- RsAnaManquants
  }
  
  
  ##### test cohérence code / nom paramètre (analyses et Res_enc) #####
  #####Analyses #####
  Parametres <- Analyses %>%
    dplyr::select(cdparametre, LbParametre) %>%
    dplyr::distinct()
  cdparametre <-
    paste0("'", unique(Parametres$cdparametre), "'", collapse = ", ")
  requete <-
    paste(
      "SELECT * FROM refer.tr_parametre_par WHERE par_cdparametre IN (",
      cdparametre,
      ")",
      sep = ""
    )
  
  refer_param <- DBI::dbGetQuery(connexion, requete)
  Parametres <-
    dplyr::left_join(Parametres,
                     refer_param,
                     by = c("cdparametre" = "par_cdparametre"))
  Parametres$LbParametreConforme <-
    ifelse(
      Parametres$LbParametre == Parametres$par_nomparametre |
        Parametres$LbParametre == Parametres$par_nomcourt,
      TRUE,
      FALSE
    )
  
  Parametres <- dplyr::rename(
    Parametres,
    rendu_dans_xml = LbParametre,
    attendu = par_nomparametre,
    nom_court_attendu = par_nomcourt,
    statut_parametre_dans_SANDRE = par_statutparametre
  )
  
  
  Rapport$nom_cd_parametre_analyses <- Parametres %>%
    subset((
      LbParametreConforme == FALSE &
        !is.na(rendu_dans_xml) &
        rendu_dans_xml != ""
    ) |
      statut_parametre_dans_SANDRE != "Validé"
    ) %>%
    dplyr::select(
      cdparametre,
      rendu_dans_xml,
      attendu,
      nom_court_attendu,
      statut_parametre_dans_SANDRE
    )
  
  #### Res_env #####
  Parametres <- Res_env %>%
    dplyr::select(cdparametre, LbParametre) %>%
    dplyr::distinct()
  cdparametre <-
    paste0("'", unique(Parametres$cdparametre), "'", collapse = ", ")
  requete <-
    paste(
      "SELECT * FROM refer.tr_parametre_par WHERE par_cdparametre IN (",
      cdparametre,
      ")",
      sep = ""
    )
  
  refer_param <- DBI::dbGetQuery(connexion, requete)
  Parametres <-
    dplyr::left_join(Parametres,
                     refer_param,
                     by = c("cdparametre" = "par_cdparametre"))
  Parametres$LbParametreConforme <-
    ifelse(
      Parametres$LbParametre == Parametres$par_nomparametre |
        Parametres$LbParametre == Parametres$par_nomcourt,
      TRUE,
      FALSE
    )
  
  Parametres <- dplyr::rename(
    Parametres,
    rendu_dans_xml = LbParametre,
    attendu = par_nomparametre,
    statut_parametre_dans_SANDRE = par_statutparametre
  )
  
  
  Rapport$nom_cd_parametre_res_env <- Parametres %>%
    subset((
      LbParametreConforme == FALSE &
        !is.na(rendu_dans_xml) &
        rendu_dans_xml != ""
    ) |
      statut_parametre_dans_SANDRE != "Validé"
    ) %>%
    dplyr::select(cdparametre,
                  rendu_dans_xml,
                  attendu,
                  statut_parametre_dans_SANDRE)
  
  
  
  ##### test cohérence code fraction / nom fraction #####
  #####Analyses #####
  Fraction <- Analyses %>%
    dplyr::select(cdfractionanalysee, LbFraction) %>%
    dplyr::distinct()
  cdfraction <-
    paste0("'", unique(Fraction$cdfractionanalysee), "'", collapse = ", ")
  requete <-
    paste("SELECT * FROM refer.tr_fraction_fra WHERE fra_codefraction IN (",
          cdfraction,
          ")",
          sep = "")
  
  refer_frac <- DBI::dbGetQuery(connexion, requete)
  Fraction <-
    dplyr::left_join(Fraction,
                     refer_frac,
                     by = c("cdfractionanalysee" = "fra_codefraction"))
  Fraction$LbFractionConforme <-
    ifelse(Fraction$LbFraction == Fraction$fra_nomfraction,
           TRUE,
           FALSE)
  
  Fraction <- dplyr::rename(Fraction,
                            rendu_dans_xml = LbFraction,
                            attendu = fra_nomfraction)
  
  
  Rapport$nom_cd_fraction_analyses <- Fraction %>%
    subset(LbFractionConforme == FALSE &
             !is.na(rendu_dans_xml) &
             rendu_dans_xml != "")
  
  ##### test cohérence code unité / nom unité #####
  Unites <- Analyses %>%
    dplyr::select(cdunitemesure, SymUnite) %>%
    dplyr::distinct()
  cdunitemesure <-
    paste0("'", unique(Unites$cdunitemesure), "'", collapse = ", ")
  requete <-
    paste(
      "SELECT * FROM refer.tr_uniteparametre_uni WHERE uni_codesandreunite IN (",
      cdunitemesure,
      ")",
      sep = ""
    )
  
  refer_unit <- DBI::dbGetQuery(connexion, requete)
  Unites <-
    dplyr::left_join(Unites,
                     refer_unit,
                     by = c("cdunitemesure" = "uni_codesandreunite"))
  Unites$LbUniteConforme <-
    ifelse(Unites$SymUnite == Unites$uni_symbole,
           TRUE,
           FALSE)
  
  Unites <- dplyr::rename(
    Unites,
    Lb_rendu_dans_xml = SymUnite,
    Lb_attendu = uni_symbole
  )
  
  
  Rapport$nom_cd_unites_analyses <- Unites %>%
    subset((
      LbUniteConforme == FALSE &
        !is.na(Lb_rendu_dans_xml) &
        Lb_rendu_dans_xml != ""
    )
    ) %>%
    dplyr::select(cdunitemesure,
                  Lb_rendu_dans_xml,
                  Lb_attendu)
  
  
  ##### test cohérence code / nom intervenants #####
  cdintervenants <-
    paste0("'", unique(Intervenants$CdIntervenant), "'", collapse = ", ")
  requete <-
    paste(
      "SELECT * FROM refer.tr_intervenantsandre_isa WHERE isa_codesandre IN (",
      cdintervenants,
      ")",
      sep = ""
    )
  
  refer_interv <- DBI::dbGetQuery(connexion, requete)
  Intervenants <-
    dplyr::left_join(Intervenants,
                     refer_interv,
                     by = c("CdIntervenant" = "isa_codesandre"))
  Intervenants$NomIntervenantConforme <-
    ifelse(Intervenants$NomIntervenant == Intervenants$isa_nom,
           TRUE,
           FALSE)
  
  Intervenants <- dplyr::rename(
    Intervenants,
    Nom_rendu_dans_xml = NomIntervenant,
    Nom_attendu = isa_nom
  )
  
  
  Rapport$nom_mnemo_intervenants <- Intervenants %>%
    subset((
      NomIntervenantConforme == FALSE &
        !is.na(Nom_rendu_dans_xml) &
        Nom_rendu_dans_xml != ""
    )
    ) %>%
    dplyr::select(CdIntervenant,
                  Nom_rendu_dans_xml,
                  Nom_attendu)
  
  ##### test cohérence format dates + date analyse >= date prélèvement (idem date heure) #####
  Analyses$dateprel_date<-as.Date(Analyses$dateprel, format="%Y-%m-%d")
  Analyses$dateana_date<-as.Date(Analyses$dateana, format="%Y-%m-%d")
  Analyses$heureprel_date<-as.POSIXct(Analyses$heureprel, format="%Y-%m-%dT%H:%M:%S")
  Analyses$heureana_date<-as.POSIXct(Analyses$heureana, format="%Y-%m-%dT%H:%M:%S")
  
  Rapport$dateprel_format <- Analyses[is.na(Analyses$dateprel_date),]%>%dplyr::select("cdstationmesureinterne",
                                                                                      "LbStationMesureEauxSurface",
                                                                                      "dateprel")%>%unique()
  Rapport$dateana_format <- Analyses[is.na(Analyses$dateana_date),]
  Rapport$heureprel_format <- Analyses[is.na(Analyses$heureprel_date),]%>%dplyr::select("cdstationmesureinterne",
                                                                                        "LbStationMesureEauxSurface",
                                                                                        "heureprel")%>%unique()
  Rapport$heureana_format <- Analyses[is.na(Analyses$heureana_date),]
  Rapport$incoherence_dates <- Analyses[Analyses$dateprel_date>Analyses$dateana_date,]
  Rapport$incoherence_heures <- Analyses[Analyses$heureprel_date>Analyses$heureana_date,]
  
  Res_env$dateparenv_date<-as.Date(Res_env$dateparenv, format="%Y-%m-%d")
  Res_env$heureparenv_date<-as.POSIXct(Res_env$HeureParEnv, format="%Y-%m-%dT%H:%M:%S")
  
  Rapport$dateparaenv_format <- Res_env[is.na(Res_env$dateparenv_date),]
  Rapport$heureparaenv_format <- Res_env[is.na(Res_env$heureparenv_date),]
  
  Analyses<-Analyses%>%
    dplyr::select(-dateprel_date, -dateana_date, -heureprel_date, -heureana_date)
  
  Res_env<-Res_env%>%
    dplyr::select(-dateparenv_date, -heureparenv_date)
  
  ##### test date / heure res_env dans dates de prélèvement #####
  
  # Filtrer les lignes uniques de Res_env et Analyses pour la jointure anti
  Res_env_unique <- Res_env %>% dplyr::distinct(cdstationmesureinterne, dateparenv, codeprel)
  Analyses_unique <- Analyses %>% dplyr::distinct(cdstationmesureinterne, dateprel, codeprel)
  
  # Effectuer la jointure anti
  Rapport$dateResEnv_diff_datePrel <- dplyr::anti_join(Res_env_unique, Analyses_unique,
                                                       by = c("cdstationmesureinterne" = "cdstationmesureinterne",
                                                              "dateparenv" = "dateprel",
                                                              "codeprel"="codeprel"))
  
  rm(Res_env_unique, Analyses_unique)
  
  # Filtrer les lignes uniques de Res_env et Analyses pour la jointure anti
  Res_env_unique <- Res_env %>% dplyr::distinct(cdstationmesureinterne, dateparenv, HeureParEnv, codeprel)
  Analyses_unique <- Analyses %>% dplyr::distinct(cdstationmesureinterne, dateprel, heureprel, codeprel)
  
  # Effectuer la jointure anti
  Rapport$heureResEnv_diff_heurePrel <- dplyr::anti_join(Res_env_unique, Analyses_unique,
                                                         by = c("cdstationmesureinterne" = "cdstationmesureinterne",
                                                                "dateparenv" = "dateprel",
                                                                "HeureParEnv"="heureprel",
                                                                "codeprel"="codeprel"
                                                         ))
  
  rm(Res_env_unique, Analyses_unique)
  
  
  ##### test cohérence valeur résultat, LQ, code remarque #####
  incoherence_cd_rq <-
    (
      ifelse(
        Analyses$cdrqana == "10" &
          Analyses$rsana > Analyses$lqana,
        TRUE,
        FALSE
      ) |
        ifelse(
          Analyses$cdrqana == "1" &
            Analyses$rsana < Analyses$lqana,
          TRUE,
          FALSE
        )
    ) &
    ifelse(is.na(Analyses$lqana) | is.na(Analyses$rsana), FALSE, TRUE)
  
  Rapport$cd_rq_ou_lq_incoherent <- Analyses[incoherence_cd_rq,]
  
  
  
  ##### test cohérence O2 satO2 temp #####
  
  analyses_a_tester <-
    Analyses %>% subset(cdparametre %in% c("1301", "1311", "1312") &
                          cdsupport == "3" &
                          cdinsituana == "1") %>% unique()
  
  if (nrow(analyses_a_tester) > 0) {
    analyses_a_tester <-
      analyses_a_tester %>% tidyr::pivot_wider(
        id_cols = c(
          cdstationmesureinterne,
          dateprel,
          heureprel,
          codeprel,
          cdrdd
        ),
        names_from = cdparametre,
        names_prefix = "p",
        values_from = rsana,
        values_fn = max
      )
    
    
    #on teste seulement si on a des données à la fois sur O2 dissous, sat O2 et température
    if (all(c("p1311", "p1312", "p1301") %in% names(analyses_a_tester)))
    {
      analyses_a_tester <-
        analyses_a_tester %>% subset(!is.na(p1301) &
                                       !is.na(p1311) & !is.na(p1312))
      
      fct_test_O2 <-
        function(x) {
          func_test_metier_coherenceO2(O2 = analyses_a_tester[["p1311"]][x],
                                       satO2 = analyses_a_tester[["p1312"]][x],
                                       temp = analyses_a_tester[["p1301"]][x])
        }
      
      fct_test_O2_val <-
        function(x) {
          func_test_metier_coherenceO2(
            O2 = analyses_a_tester[["p1311"]][x],
            satO2 = analyses_a_tester[["p1312"]][x],
            temp = analyses_a_tester[["p1301"]][x],
            export = "value"
          )
        }
      
      analyses_a_tester$testO2 <-
        sapply(seq_along(analyses_a_tester$cdstationmesureinterne),
               FUN = fct_test_O2)
      analyses_a_tester$test_satO2_attendu <-
        sapply(seq_along(analyses_a_tester$cdstationmesureinterne),
               FUN = fct_test_O2_val)
      
      analyses_a_tester <-
        analyses_a_tester %>% dplyr::rename(O2 = p1311,
                                            satO2 = p1312,
                                            temperature = p1301)
      
      analyses_a_tester$testO2 <- analyses_a_tester$testO2 %>%
        dplyr::recode_values(
          "1" ~ "correcte",
          "2" ~ "incertain",
          "3" ~ "incorrect"
        )
      
      analyses_a_tester$test_satO2_attendu <-
        round(analyses_a_tester$test_satO2_attendu, 1)
      
      
      Rapport$coherence_O2_temp <-
        analyses_a_tester %>% subset(testO2 != "correcte")
    }
  }
  
  ##### test cohérence Ptot PO4 #####
  
  # Cd Sandre PO4 = 1433
  # Cd Sandre Ptot = 1350
  
  analyses_a_tester <-
    Analyses %>% subset((cdparametre == "1350" &
                           cdfractionanalysee == "23") |
                          (cdparametre == "1433" &
                             cdsupport == "3")
    ) %>% unique()
  
  if (nrow(analyses_a_tester) > 0)
  {
    incertitudes_a_tester <-
      analyses_a_tester %>% tidyr::pivot_wider(
        id_cols = c(
          cdstationmesureinterne,
          dateprel,
          heureprel,
          codeprel,
          cdrdd
        ),
        names_from = cdparametre,
        names_prefix =
          "incer",
        values_from = incertitude,
        values_fn = max
      )
    
    analyses_a_tester <-
      analyses_a_tester %>% tidyr::pivot_wider(
        id_cols = c(
          cdstationmesureinterne,
          dateprel,
          heureprel,
          codeprel,
          cdrdd
        ),
        names_from =
          cdparametre,
        names_prefix =
          "p",
        values_from = rsana,
        values_fn = max
      )
    
    analyses_a_tester <- dplyr::left_join(
      analyses_a_tester,
      incertitudes_a_tester,
      by = c(
        "cdstationmesureinterne",
        "dateprel",
        "heureprel",
        "codeprel",
        "cdrdd"
      )
    )
    
    
    #on teste seulement si on a des données à la fois sur PO4 et Ptot
    if (all(c("p1350", "p1433") %in% names(analyses_a_tester)))
    {
      analyses_a_tester <-
        analyses_a_tester %>% subset(!is.na(p1350) & !is.na(p1433))
      
      fct_test_P <-
        function(x) {
          print(analyses_a_tester[["cdstationmesureinterne"]][x])
          func_test_metier_coherenceP(
            Ptot = analyses_a_tester[["p1350"]][x],
            PO4 = analyses_a_tester[["p1433"]][x],
            incertPtot = analyses_a_tester[["incer1350"]][x],
            incertPO4 = analyses_a_tester[["incer1350"]][x],
            forceincert = TRUE
          )
        }
      
      fct_test_P_val <-
        function(x) {
          func_test_metier_coherenceP(
            Ptot = analyses_a_tester[["p1350"]][x],
            PO4 = analyses_a_tester[["p1433"]][x],
            incertPtot = analyses_a_tester[["incer1350"]][x],
            incertPO4 = analyses_a_tester[["incer1350"]][x],
            export = "value",
            forceincert = TRUE
          )
        }
      
      
      analyses_a_tester$testPtot_PO4 <-
        sapply(seq_along(analyses_a_tester$cdstationmesureinterne),
               FUN = fct_test_P)
      analyses_a_tester$PO4_en_PPO4 <-
        sapply(seq_along(analyses_a_tester$cdstationmesureinterne),
               FUN = fct_test_P_val)
      
      analyses_a_tester$pourcentage_PO4_sur_Ptot <-
        round(analyses_a_tester$PO4_en_PPO4 / analyses_a_tester$p1350 * 100,
              1)
      
      analyses_a_tester <-
        analyses_a_tester %>% dplyr::rename(
          Ptot = p1350,
          PO4 = p1433,
          incertitude_Ptot = incer1350,
          incertitude_PO4 = incer1433
        )
      
      analyses_a_tester$testPtot_PO4 <-
        analyses_a_tester$testPtot_PO4 %>%
        dplyr::recode_values(
          "1" ~ "correcte",
          "2" ~ "dans marge incertitude",
          "3" ~ "incorrect"
        )
      
      Rapport$coherence_Ptot_PO4 <-
        analyses_a_tester %>% subset(testPtot_PO4 != "correcte")
    }
  }
  
  
  
  
  ##### Comparaison bon de commande - réalisé #####
  
  analyses_attendues$cle <-
    paste0(
      analyses_attendues$res_stm_cdstationmesureinterne,
      "_",
      analyses_attendues$rea_cdfractionanalysee,
      "_",
      analyses_attendues$rea_par_cdparametre,
      "_",
      analyses_attendues$rea_cdunitemesure,
      "_",
      analyses_attendues$rea_cdinsituana
    )
  
  Analyses$cle <-
    paste0(
      Analyses$cdstationmesureinterne,
      "_",
      Analyses$cdfractionanalysee,
      "_",
      Analyses$cdparametre,
      "_",
      Analyses$cdunitemesure,
      "_",
      Analyses$cdinsituana
    )
  
  ##### conversion d'unités si pertinent #####
  # on regarde si un couple paramètre/fraction est rendu dans une unité différente de celle attendue.
  # Si une seule unité attendue pour le couple concerné et si l'unité de rendu est différente, alors
  # on convertit rsAna, LQAna, LDAna dans l'unité attendue et on change le code unité en conséquent
  
  # tableau facteur de conversion à créer sous forme data
  # mg/L en µg/L et inversement
  # ng/L en mg/L et inversement
  # mg N-NO3/L en mg NO3/L et inversement
  # ...
  
  
  ##### Création des clés alternatives avec conversion d'unités #####
  conversion_possibles <- dplyr::inner_join(
    Analyses,
    conversion_unites,
    by = c("cdunitemesure" = "CdUniteIn"),
    relationship = "many-to-many"
  )
  
  conversion_possibles <- conversion_possibles %>%
    dplyr::mutate(
      cle = paste0(
        cdstationmesureinterne, "_",
        cdfractionanalysee, "_",
        cdparametre, "_",
        CdUniteOut, "_",
        cdinsituana
      ),
      cle_origine = paste0(
        cdstationmesureinterne, "_",
        cdfractionanalysee, "_",
        cdparametre, "_",
        cdunitemesure, "_",
        cdinsituana
      ),
      rsana = rsana * ConversionInOut,
      lqana = lqana * ConversionInOut,
      ldana = ldana * ConversionInOut,
      cdunitemesure = CdUniteOut,
      SymUnite = SymUniteOut,
      unite=SymUniteOut
    )
  
  # Fusion des analyses originales et converties
  Analyses_completes <- dplyr::bind_rows(
    Analyses,
    conversion_possibles%>%
      dplyr::select(-CdUniteOut, -SymUniteOut, -ConversionInOut, -SymUniteIn)
    
  )
  
  ##### Rapport des analyses converties utiles #####
  
  # 1. Clés attendues
  cles_attendues <- unique(analyses_attendues$cle)
  
  # 2. Clés présentes dans les analyses originales (sans conversion)
  cles_rendues_originales <- unique(Analyses$cle)
  
  # 3. Clés manquantes avant conversion
  cles_manquantes_avant_conversion <- setdiff(cles_attendues, cles_rendues_originales)
  
  # 4. Clés rendues après conversion
  cles_rendues_converties <- unique(conversion_possibles$cle)
  
  # 5. Clés qui comblent les manques
  cles_comblees_par_conversion <- intersect(cles_manquantes_avant_conversion, cles_rendues_converties)
  
  # 6. Clés converties qui correspondent à des analyses attendues
  cles_converties_attendues <- intersect(cles_attendues, cles_rendues_converties)
  
  # 7. Ensemble des analyses valides converties ou non
  cles_analyses_valides <- union(cles_rendues_originales, cles_rendues_converties)
  
  # 8. Clés manquantes apres conversion
  cles_manquantes_apres_conversion <- setdiff(cles_attendues, cles_analyses_valides)
  
  # 9. Extraire les lignes correspondantes
  Rapport$analyses_converties <- conversion_possibles %>%
    dplyr::filter(cle %in% cles_comblees_par_conversion)%>%
    dplyr::mutate(conversion=paste0("unité convertie de ", SymUniteIn," en ",SymUniteOut))%>%
    dplyr::select(cdstationmesureinterne,
                  LbStationMesureEauxSurface,
                  cdsupport,
                  LbSupport,
                  cdfractionanalysee,
                  LbFraction,
                  codeprel,
                  RefAnaProd,
                  dateprel,
                  heureprel,
                  cdparametre,
                  LbParametre,
                  conversion
    )
  
  # 10. On conserve :
  # a) les analyses d'origine qui ne sont pas dans la liste des analyses converties et attendues
  # b) les analyses converties et attendues
  
  # a) les analyses d'origine qui ne sont pas dans la liste des analyses converties et attendues
  analyses_origine_non_con<-Analyses %>%
    subset(!(cle%in%Analyses_completes[Analyses_completes$cle%in%cles_comblees_par_conversion,]$cle_origine))
  
  # b) les analyses converties et attendues
  Analyses_filtrees <- Analyses_completes %>%
    dplyr::filter(
      cle %in%    cles_comblees_par_conversion # cles issues de la conversion et attendues
    )
  
  Analyses<- dplyr::bind_rows(analyses_origine_non_con, Analyses_filtrees)%>%
    dplyr::select(-cle_origine)
  
  ##### Stations commandées et réalisées #####
  
  Rapport$stations_commandees_analysees <- Analyses %>%
    subset(
      cdstationmesureinterne %in% analyses_attendues$res_stm_cdstationmesureinterne &
        cdrqana != "0"
    ) %>%
    dplyr::group_by(cdstationmesureinterne, station, dateprel) %>%
    dplyr::summarise(nb_donnees = dplyr::n())
  
  ##### Stations non commandées et réalisées #####                                                                                                                  )
  Rapport$stations_non_commandees_analysees <- Analyses %>%
    subset(
      !(
        cdstationmesureinterne %in% analyses_attendues$res_stm_cdstationmesureinterne
      )
    ) %>%
    dplyr::group_by(cdstationmesureinterne, station, dateprel) %>%
    dplyr::summarise(nb_donnees = dplyr::n())
  
  ##### Stations commandées et non réalisées #####                                                                                                                  )
  analyses_attendues$station <- func_ajoute_nom_sandre(connexion,
                                                       code = analyses_attendues$res_stm_cdstationmesureinterne,
                                                       out = "nom_station")
  
  Rapport$stations_manquantes <- analyses_attendues %>%
    subset(!(
      res_stm_cdstationmesureinterne %in% Analyses$cdstationmesureinterne
    )) %>%
    dplyr::group_by(res_stm_cdstationmesureinterne, station) %>%
    dplyr::summarise(nb_donnees = dplyr::n())
  
  
  ##### suppression des stations manquantes dans le rapport avec les dates heure de résultats environnementaux différents des dates heures de résultats d'analyses #####
  
  Rapport$heureResEnv_diff_heurePrel<-Rapport$heureResEnv_diff_heurePrel%>%
    subset(!cdstationmesureinterne%in%Rapport$stations_manquantes$res_stm_cdstationmesureinterne)
  Rapport$dateResEnv_diff_datePrel<-Rapport$dateResEnv_diff_datePrel%>%
    subset(!cdstationmesureinterne%in%Rapport$stations_manquantes$res_stm_cdstationmesureinterne)

  ##### Analyses hors bon de commande (analyses en +) #####
  ##### Analyses hors bon de commande (corrigé avec conversion) #####
  Rapport$analyses_hors_bon_de_commande <- Analyses %>%
    dplyr::filter(
      !(cle %in% cles_analyses_valides) &
        !(cdstationmesureinterne %in% Rapport$stations_manquantes$res_stm_cdstationmesureinterne)
    ) %>%
    dplyr::select(
      "cdstationmesureinterne",
      "station",
      "dateprel",
      "heureprel",
      "profondeurpre",
      "ZoneVerticaleProspectee",
      "cdparametre",
      "nomparametre",
      "insitu",
      "nomfraction",
      "cdrqana",
      "rsana",
      "unite",
      "lqana",
      "commentairesana",
      "cdpreleveur",
      "nompreleveur",
      "cdlaboratoire",
      "nomlabo",
      "cdrdd",
      "nomrdd"
    ) %>%
    unique()
  
  
  
  
  ##### Analyses en doublon #####
  doublons <- Analyses  %>%
    dplyr::group_by(cle, dateprel) %>%
    dplyr::summarise(nb = dplyr::n()) %>%
    subset(nb > 1)
  
  Rapport$analyses_en_doublon <- Analyses  %>%
    dplyr::inner_join(doublons, by = c("cle", "dateprel")) %>%
    dplyr::select(
      "cdstationmesureinterne",
      "station",
      "dateprel",
      "heureprel",
      "dateana",
      "heureana",
      "profondeurpre",
      "ZoneVerticaleProspectee",
      "cdparametre",
      "nomparametre",
      "insitu",
      "nomfraction",
      "cdrqana",
      "rsana",
      "unite",
      "lqana",
      "commentairesana",
      "cdpreleveur",
      "nompreleveur",
      "cdlaboratoire",
      "nomlabo",
      "cdrdd",
      "nomrdd"
    ) %>% unique()
  
  ##### Analyses manquantes #####
  nb_analyses_attendues <- analyses_attendues %>%
    subset(
      !res_stm_cdstationmesureinterne %in%
        Rapport$stations_manquantes$res_stm_cdstationmesureinterne
    ) %>%
    unique %>%
    dplyr::group_by(cle) %>%
    dplyr::summarise(nb_attendu = dplyr::n()) %>%
    dplyr::ungroup()
  
  nb_analyses_rendues <- Analyses %>%
    subset(cdrqana != "0") %>%
    dplyr::select(cle, dateprel) %>%
    unique %>%
    dplyr::group_by(cle) %>%
    dplyr::summarise(nb_rendu = dplyr::n()) %>%
    dplyr::ungroup()
  nb_analyses_rendues$nb_rendu <-
    ifelse(is.na(nb_analyses_rendues$nb_rendu),
           0,
           nb_analyses_rendues$nb_rendu)
  
  
  delta_analyses <- dplyr::left_join(nb_analyses_attendues,
                                     nb_analyses_rendues,
                                     by = "cle") %>%
    dplyr::mutate(delta = nb_attendu - ifelse(is.na(nb_rendu), 0, nb_rendu)) %>%
    subset(delta > 0)
  
  extraire_lignes <- function(df, delta_analyses) {
    # Calculer le nombre d'occurrences de chaque clé dans le data.frame d'analyses
    df$occurrences <- ave(df$cle, df$cle, FUN = length)
    
    # Fusionner avec le data.frame delta pour récupérer les nombres d'occurrences souhaités
    df <- merge(df, delta_analyses, by = "cle", all = TRUE)
    
    # Extraire les lignes avec les clés et les nombres d'occurrences souhaités
    df <- subset(df, occurrences <= delta)
    
    # Retirer la colonne "occurrences" ajoutée précédemment
    df$occurrences <- NULL
    
    # Retourner le data.frame résultant
    return(df)
  }
  
  
  
  Rapport$analyses_manquantes <-
    extraire_lignes(analyses_attendues, delta_analyses) %>%
    dplyr::select(
      "res_stm_cdstationmesureinterne",
      "station",
      "rea_dateprel_prev",
      "rea_profondeurpre",
      "rea_par_cdparametre",
      "nomparametre",
      "rea_cdinsituana",
      "insitu",
      "nomfraction",
      "rea_cdpreleveur",
      "nompreleveur",
      "rea_cdlaboratoire",
      "nomlabo",
      "rea_rdd_cdrdd"
    ) %>% unique() %>%
    subset(
      !res_stm_cdstationmesureinterne %in%
        Rapport$stations_manquantes$res_stm_cdstationmesureinterne
    )
  
  
  ##### Test vraisemblance des résultats d'analyses #####
  Analyses$cle_frac_unit <- paste0(
    Analyses$cdparametre,
    "_",
    Analyses$cdfractionanalysee,
    "_",
    Analyses$cdunitemesure
  )
  
  Analyses$cle_staq_frac_unit <-
    paste0(
      Analyses$cdstationmesureinterne,
      "_",
      Analyses$cdparametre,
      "_",
      Analyses$cdfractionanalysee,
      "_",
      Analyses$cdunitemesure
    )
  verif <- Analyses %>%
    dplyr::left_join(table_stat_analyses, by = "cle_staq_frac_unit") %>%
    dplyr::left_join(table_stat_analyses_toutes_staq, by = "cle_frac_unit") %>%
    subset(cdrqana == "1")
  
  ##### ajout de la qualification à la table Analyses #####
  
  ##### qualif par station #####
  verif$classement_par_station <- 99
  
  # classe 1
  verif$classement_par_station <-
    ifelse(
      verif$rsana >= verif$Q10ST &
        verif$rsana <= verif$Q90ST,
      1,
      verif$classement_par_station
    )
  
  # classe 2
  verif$classement_par_station <- ifelse(
    (verif$rsana >= verif$Q5ST & verif$rsana < verif$Q10ST) |
      (verif$rsana <= verif$Q95ST &
         verif$rsana > verif$Q90ST),
    2,
    verif$classement_par_station
  )
  
  # classe 3
  verif$classement_par_station <- ifelse(
    (verif$rsana >= verif$minST & verif$rsana < verif$Q5ST) |
      (verif$rsana <= verif$maxST &
         verif$rsana > verif$Q95ST),
    3,
    verif$classement_par_station
  )
  
  # classe 7
  verif$classement_par_station <- ifelse(
    (
      verif$rsana >= (verif$minST - verif$sdST) &
        verif$rsana < verif$minST
    ) |
      (
        verif$rsana <= (verif$maxST + verif$sdST) &
          verif$rsana > verif$maxST
      ),
    7,
    verif$classement_par_station
  )
  
  # classe 9
  verif$classement_par_station <- ifelse(
    (
      verif$rsana >= (verif$minST - 2 * verif$sdST) &
        verif$rsana < (verif$minST - verif$sdST)
    ) |
      (
        verif$rsana <= (verif$maxST + 2 * verif$sdST) &
          verif$rsana > (verif$maxST + verif$sdST)
      ),
    9,
    verif$classement_par_station
  )
  
  # classe 10
  verif$classement_par_station <-
    ifelse((verif$rsana <= (verif$minST - 2 *
                              verif$sdST)) |
             (verif$rsana >= (verif$maxST + 2 * verif$sdST)),
           10,
           verif$classement_par_station)
  
  # classe 0
  verif$classement_par_station <-
    ifelse(is.na(verif$classement_par_station),
           0,
           verif$classement_par_station)
  
  
  ##### qualif toutes stations
  
  verif$classement_toutes_station <- 99
  
  # classe 2
  verif$classement_toutes_station <-
    ifelse(
      verif$rsana >= verif$Q10 &
        verif$rsana <= verif$Q90,
      2,
      verif$classement_toutes_station
    )
  
  # classe 3
  verif$classement_toutes_station <- ifelse(
    (verif$rsana >= verif$Q5 & verif$rsana < verif$Q10) |
      (verif$rsana <= verif$Q95 &
         verif$rsana > verif$Q90),
    3,
    verif$classement_toutes_station
  )
  
  # classe 4
  verif$classement_toutes_station <- ifelse(
    (verif$rsana >= verif$Q1 & verif$rsana < verif$Q5) |
      (verif$rsana <= verif$Q99 &
         verif$rsana > verif$Q95),
    4,
    verif$classement_toutes_station
  )
  
  # classe 6
  verif$classement_toutes_station <- ifelse(
    (verif$rsana >= verif$min & verif$rsana < verif$Q1) |
      (verif$rsana <= verif$max &
         verif$rsana > verif$Q99),
    6,
    verif$classement_toutes_station
  )
  
  # classe 8
  verif$classement_toutes_station <- ifelse(
    (verif$rsana >= (verif$min - verif$sd) &
       verif$rsana < verif$min) |
      (verif$rsana <= (verif$max + verif$sd) &
         verif$rsana > verif$max),
    8,
    verif$classement_toutes_station
  )
  
  
  # classe 10
  verif$classement_toutes_station <-
    ifelse((verif$rsana <= (verif$min -
                              verif$sd)) |
             (verif$rsana >= (verif$max + verif$sd)),
           10,
           verif$classement_toutes_station)
  
  # classe 0
  verif$classement_toutes_station <-
    ifelse(is.na(verif$classement_toutes_station),
           0,
           verif$classement_toutes_station)
  
  ##### agrégation des vérifications
  verif$cdqualana <-
    ifelse(verif$classement_toutes_station == 0 &
             verif$classement_par_station == 0,
           3,
           4)
  
  verif$cdqualana <-
    ifelse(
      verif$classement_toutes_station %in% c(2, 3, 4) &
        verif$classement_par_station %in% c(0, 1, 2),
      1,
      verif$cdqualana
    )
  
  verif$cdqualana <-
    ifelse(
      verif$classement_toutes_station %in% c(2, 3) &
        verif$classement_par_station %in% c(3),
      1,
      verif$cdqualana
    )
  
  verif$cdqualana <-
    ifelse(
      verif$classement_toutes_station %in% c(6) &
        verif$classement_par_station %in% c(3),
      1,
      verif$cdqualana
    )
  
  verif$cdqualana <-
    ifelse(
      verif$classement_toutes_station %in% c(6) &
        verif$classement_par_station %in% c(0, 1, 2, 3, 7),
      3,
      verif$cdqualana
    )
  
  verif$cdqualana <-
    ifelse(
      verif$classement_toutes_station %in% c(4) &
        verif$classement_par_station %in% c(3, 7),
      3,
      verif$cdqualana
    )
  
  verif$cdqualana <-
    ifelse(
      verif$classement_toutes_station %in% c(3) &
        verif$classement_par_station %in% c(7),
      3,
      verif$cdqualana
    )
  
  verif$cdqualana <-
    ifelse(
      verif$classement_toutes_station %in% c(2) &
        verif$classement_par_station %in% c(7, 9),
      3,
      verif$cdqualana
    )
  
  verif$cdqualana <-
    ifelse(verif$classement_toutes_station %in% c(8, 10),
           2,
           verif$cdqualana)
  
  verif$cdqualana <-
    ifelse(
      verif$classement_toutes_station %in% c(3, 4, 6) &
        verif$classement_par_station %in% c(9, 10),
      2,
      verif$cdqualana
    )
  
  verif$cdqualana <-
    ifelse(
      verif$classement_toutes_station %in% c(2) &
        verif$classement_par_station %in% c(10),
      2,
      verif$cdqualana
    )
  
  
  verif$cdqualana <- as.character(verif$cdqualana)
  verif$nomclassement <- verif$cdqualana %>%
    dplyr::recode_values(
      "0" ~ "non definissable",
      "1" ~ "correct",
      "2" ~ "incorrect",
      "3" ~ "incertain",
      "4" ~ "non qualifié"
    )
  
  
  
  # export des resultats
  
  Rapport$resultats_a_confirmer <- verif %>%
    dplyr::select(
      "nomclassement",
      "cdstationmesureinterne",
      "station",
      "dateprel",
      "heureprel",
      "profondeurpre",
      "ZoneVerticaleProspectee",
      "cdparametre",
      "nomparametre",
      "nomlongparametre",
      "insitu",
      "nomfraction",
      "cdrqana",
      "rsana",
      "unite",
      "lqana",
      "commentairesana",
      "cdpreleveur",
      "nompreleveur",
      "cdlaboratoire",
      "nomlabo",
      "cdrdd",
      "nomrdd"
    ) %>%
    subset(nomclassement %in% c("incorrect",
                                "incertain"))
  
  ##### ajout de la qualification à la table Analyses #####
  verif <-
    verif %>% dplyr::select(
      "cdqualana",
      "cdstationmesureinterne",
      "dateprel",
      "heureprel",
      "profondeurpre",
      "ZoneVerticaleProspectee",
      "cdparametre",
      "cdinsituana",
      "cdfractionanalysee",
      "cdrqana",
      "rsana",
      "cdunitemesure",
      "cdpreleveur",
      "cdlaboratoire"
    )
  
  Analyses <-
    Analyses %>% dplyr::left_join(
      verif,
      by = c(
        "cdstationmesureinterne",
        "dateprel",
        "heureprel",
        "profondeurpre",
        "ZoneVerticaleProspectee",
        "cdparametre",
        "cdinsituana",
        "cdfractionanalysee",
        "cdrqana",
        "rsana",
        "cdunitemesure",
        "cdpreleveur",
        "cdlaboratoire"
      ),
      relationship = "many-to-many"
    )
  
  
  ##### Vérification dispositif de collecte #####
  Rapport$reseaux_de_mesures <- Analyses %>%
    dplyr::group_by(cdrdd, nomrdd) %>%
    dplyr::summarise(nb = dplyr::n(),
                     pourcent = 100 * dplyr::n() / nrow(Analyses))
  
  ##### Vérification respect des LQ contractuelles #####
  
  compar_perf_anal <- dplyr::inner_join(
    Analyses,
    analyses_attendues %>% subset(!is.na(rea_par_cdparametre)),
    by = "cle",
    suffix = c("", ".attendu"),
    multiple = "all",
    relationship = "many-to-many"
  )
  
  
  Rapport$lq_non_conforme <- compar_perf_anal %>%
    subset((lqana > rea_lqprev) & cdrqana == "10") %>%
    dplyr::select(
      "cdstationmesureinterne",
      "station",
      "dateprel",
      "heureprel",
      "profondeurpre",
      "ZoneVerticaleProspectee",
      "cdparametre",
      "nomparametre",
      "insitu",
      "nomfraction",
      "cdrqana",
      "rsana",
      "unite",
      "lqana",
      "rea_lqprev",
      "commentairesana",
      "cdpreleveur",
      "nompreleveur",
      "cdlaboratoire",
      "nomlabo",
      "cdrdd",
      "nomrdd"
    ) %>%
    dplyr::rename("lq_au_bpu" = "rea_lqprev") %>%
    unique()
  
  ##### Vérification respect des accréditations #####
  
  Rapport$accreditation_non_conforme <- compar_perf_anal %>%
    subset(rea_cdaccreanaprev == "true" & cdaccreana != "1") %>%
    dplyr::select(
      "cdstationmesureinterne",
      "station",
      "dateprel",
      "heureprel",
      "profondeurpre",
      "ZoneVerticaleProspectee",
      "cdparametre",
      "nomparametre",
      "insitu",
      "nomfraction",
      "cdrqana",
      "rsana",
      "unite",
      "cdaccreana",
      "rea_cdaccreanaprev",
      "commentairesana",
      "cdpreleveur",
      "nompreleveur",
      "cdlaboratoire",
      "nomlabo",
      "cdrdd",
      "nomrdd"
    ) %>%
    dplyr::rename("accreditation_attendue" = "rea_cdaccreanaprev")
  
  
  # calcul taux de résultats rendus sous accréditation par rapport à celui attendu
  
  if (nrow(compar_perf_anal %>%
           subset(rea_cdaccreanaprev == "true")) > 0)
  {
    Rapport$taux_result_accredites_sur_attendus <-
      nrow(compar_perf_anal %>%
             subset(rea_cdaccreanaprev ==
                      "true" &
                      cdaccreana == "1")) / nrow(compar_perf_anal %>%
                                                   subset(rea_cdaccreanaprev ==
                                                            "true")) * 100
  }
  
  ##### Vérification des incertitudes contractuelles #####
  Rapport$incertitude_non_conforme <- compar_perf_anal %>%
    subset(incertitude > rea_incertitudeprev) %>%
    dplyr::select(
      "cdstationmesureinterne",
      "station",
      "dateprel",
      "heureprel",
      "profondeurpre",
      "ZoneVerticaleProspectee",
      "cdparametre",
      "nomparametre",
      "insitu",
      "nomfraction",
      "cdrqana",
      "rsana",
      "unite",
      "lqana",
      "incertitude",
      "rea_incertitudeprev",
      "commentairesana",
      "cdpreleveur",
      "nompreleveur",
      "cdlaboratoire",
      "nomlabo",
      "cdrdd",
      "nomrdd"
    ) %>%
    dplyr::rename("incertitude_au_bpu" = "rea_incertitudeprev") %>%
    unique()
  
  
  ##### Vérification des méthodes analytiques contractuelles #####
  compar_perf_anal$methode_au_bpu <- func_ajoute_nom_sandre(connexion,
                                                            code = compar_perf_anal$rea_cdmethode,
                                                            out = "nom_methode")
  
  compar_perf_anal$methode_analyse <-
    func_ajoute_nom_sandre(connexion,
                           code = compar_perf_anal$cdmethode,
                           out = "nom_methode")
  
  Rapport$methode_non_conforme <- compar_perf_anal %>%
    subset(cdmethode != rea_cdmethode) %>%
    dplyr::select(
      "cdstationmesureinterne",
      "station",
      "dateprel",
      "heureprel",
      "profondeurpre",
      "ZoneVerticaleProspectee",
      "cdparametre",
      "nomparametre",
      "insitu",
      "nomfraction",
      "cdrqana",
      "rsana",
      "unite",
      "cdmethode",
      "methode_analyse",
      "rea_cdmethode",
      "methode_au_bpu",
      "commentairesana",
      "cdpreleveur",
      "nompreleveur",
      "cdlaboratoire",
      "nomlabo",
      "cdrdd",
      "nomrdd"
    ) %>%
    dplyr::rename("cd_methode_au_bpu" = "rea_cdmethode") %>%
    unique()
  
  
  ##### Test durée transport / réception échantillon labo #####
  
  Rapport$duree_transport <- "Fonction à implémenter"
  
  
  
  ##### Test température réception échantillon #####
  
  Rapport$temperature_reception_echantillon <-
    "Fonction à implémenter"

  
  return(list(
    Rapport = Rapport,
    Analyses = Analyses
  ))
}


# =====================================================================
#   FONCTION SECURISEE
#   -> effectue un tryCatch propre autour de la fonction métier
# =====================================================================

safe_execute_quesu <- function(fichier, bon_de_commande_id, connexion) {
  
  tryCatch(
    {
      # Appel direct à la fonction métier
      res <- simple_test_function(
        fichier            = fichier,
        bon_de_commande_id = bon_de_commande_id,
        connexion          = connexion
      )
      
      # Vérifications minimales
      if (!("Rapport" %in% names(res)) || !("Analyses" %in% names(res))) {
        stop("La fonction métier doit renvoyer Rapport et Analyses.")
      }
      
      return(res)
    },
    error = function(e) {
      stop(paste("Erreur pendant l'analyse QUESU :", e$message))
    }
  )
}


# =====================================================================
#   FONCTION PUBLIQUE APPELÉE PAR SHINY
# =====================================================================

run_tests_quesu <- function(fichier, bco_id, pool) {
  
  if (is.null(fichier) || !file.exists(fichier)) {
    stop("Le fichier XML à analyser est introuvable.")
  }
  
  if (is.null(bco_id)) {
    stop("L'identifiant du bon de commande (bco_id) est obligatoire.")
  }
  
  # Appel de la fonction sécurisée
  return(
    safe_execute_quesu(
      fichier             = fichier,
      bon_de_commande_id  = bco_id,
      connexion           = pool
    )
  )
}



# 
# 
# # ============================================================================
# #  FONCTION PUBLIQUE APPELÉE PAR SHINY
# # ============================================================================
# run_tests_quesu <- function(fichier, bco_id, pool) {
#   
#   if (is.null(fichier) || !file.exists(fichier)) {
#     stop("Le fichier à analyser est introuvable.")
#   }
#   
#   if (is.null(bco_id)) {
#     stop("Aucun identifiant de bon de commande fourni.")
#   }
#   
#   # Exécuter la logique métier encapsulée
#   out <- safe_execute_quesu(
#     fichier = fichier,
#     bco_id  = bco_id,
#     pool    = pool
#   )
#   
#   # Vérifications minimales
#   if (!("Rapport" %in% names(out)) || !("Analyses" %in% names(out))) {
#     stop("Erreur interne : Rapport ou Analyses manquant(e).")
#   }
#   
#   return(out)
# }
# 
# # ============================================================================
# #  ENVIRONNEMENT SÉCURISÉ POUR LE SCRIPT D'IMPORT COMPLET
# # ============================================================================
# 
# 
# safe_execute_quesu <- function(fichier, bco_id, pool) {
#   
#   env <- new.env(parent = emptyenv())
#   
#   assign("fichier", fichier, envir = env)
#   assign("bon_de_commande_id", bco_id, envir = env)
#   assign("connexion", pool, envir = env)
#   

#   
#   
# 
#   env$execute <- function() {
#     
#     

#     
#   } 
#   
#   
#   # ----------------------------------------------------
#   # EXÉCUTION DU SCRIPT DANS env
#   # ----------------------------------------------------
#   tryCatch(
#     {
#       evalq({
#         execute()
#       }, envir = env)
#       
#       list(
#         Rapport  = env$Rapport,
#         Analyses = env$Analyses
#       )
#     },
#     error = function(e) {
#       stop(paste("Erreur pendant l'analyse QUESU :", e$message))
#     }
#   )
#   
# } 
# 
# 
