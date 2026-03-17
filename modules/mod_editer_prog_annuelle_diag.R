
# =====================================================================
# MODULE : Diagnostic de cohérence prog annuelle (SERVER + UI wrappers)
# Fichier : mod_editer_prog_annuelle_diag.R
# =====================================================================

# -----------------------------
# UI wrapper (déjà appelé côté UI principal)
# -----------------------------
# mod_editer_prog_annuelle_diag_ui(id) est défini dans le fichier UI principal.


# -----------------------------
# SERVER
# -----------------------------
mod_editer_prog_annuelle_diag_server <- function(
    id,
    pool,
    schema_sqe = "sqe",
    key_ctx,        # reactive() -> list(mar, an[char])
    pga, cal, bco, bcq, bcp, rea  # reactiveVal() renvoyant des data.frames
){
  moduleServer(id, function(input, output, session){
    
    ns <- session$ns
    
    # --------------------------------------------------------------
    # Helpers
    # --------------------------------------------------------------
    sql <- function(..., .envir = parent.frame()){
      glue::glue_sql(..., .con = pool, .envir = .envir)
    }
    
    with_conn <- function(expr){
      conn <- pool::poolCheckout(pool)
      on.exit(pool::poolReturn(conn), add = TRUE)
      
      eval(
        substitute(expr),
        envir  = list(conn = conn),
        enclos = parent.frame()
      )
    }
    
    # --------------------------------------------------------------
    # Stockage des tableaux de diagnostic (pour affichage + export)
    # --------------------------------------------------------------
    rv <- reactiveValues(
      resume    = NULL,
      pga_cal   = NULL,
      cal_bco   = NULL,
      bco_bcq   = NULL,
      bco_bcp   = NULL,
      bcp_rea   = NULL,
      rea_pga   = NULL
    )
    
    # --------------------------------------------------------------
    # Calcul des diagnostics (réactif aux données + contexte)
    # --------------------------------------------------------------
    compute_diag <- function(){
      
      ctx <- key_ctx()
      
      df_pga <- pga(); if (is.null(df_pga)) df_pga <- data.frame()
      df_cal <- cal(); if (is.null(df_cal)) df_cal <- data.frame()
      df_bco <- bco(); if (is.null(df_bco)) df_bco <- data.frame()
      df_bcq <- bcq(); if (is.null(df_bcq)) df_bcq <- data.frame()
      df_bcp <- bcp(); if (is.null(df_bcp)) df_bcp <- data.frame()
      df_rea <- rea(); if (is.null(df_rea)) df_rea <- data.frame()
      
      # Normalisations minimales
      if (!is.null(df_cal$cal_date)) df_cal$cal_date <- as.Date(df_cal$cal_date)
      if (!is.null(df_bcp$bcp_dateinterv)) df_bcp$bcp_dateinterv <- as.Date(df_bcp$bcp_dateinterv)
      if (!is.null(df_rea$rea_dateprel_prev)) df_rea$rea_dateprel_prev <- as.Date(df_rea$rea_dateprel_prev)
      
      # ----------------------------------------------------------
      # 1) PGA ↔ CAL
      #    - types station présents dans PGA absents de CAL
      #    - types station présents dans CAL absents de PGA (pour info)
      # ----------------------------------------------------------
      types_pga <- if(nrow(df_pga)) unique(df_pga$pga_cal_typestation) else character(0)
      types_cal <- if(nrow(df_cal)) unique(df_cal$cal_typestation) else character(0)
      
      pga_missing_in_cal <- setdiff(types_pga, types_cal)
      cal_missing_in_pga <- setdiff(types_cal, types_pga)
      
      diag_pga_cal <- data.frame(
        type = c(rep("PGA_non_trouvé_dans_CAL", length(pga_missing_in_cal)),
                 rep("CAL_non_trouvé_dans_PGA", length(cal_missing_in_pga))),
        typestation = c(pga_missing_in_cal, cal_missing_in_pga),
        stringsAsFactors = FALSE
      )
      
      # ----------------------------------------------------------
      # 2) CAL ↔ BCO (via périmètre depuis PGA & rattachement BDC dans CAL)
      #    Logique :
      #     - join CAL (typestation) -> PGA (perimetre) pour associer un périmètre
      #     - pour chaque (perimetre, rattachement) présent dans CAL,
      #       chercher au moins un BCO bco_per_nom = périmètre ET
      #       bco_refcommande qui contient "_<perimetre>_<rattachement>_"
      # ----------------------------------------------------------
      diag_cal_bco <- data.frame()
      
      if (nrow(df_cal) && nrow(df_pga) && nrow(df_bco)) {
        
        # Join CAL (typestation) -> PGA (perimetre)
        key_cols <- c("cal_typestation" = "pga_cal_typestation")
        calx <- merge(df_cal, unique(df_pga[, c("pga_cal_typestation","pga_per_nom")]),
                      by.x = "cal_typestation", by.y = "pga_cal_typestation", all.x = TRUE)
        
        # Comptage des combinaisons (perimetre, rattachement)
        pairs <- unique(calx[, c("pga_per_nom","cal_rattachement_bdc")])
        pairs <- pairs[!is.na(pairs$pga_per_nom) & !is.na(pairs$cal_rattachement_bdc), , drop = FALSE]
        
        if (nrow(pairs)) {
          bad_list <- lapply(seq_len(nrow(pairs)), function(i){
            per <- pairs$pga_per_nom[i]
            rat <- pairs$cal_rattachement_bdc[i]
            
            # BCO attendu : perimetre OK ET refcommande qui matche _<per>_<rat>_
            has_bco <- any(df_bco$bco_per_nom == per &
                             grepl(paste0("_", gsub("([\\W_])","\\\\\\1", per), "_",
                                          gsub("([\\W_])","\\\\\\1", rat), "_"),
                                   df_bco$bco_refcommande))
            
            if (!has_bco) {
              data.frame(pga_per_nom = per, cal_rattachement_bdc = rat,
                         pb = "Aucun BCO avec refcommande concordante", stringsAsFactors = FALSE)
            } else {
              NULL
            }
          })
          diag_cal_bco <- do.call(rbind, bad_list)
          if (is.null(diag_cal_bco)) diag_cal_bco <- data.frame()
        }
      }
      
      # ----------------------------------------------------------
      # 3) BCO ↔ BCQ
      #    - BCQ sans BCO
      #    - (bco_id, prs_id) dupliqués dans BCQ
      #    - bcq_nbprestacom < 0 ou NA
      # ----------------------------------------------------------
      diag_bco_bcq <- data.frame()
      
      if (nrow(df_bcq)) {
        # BCQ sans BCO
        bcq_bco_ids <- unique(df_bcq$bcq_bco_id)
        known_bco <- unique(df_bco$bco_id)
        bcq_without_bco <- setdiff(bcq_bco_ids, known_bco)
        
        part1 <- if (length(bcq_without_bco)) {
          data.frame(type="BCQ_sans_BCO", bco_id = bcq_without_bco, stringsAsFactors = FALSE)
        } else data.frame()
        
        # Doublons (bco, prs)
        dups <- aggregate(list(n = df_bcq$bcq_nbprestacom),
                          by = list(bco = df_bcq$bcq_bco_id, prs = df_bcq$bcq_prs_id),
                          FUN = length)
        dups <- dups[dups$n > 1, ]
        part2 <- if (nrow(dups)) {
          data.frame(type="BCQ_duplicata_(bco,prs)", bco_id = dups$bco, prs_id = dups$prs, stringsAsFactors = FALSE)
        } else data.frame()
        
        # Val négatives ou NA
        bad_nb <- df_bcq[is.na(df_bcq$bcq_nbprestacom) | df_bcq$bcq_nbprestacom < 0, ]
        part3 <- if (nrow(bad_nb)) {
          data.frame(type="BCQ_nbprestacom_invalide", bco_id = bad_nb$bcq_bco_id,
                     prs_id = bad_nb$bcq_prs_id, nb = bad_nb$bcq_nbprestacom, stringsAsFactors = FALSE)
        } else data.frame()
        
        diag_bco_bcq <- rbind(part1, part2, part3)
      }
      
      # ----------------------------------------------------------
      # 4) BCO ↔ BCP
      #    - BCP sans BCO
      #    - Surcomptage : nb lignes BCP (par bco, prs) > bcq_nbprestacom
      # ----------------------------------------------------------
      diag_bco_bcp <- data.frame()
      
      if (nrow(df_bcp)) {
        # BCP sans BCO
        bcp_bco_ids <- unique(df_bcp$bcp_bco_id)
        known_bco <- unique(df_bco$bco_id)
        bcp_without_bco <- setdiff(bcp_bco_ids, known_bco)
        
        part1 <- if (length(bcp_without_bco)) {
          data.frame(type="BCP_sans_BCO", bco_id = bcp_without_bco, stringsAsFactors = FALSE)
        } else data.frame()
        
        # Surcomptage par (bco, prs)
        if (nrow(df_bcq)) {
          cnt_bcp <- aggregate(list(n = df_bcp$bcp_prs_id),
                               by = list(bco = df_bcp$bcp_bco_id, prs = df_bcp$bcp_prs_id),
                               FUN = length)
          colnames(cnt_bcp)[3] <- "nb_bcp"
          
          bcq_min <- unique(df_bcq[, c("bcq_bco_id","bcq_prs_id","bcq_nbprestacom")])
          colnames(bcq_min) <- c("bco","prs","nb_bcq")
          
          cmp <- merge(cnt_bcp, bcq_min, by = c("bco","prs"), all.x = TRUE)
          cmp$nb_bcq[is.na(cmp$nb_bcq)] <- 0
          
          part2 <- cmp[cmp$nb_bcp > cmp$nb_bcq, ]
          if (nrow(part2)) {
            part2 <- data.frame(type="BCP>BCQ_(surcomptage)",
                                bco_id = part2$bco, prs_id = part2$prs,
                                nb_bcp = part2$nb_bcp, nb_bcq = part2$nb_bcq, stringsAsFactors = FALSE)
          } else part2 <- data.frame()
        } else {
          part2 <- data.frame()
        }
        
        diag_bco_bcp <- rbind(part1, part2)
      }
      
      # ----------------------------------------------------------
      # 5) BCP ↔ REA
      #    - REA attendus pour chaque BCP (mêmes clés : bco_id, prs_id, date, station)
      #    - BCP sans REA / REA sans BCP
      # ----------------------------------------------------------
      diag_bcp_rea <- data.frame()
      
      if (nrow(df_bcp) || nrow(df_rea)) {
        # Clés d'égalité (bco, prs, date, station)
        k_bcp <- unique(df_bcp[, c("bcp_bco_id","bcp_prs_id","bcp_dateinterv","bcp_stm_cdstationmesureinterne")])
        colnames(k_bcp) <- c("bco","prs","date","stm")
        
        k_rea <- unique(df_rea[, c("res_bco_id","rea_dateprel_prev","res_stm_cdstationmesureinterne","rea_par_cdparametre")])
        # NB : le paramètre n'est pas une clé d'identité, on l'ignore pour matcher BCP
        colnames(k_rea) <- c("bco","date","stm","par")
        k_rea <- unique(k_rea[, c("bco","date","stm")])
        
        # BCP sans REA
        miss_rea <- merge(k_bcp, k_rea, by = c("bco","date","stm"), all.x = TRUE)
        miss_rea <- miss_rea[is.na(miss_rea$prs) | is.na(miss_rea$stm), c("bco","prs","date","stm")]
        if (nrow(miss_rea)) {
          miss_rea$type <- "BCP_sans_REA"
        }
        
        # REA sans BCP
        miss_bcp <- merge(k_rea, k_bcp, by = c("bco","date","stm"), all.x = TRUE)
        miss_bcp <- miss_bcp[is.na(miss_bcp$prs) | is.na(miss_bcp$stm), c("bco","date","stm")]
        if (nrow(miss_bcp)) {
          miss_bcp$type <- "REA_sans_BCP"
        }
        
        diag_bcp_rea <- rbind(
          miss_rea[, c("type","bco","prs","date","stm")],
          cbind(miss_bcp[, c("type","bco","date","stm")], prs = NA)
        )
        if (!nrow(diag_bcp_rea)) diag_bcp_rea <- data.frame()
      }
      
      # ----------------------------------------------------------
      # 6) REA ↔ PGA
      #    - stations de REA doivent exister dans PGA (marché/année courants)
      #    - cohérence année de date REA avec 'an'
      # ----------------------------------------------------------
      diag_rea_pga <- data.frame()
      
      if (nrow(df_rea)) {
        sta_pga <- unique(df_pga$pga_stm_cdstationmesureinterne)
        rea_bad_sta <- df_rea[!(df_rea$res_stm_cdstationmesureinterne %in% sta_pga), ]
        part1 <- if (nrow(rea_bad_sta)) {
          unique(rea_bad_sta[, c("res_bco_id","res_stm_cdstationmesureinterne","rea_dateprel_prev")])
        } else data.frame()
        
        if (nrow(part1)) {
          colnames(part1) <- c("bco","stm","date")
          part1$type <- "REA_station_absente_PGA"
        }
        
        # Année incohérente
        part2 <- df_rea[format(df_rea$rea_dateprel_prev, "%Y") != ctx$an, c("res_bco_id","res_stm_cdstationmesureinterne","rea_dateprel_prev")]
        if (nrow(part2)) {
          colnames(part2) <- c("bco","stm","date")
          part2$type <- "REA_annee_diff_PGA"
        }
        
        diag_rea_pga <- rbind(part1, part2)
        if (!nrow(diag_rea_pga)) diag_rea_pga <- data.frame()
      }
      
      # ----------------------------------------------------------
      # Résumé global : compte anomalies par bloc
      # ----------------------------------------------------------
      resume <- data.frame(
        bloc = c("PGA_vs_CAL","CAL_vs_BCO","BCO_vs_BCQ","BCO_vs_BCP","BCP_vs_REA","REA_vs_PGA"),
        nb_anomalies = c(nrow(diag_pga_cal), nrow(diag_cal_bco), nrow(diag_bco_bcq),
                         nrow(diag_bco_bcp), nrow(diag_bcp_rea), nrow(diag_rea_pga)),
        stringsAsFactors = FALSE
      )
      
      # Affectation dans rv
      rv$resume  <- resume
      rv$pga_cal <- diag_pga_cal
      rv$cal_bco <- diag_cal_bco
      rv$bco_bcq <- diag_bco_bcq
      rv$bco_bcp <- diag_bco_bcp
      rv$bcp_rea <- diag_bcp_rea
      rv$rea_pga <- diag_rea_pga
    }
    
    # Recalculer à chaque changement de contexte/données
    observeEvent(list(key_ctx(), pga(), cal(), bco(), bcq(), bcp(), rea()), {
      compute_diag()
    }, ignoreInit = FALSE)
    
    # --------------------------------------------------------------
    # Rendu des tableaux (DT)
    # --------------------------------------------------------------
    make_dt <- function(df){
      if (is.null(df) || !nrow(df)) {
        DT::datatable(data.frame(message = "Aucune anomalie détectée."), options = list(dom='t', paging=FALSE))
      } else {
        DT::datatable(df, options = list(scrollX = TRUE, pageLength = 10))
      }
    }
    
    output$resume_global <- DT::renderDT(make_dt(rv$resume))
    output$diag_pga_cal  <- DT::renderDT(make_dt(rv$pga_cal))
    output$diag_cal_bco  <- DT::renderDT(make_dt(rv$cal_bco))
    output$diag_bco_bcq  <- DT::renderDT(make_dt(rv$bco_bcq))
    output$diag_bco_bcp  <- DT::renderDT(make_dt(rv$bco_bcp))
    output$diag_bcp_rea  <- DT::renderDT(make_dt(rv$bcp_rea))
    output$diag_rea_pga  <- DT::renderDT(make_dt(rv$rea_pga))
    
    # --------------------------------------------------------------
    # API pour export Excel (Message 3B)
    # --------------------------------------------------------------
    get_all_tables <- function(){
      list(
        resume  = rv$resume,
        pga_cal = rv$pga_cal,
        cal_bco = rv$cal_bco,
        bco_bcq = rv$bco_bcq,
        bco_bcp = rv$bco_bcp,
        bcp_rea = rv$bcp_rea,
        rea_pga = rv$rea_pga
      )
    }
    
    # Retour de l'API au parent
    list(
      get_all_tables = get_all_tables
    )
  })
}
