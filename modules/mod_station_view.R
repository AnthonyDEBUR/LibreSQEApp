# module pour afficher / éditer les fiches stations


# modules/mod_station_view.R
stationViewUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Fiche station à visualiser / éditer"),
    shiny::fluidRow(
      shiny::column(
        width = 8,
        # Menu déroulant avec recherche intégrée (selectize)
        shiny::selectizeInput(
          ns("station_sel"),
          label = "Station (code interne — libellé SANDRE)",
          choices = NULL, multiple = FALSE,
          options = list(
            placeholder = "Choisir ou taper le code / nom de la station…",
            openOnFocus = TRUE
          )
        )
      )
    ),
    
    shiny::tags$hr(),
    shiny::h3("Carte de localisation"),
    shiny::helpText("Localisation selon SANDRE (geom si disponible) ou coordonnées X/Y."),
    leaflet::leafletOutput(ns("map"), height = 800),
    
    # Squelettes d'autres sections du prototype (placeholders)
    shiny::tags$hr(),
    shiny::h3("Accès station"),
    shiny::helpText("(contenu à venir)"),
    shiny::h3("Précisions sur prélèvement"),
    shiny::helpText("(contenu à venir)"),
    shiny::h3("Commentaires généraux sur la station"),
    shiny::helpText("(contenu à venir)"),
    shiny::h3("Photos"),
    shiny::helpText("(contenu à venir)")
  )
}

stationViewServer <- function(id, con, crs_xy = 2154) {
  # crs_xy : EPSG des colonnes stm_x/stm_y (par défaut Lambert-93 = 2154)
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # --- Helpers DB (compatibles pool::Pool et DBI::PqConnection) ------------
    is_pool <- function(con) inherits(con, "Pool")
    db_get <- function(con, sql) {
      if (is_pool(con) && requireNamespace("pool", quietly = TRUE)) {
        pool::dbGetQuery(con, sql)
      } else {
        DBI::dbGetQuery(con, sql)
      }
    }
    
    # --- Chargement des stations ---------------------------------------------
    # On tente d’obtenir lon/lat via ST_Transform(geom, 4326) si PostGIS, sinon on retombe sur X/Y.
    stations <- shiny::reactiveVal({
      sql <- "
  SELECT
  stm_cdstationmesureinterne,
  stm_lbstationmesureeauxsurface,
  stm_cdstationmesureauxsurface,
  stm_x, stm_y,
  /* Priorité à geom si présent */
  COALESCE(
    ST_X(ST_Transform(geom, 4326)),
    ST_X(ST_Transform(ST_SetSRID(ST_MakePoint(stm_x, stm_y), 2154), 4326))
  ) AS lon,
  COALESCE(
    ST_Y(ST_Transform(geom, 4326)),
    ST_Y(ST_Transform(ST_SetSRID(ST_MakePoint(stm_x, stm_y), 2154), 4326))
  ) AS lat
FROM refer.tr_stationmesure_stm
ORDER BY stm_cdstationmesureinterne;
      "
      df <- try(db_get(con, sql), silent = TRUE)
      if (inherits(df, "try-error")) {
        # Fallback sans PostGIS : lire sans lon/lat
        df <- db_get(con, "
          SELECT stm_cdstationmesureinterne, stm_lbstationmesureeauxsurface,
                 stm_cdstationmesureauxsurface, stm_x, stm_y
          FROM refer.tr_stationmesure_stm
          ORDER BY stm_cdstationmesureinterne;
        ")
        df$lat <- NA_real_
        df$lon <- NA_real_
      }
      df
    })
    
    # --- Peupler le selectize ------------------------------------------------
    shiny::observe({
      df <- stations()
      shiny::req(nrow(df) > 0)
      
      labels <- paste0(df$stm_cdstationmesureinterne, " - ", df$stm_lbstationmesureeauxsurface)
      # Afficher "code - libellé", mais renvoyer la valeur "code interne"
      choices <- stats::setNames(df$stm_cdstationmesureinterne, labels)
      
      shiny::updateSelectizeInput(session, "station_sel",
                                  choices = choices, selected = NULL, server = TRUE
      )
    })
    
   
    
    # --- Carte Leaflet --------------------------------------------------------
    
    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet(options = leaflet::leafletOptions(minZoom = 2)) %>%
        # Fond 1 : OSM (rendu par défaut)
        leaflet::addProviderTiles(
          provider = "OpenStreetMap",
          group = "Plan OSM"
        ) %>%
        # Fond 2 : imagerie (photo aérienne)
        leaflet::addProviderTiles(
          provider = "Esri.WorldImagery",
          group = "Photo aérienne"
        ) %>%
        # Contrôle des fonds (l’utilisateur coche l’un OU l’autre)
        leaflet::addLayersControl(
          baseGroups = c("Plan OSM", "Photo aérienne"),
          options = leaflet::layersControlOptions(collapsed = FALSE)
        ) %>%
        # Optionnel : échelle cartographique
        leaflet::addScaleBar(position = "bottomleft") %>%
        # Vue initiale Bretagne
        leaflet::setView(lng = -1.7, lat = 47.6, zoom = 7)
    })
    
    
    # Calcul lon/lat en WGS84 si absents (fallback via stm_x/stm_y)
    
    # Conversion sûre Lambert‑93 (EPSG:2154) -> WGS84 (EPSG:4326)
    to_wgs84 <- function(x, y, crs_xy = 2154) {
      # Types numériques
      x <- suppressWarnings(as.numeric(x))
      y <- suppressWarnings(as.numeric(y))
      if (is.na(x) || is.na(y)) return(c(NA_real_, NA_real_))
      
      # Exiger sf si le CRS n'est pas déjà WGS84
      if (crs_xy != 4326) {
        if (!requireNamespace("sf", quietly = TRUE)) {
          # Ici on préfère ne rien centrer plutôt que de renvoyer des mètres Lambert
          warning("Package 'sf' requis pour transformer ", crs_xy, " -> 4326.")
          return(c(NA_real_, NA_real_))
        }
        s  <- sf::st_sfc(sf::st_point(c(x, y)), crs = crs_xy)
        s2 <- sf::st_transform(s, 4326)   # WGS84
        xy <- sf::st_coordinates(s2)[1, ]
        lon <- xy[1]; lat <- xy[2]
      } else {
        # Déjà WGS84 : x = lon, y = lat
        lon <- x; lat <- y
      }
      
      # Bornes plausibles
      if (!is.finite(lon) || !is.finite(lat) || abs(lon) > 180 || abs(lat) > 90) {
        return(c(NA_real_, NA_real_))
      }
      c(lon, lat)
    }
    
    
    # Mettre à jour le point sur la carte quand une station est sélectionnée
    shiny::observeEvent(input$station_sel, {
      req(input$station_sel)
      df <- stations()
      row <- df[df$stm_cdstationmesureinterne == input$station_sel, ]
      shiny::req(nrow(row) == 1)
      
      # lon/lat : préférer geom transformé ; sinon calculer via X/Y
      lon <- row$lon[1]; lat <- row$lat[1]
      if (is.na(lon) || is.na(lat)) {
        xy <- to_wgs84(row$stm_x[1], row$stm_y[1])
        lon <- xy[1]; lat <- xy[2]
      }
      
      # Sécurité : si toujours NA, ne rien placer
      if (is.na(lon) || is.na(lat)) {
        shiny::showNotification("Coordonnées non disponibles pour la station sélectionnée.", type = "warning")
        return(invisible(NULL))
      }
      
      label <- paste0(row$stm_cdstationmesureinterne[1], " - ", row$stm_lbstationmesureeauxsurface[1])
      
      leaflet::leafletProxy(ns("map")) |>
        leaflet::clearMarkers() |>
        leaflet::clearShapes() |>
        leaflet::addCircleMarkers(
          lng = lon, lat = lat,
          label = label, radius = 8,
          color = "#1E90FF", fillColor = "#1E90FF", fillOpacity = 0.85,
          popup = htmltools::tags$div(
            htmltools::tags$b(label),
            htmltools::tags$br(),
            paste("Code SANDRE :", row$stm_cdstationmesureauxsurface[1])
          )
        ) |>
        leaflet::setView(lng = lon, lat = lat, zoom = 14)
    }, ignoreInit = TRUE)
  })
}
