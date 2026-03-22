library(shiny)
library(leaflet)
library(sf)
library(terra)
library(tidyterra)
library(DBI)
library(RSQLite)
library(jsonlite)
library(DT)
library(geomtextpath)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(webshot2)
library(htmlwidgets)
library(ggrepel)

# Disable S2 to avoid "Loop 0 is not valid" errors with Natural Earth data
sf_use_s2(FALSE)

# --- Configuration & Local Data Management ---

DB_PATH <- "charts.sqlite"
init_db <- function() {
  con <- dbConnect(RSQLite::SQLite(), DB_PATH)
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS charts (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      name TEXT UNIQUE,
      state_json TEXT,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )
  ")
  dbDisconnect(con)
}
init_db()

get_all_charts <- function() {
  con <- dbConnect(RSQLite::SQLite(), DB_PATH)
  res <- dbGetQuery(con, "SELECT id, name FROM charts ORDER BY id DESC")
  dbDisconnect(con)
  return(res)
}

DATA_DIR <- "data/ne"
if (!dir.exists(DATA_DIR)) dir.create(DATA_DIR, recursive = TRUE)

# Globaler Relief-Raster (einmalig laden, spart Zeit bei jedem Update)
RELIEF_PATH <- "data/shaded_relief/SR_LR.tif"
global_relief <- NULL
if (file.exists(RELIEF_PATH)) {
  global_relief <- rast(RELIEF_PATH)
} else {
  warning("Relief-Datei nicht gefunden unter: ", RELIEF_PATH)
}

get_local_ne_data <- \(type, scale) {
  file_path <- file.path(DATA_DIR, paste0(type, "_", scale, ".rds"))
  
  if (file.exists(file_path)) {
    return(readRDS(file_path))
  }
  
  # Download and process if not exists
  message("Lade Daten herunter: ", type, " (Skala: ", scale, ")")
  
  data <- NULL
  if (type == "countries") {
    data <- ne_countries(scale = scale, returnclass = "sf") |> st_make_valid()
    # German names fallback logic (concise names)
    label_col <- "name_de"
    name_col <- "name"
  } else if (type == "cities") {
    data <- ne_download(scale = scale, type = "populated_places", category = "cultural", returnclass = "sf") |> 
      st_make_valid()
    data <- data[data$FEATURECLA == "Admin-0 capital", ]
    # German names fallback logic
    label_col <- "NAME_DE"
    name_col <- "NAME"
  }
  
  # Unified label logic
  data$label_text <- data[[label_col]]
  nas_or_empty <- is.na(data$label_text) | data$label_text == ""
  data$label_text[nas_or_empty] <- data[[name_col]][nas_or_empty]
  
  saveRDS(data, file_path)
  return(data)
}

# --- Helper Functions ---

get_ne_scale <- \(bbox) {
  if (is.null(bbox)) return(110)
  
  width <- abs(bbox$east - bbox$west)
  height <- abs(bbox$north - bbox$south)
  # Nutze die kleinere Seite, da bei extremem Quer-/Hochformat 
  # sonst zu früh Details verloren gehen.
  min_dim <- min(width, height)
  
  if (min_dim > 60) {
    return(110)
  } else if (min_dim > 15) {
    return(50)
  } else {
    return(10)
  }
}

# Liefert exakt aufgefüllte (Aspekt-Ratio) Koordinaten in beliebiger Projektion
get_expanded_bbox_proj <- \(b, proj, width_cm, height_cm) {
  if (is.null(proj) || proj == "") proj <- "EPSG:3857"
  
  lons <- seq(b$west, b$east, length.out = 50)
  lats <- seq(b$south, b$north, length.out = 50)
  s_edge <- cbind(lons, rep(b$south, 50))
  e_edge <- cbind(rep(b$east, 50), lats)
  n_edge <- cbind(rev(lons), rep(b$north, 50))
  w_edge <- cbind(rep(b$west, 50), rev(lats))
  
  coords <- rbind(s_edge, e_edge, n_edge, w_edge)
  coords <- rbind(coords, coords[1,,drop=FALSE])
  
  bbox_poly <- st_polygon(list(coords))
  bbox_sfc <- st_sfc(bbox_poly, crs = 4326)
  
  bbox_proj <- st_transform(bbox_sfc, proj)
  ex <- st_bbox(bbox_proj)
  
  xmin <- ex["xmin"]; xmax <- ex["xmax"]
  ymin <- ex["ymin"]; ymax <- ex["ymax"]
  
  x_range <- xmax - xmin
  y_range <- ymax - ymin
  
  if (x_range <= 0 || y_range <= 0) {
    return(list(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax))
  }
  
  target_aspect <- width_cm / height_cm
  
  # Check if projection is geographic (degrees) to account for ggplot2's automatic cos() scaling
  is_geo <- grepl("4326", proj) || (tryCatch(st_is_longlat(st_crs(proj)), error = function(e) FALSE))
  
  if (is_geo) {
    y_center <- (ymin + ymax) / 2
    cos_factor <- cos(y_center * pi / 180)
    if (cos_factor < 0.05) cos_factor <- 0.05 # Vermeidung von Pol-Streckungen ins Unendliche
    
    vis_x <- x_range
    vis_y <- y_range / cos_factor
    current_aspect <- vis_x / vis_y
    
    if (current_aspect > target_aspect) {
      new_vis_y <- vis_x / target_aspect
      new_y_range <- new_vis_y * cos_factor
      y_pad <- (new_y_range - y_range) / 2
      ymin <- ymin - y_pad
      ymax <- ymax + y_pad
    } else {
      new_vis_x <- vis_y * target_aspect
      new_x_range <- new_vis_x
      # X bleibt regulär in ungestauchten Einheiten
      x_pad <- (new_x_range - x_range) / 2
      xmin <- xmin - x_pad
      xmax <- xmax + x_pad
    }
  } else {
    current_aspect <- x_range / y_range
    
    if (current_aspect > target_aspect) {
      new_y <- x_range / target_aspect
      y_pad <- (new_y - y_range) / 2
      ymin <- ymin - y_pad
      ymax <- ymax + y_pad
    } else {
      new_x <- y_range * target_aspect
      x_pad <- (new_x - x_range) / 2
      xmin <- xmin - x_pad
      xmax <- xmax + x_pad
    }
  }
  
  list(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)
}

# --- UI ---

ui <- fluidPage(
  titlePanel("Karten-Designer (Shiny)"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Datenbank & Permanenz"),
      uiOutput("load_chart_ui"),
      textInput("chart_name", "Neuer/Aktueller Chart-Name:", placeholder = "Mein Chart..."),
      fluidRow(
        column(6, actionButton("save_chart_btn", "Speichern", icon = icon("save"), class = "btn-success", width = "100%")),
        column(6, actionButton("delete_chart_btn", "Löschen", icon = icon("trash"), class = "btn-danger", width = "100%"))
      ),
      hr(),
      h4("Abmessungen (Vektor-Export)"),
      numericInput("width_cm", "Breite (cm):", value = 15, min = 5, max = 50),
      numericInput("height_cm", "Höhe (cm):", value = 10, min = 5, max = 50),
      selectInput("map_projection", "Karten-Projektion (nur Vorschau):", 
                  choices = c("Web Mercator (Standard)" = "EPSG:3857",
                              "WGS 84 (Längen-/Breitengrade)" = "EPSG:4326",
                              "Robinson" = "+proj=robin",
                              "Winkel Tripel" = "+proj=wintri",
                              "Mollweide" = "+proj=moll"),
                  selected = "EPSG:3857"),
      selectInput("map_provider", "Karten-Provider:", 
                  choices = list(
                    "Standard" = list(
                      "OpenStreetMap (DE)" = "OpenStreetMap.DE",
                      "OpenStreetMap (Standard)" = "OpenStreetMap",
                      "OpenTopoMap" = "OpenTopoMap"
                    ),
                    "CartoDB" = list(
                      "Hell (Positron)" = "CartoDB.Positron",
                      "Dunkel (Dark Matter)" = "CartoDB.DarkMatter",
                      "Voyager" = "CartoDB.Voyager",
                      "Hell (ohne Labels)" = "CartoDB.PositronNoLabels"
                    ),
                    "Esri" = list(
                      "Satellit" = "Esri.WorldImagery",
                      "Topo" = "Esri.WorldTopoMap",
                      "Terrain" = "Esri.WorldTerrain"
                    )
                  ),
                  selected = "OpenStreetMap.DE"),
      hr(),
      h4("Vektor-Stil"),
      selectInput("land_color", "Land-Farbe:", 
                  choices = c("Hellgrau" = "#D3D3D3", "Beige" = "#F5F5DC", "Grün" = "#90EE90", "Blaugrau" = "#778899", "Weiß" = "#FFFFFF"),
                  selected = "#D3D3D3"),
      selectInput("border_color", "Grenzen-Farbe:", 
                  choices = c("Schwarz" = "#000000", "Dunkelgrau" = "#A9A9A9", "Braun" = "#8B4513", "Weiß" = "#FFFFFF"),
                  selected = "#000000"),
      selectInput("water_color", "Wasser-Farbe:", 
                  choices = c("Hellblau" = "#ADD8E6", "Dunkelblau" = "#000080", "Weiß" = "#FFFFFF", "Transparent" = "transparent"),
                  selected = "#ADD8E6"),
      hr(),
      h4("Eigene Beschriftung (VektorVorschau)"),
      actionButton("start_draw_btn", "Pfad zeichnen", icon = icon("pen")),
      actionButton("finish_draw_btn", "Zeichnen beenden", icon = icon("check")),
      actionButton("clear_draw_btn", "Alle eigenen Labels löschen", icon = icon("trash")),
      helpText("Zeichnen: Klicke mehrfach in die Vektor-Vorschau. 'Zeichnen beenden' öffnet Texteingabe."),
      hr(),
      h4("Beschriftung"),
      checkboxInput("show_land_labels", "Ländernamen zeigen", value = TRUE),
      checkboxInput("show_city_labels", "Hauptstädte zeigen", value = TRUE),
      checkboxInput("show_relief", "Höhenzüge (Relief) anzeigen", value = FALSE),
      hr(),
      h4("Export"),
      downloadButton("export_leaflet", "Leaflet als PNG"),
      downloadButton("export_vector_png", "Vektor als PNG"),
      downloadButton("export_vector_emf", "Vektor als EMF (Win)"),
      hr(),
      helpText("Der Vektor-Ausschnitt wird automatisch an den Leaflet-Ausschnitt angepasst."),
      helpText("Massstab der Natural Earth Daten wird basierend auf dem Ausschnitt gewählt.")
    ),
    
    mainPanel(
      # Dynamische Stylesheet-Injektion für Resize ohne Widget-Destruction
      uiOutput("dynamic_css"),
      tabsetPanel(id = "main_tabs",
        tabPanel("Interaktive Karte", value = "map_tab", 
          tags$div(id = "map_wrapper", leafletOutput("map", width = "100%", height = "100%"))
        ),
        tabPanel("Vektor Vorschau", value = "vector_tab",
          tags$div(id = "vector_wrapper", 
                   plotOutput("vector_plot", width = "100%", height = "100%", click = "plot_click")),
          hr(),
          h4("Gespeicherte Pfade & Labels"),
          DTOutput("labels_dt"),
          br(),
          actionButton("delete_selected_label_btn", "Gewähltes Label löschen", icon = icon("trash"), class = "btn-danger")
        )
      )
    )
  )
)

# --- Server ---

server <- \(input, output, session) {
  
  # --- Database Permanency Logic ---
  chart_list <- reactiveVal(get_all_charts())
  
  output$load_chart_ui <- renderUI({
    charts <- chart_list()
    if (nrow(charts) > 0) {
      choices <- setNames(charts$id, paste(charts$id, "-", charts$name))
      selectInput("load_chart_id", "Chart laden:", choices = c("--- Bitte wählen ---" = "", choices), selected = "")
    } else {
      selectInput("load_chart_id", "Chart laden:", choices = c("Keine Charts vorhanden" = ""), selected = "")
    }
  })
  
  observeEvent(input$save_chart_btn, {
    name <- trimws(input$chart_name)
    if (name == "") {
      showNotification("Bitte einen Chart-Namen eingeben!", type = "error")
      return()
    }
    
    state <- list(
      map_provider = input$map_provider,
      land_color = input$land_color,
      border_color = input$border_color,
      water_color = input$water_color,
      show_land_labels = input$show_land_labels,
      show_city_labels = input$show_city_labels,
      show_relief = input$show_relief,
      width_cm = input$width_cm,
      height_cm = input$height_cm,
      map_projection = input$map_projection,
      map_bounds = bbox(),
      custom_labels = custom_labels()
    )
    
    state_json <- jsonlite::toJSON(state, auto_unbox = TRUE, force = TRUE)
    
    con <- dbConnect(RSQLite::SQLite(), DB_PATH)
    exists <- dbGetQuery(con, "SELECT id FROM charts WHERE name = ?", params = list(name))
    if (nrow(exists) > 0) {
      dbExecute(con, "UPDATE charts SET state_json = ?, created_at = CURRENT_TIMESTAMP WHERE name = ?", params = list(state_json, name))
      showNotification(sprintf("Chart '%s' aktualisiert!", name), type = "message")
    } else {
      dbExecute(con, "INSERT INTO charts (name, state_json) VALUES (?, ?)", params = list(name, state_json))
      showNotification(sprintf("Chart '%s' gespeichert!", name), type = "message")
    }
    dbDisconnect(con)
    
    chart_list(get_all_charts())
  })
  
  observeEvent(input$load_chart_id, {
    cid <- input$load_chart_id
    req(cid, cid != "")
    
    con <- dbConnect(RSQLite::SQLite(), DB_PATH)
    res <- dbGetQuery(con, "SELECT name, state_json FROM charts WHERE id = ?", params = list(cid))
    dbDisconnect(con)
    
    if (nrow(res) == 1) {
      updateTextInput(session, "chart_name", value = res$name[1])
      
      state <- jsonlite::fromJSON(res$state_json[1], simplifyVector = TRUE)
      
      if (!is.null(state$map_provider)) updateSelectInput(session, "map_provider", selected = state$map_provider)
      if (!is.null(state$land_color)) updateSelectInput(session, "land_color", selected = state$land_color)
      if (!is.null(state$border_color)) updateSelectInput(session, "border_color", selected = state$border_color)
      if (!is.null(state$water_color)) updateSelectInput(session, "water_color", selected = state$water_color)
      
      if (!is.null(state$show_land_labels)) updateCheckboxInput(session, "show_land_labels", value = state$show_land_labels)
      if (!is.null(state$show_city_labels)) updateCheckboxInput(session, "show_city_labels", value = state$show_city_labels)
      if (!is.null(state$show_relief)) updateCheckboxInput(session, "show_relief", value = state$show_relief)
      
      if (!is.null(state$width_cm)) updateNumericInput(session, "width_cm", value = state$width_cm)
      if (!is.null(state$height_cm)) updateNumericInput(session, "height_cm", value = state$height_cm)
      if (!is.null(state$map_projection)) updateSelectInput(session, "map_projection", selected = state$map_projection)
      
      b <- state$map_bounds
      if (!is.null(b) && is.list(b)) {
        leafletProxy("map") |> fitBounds(b$west, b$south, b$east, b$north)
        valid_bbox(list(west = b$west, east = b$east, south = b$south, north = b$north))
      }
      
      cl_json <- state$custom_labels
      if (!is.null(cl_json) && length(cl_json) > 0) {
        if (is.data.frame(cl_json)) {
          cl_restored <- lapply(1:nrow(cl_json), function(i) {
            list(id = cl_json$id[i], text = cl_json$text[i], coords = as.data.frame(cl_json$coords[[i]]))
          })
          custom_labels(cl_restored)
        } else if (is.list(cl_json)) {
          cl_restored <- lapply(cl_json, function(l) list(id = l$id, text = l$text, coords = as.data.frame(l$coords)))
          custom_labels(cl_restored)
        }
      } else {
        custom_labels(list())
      }
      
      showNotification(sprintf("Chart '%s' geladen!", res$name[1]), type = "message")
    }
  })
  
  observeEvent(input$delete_chart_btn, {
    name <- trimws(input$chart_name)
    cid <- input$load_chart_id
    if (cid == "") {
      showNotification("Bitte einen zu löschenden Chart aus dem Dropdown wählen!", type = "warning")
      return()
    }
    
    showModal(modalDialog(
      title = "Chart löschen",
      sprintf("Möchten Sie den Chart '%s' wirklich löschen?", name),
      footer = tagList(
        modalButton("Abbrechen"),
        actionButton("confirm_delete_chart", "Ja, löschen", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirm_delete_chart, {
    cid <- input$load_chart_id
    con <- dbConnect(RSQLite::SQLite(), DB_PATH)
    dbExecute(con, "DELETE FROM charts WHERE id = ?", params = list(cid))
    dbDisconnect(con)
    
    chart_list(get_all_charts())
    removeModal()
    showNotification("Chart gelöscht!", type = "message")
    
    # Defaults wiederherstellen
    updateTextInput(session, "chart_name", value = "")
    updateSelectInput(session, "load_chart_id", selected = "")
    updateSelectInput(session, "map_provider", selected = "OpenStreetMap.DE")
    updateSelectInput(session, "land_color", selected = "#D3D3D3")
    updateSelectInput(session, "border_color", selected = "#000000")
    updateSelectInput(session, "water_color", selected = "#ADD8E6")
    updateCheckboxInput(session, "show_land_labels", value = TRUE)
    updateCheckboxInput(session, "show_city_labels", value = TRUE)
    updateCheckboxInput(session, "show_relief", value = FALSE)
    updateNumericInput(session, "width_cm", value = 15)
    updateNumericInput(session, "height_cm", value = 10)
    updateSelectInput(session, "map_projection", selected = "EPSG:3857")
    custom_labels(list())
  })
  
  # Reactive values for dimensions
  dims <- reactive({
    list(
      w = paste0(input$width_cm * 37.8, "px"),
      h = paste0(input$height_cm * 37.8, "px"),
      w_cm = input$width_cm,
      h_cm = input$height_cm
    )
  })
  
  output$dynamic_css <- renderUI({
    d <- dims()
    tags$style(HTML(paste0(
      "#map_wrapper, #vector_wrapper { ",
      "width: ", d$w, "; ",
      "height: ", d$h, "; ",
      "transition: width 0.3s, height 0.3s;", # Smooth resize
      "}"
    )))
  })
  
  output$map <- renderLeaflet({
    # Isolation: Wir rendern die Karte nur einmal initial.
    # Änderungen am Provider werden per leafletProxy() nachgeschossen.
    leaflet(options = leafletOptions(zoomSnap = 0, zoomDelta = 0.5)) |>
      addProviderTiles(isolate(input$map_provider)) |>
      setView(lng = 10, lat = 50, zoom = 4)
  })
  
  # NEU: Provider-Wechsel ohne View-Reset
  observeEvent(input$map_provider, {
    leafletProxy("map") |>
      clearTiles() |>
      addProviderTiles(input$map_provider)
  })
  
  # Bounding Box des Leaflet-Ausschnitts
  # Wichtig: Leaflet wirft einen vollen [ -180, 180 ] Ausschnitt, wenn das Fenster durch Anzeige
  # von Notifications oder Tabs im Hintergrund resizt wird. Wir cachen den letzten gültigen Wert!
  valid_bbox <- reactiveVal(list(west = -180, east = 180, south = -90, north = 90))
  
  observe({
    b <- input$map_bounds
    tab <- input$main_tabs
    if (!is.null(b)) {
      # Nur aktualisieren, wenn Leaflet auch sichtbar ist (vermeidet 'Zoom-Out' Bug bei Tab-Wechsel)
      if (is.null(tab) || tab == "map_tab") {
        valid_bbox(list(
          west = b$west,
          east = b$east,
          south = b$south,
          north = b$north
        ))
      }
    }
  })
  
  bbox <- reactive({ valid_bbox() })
  
  current_scale <- reactive({
    get_ne_scale(bbox())
  })
  
  # Reactive Natural Earth Data (Countries)
  ne_data <- reactive({
    get_local_ne_data("countries", current_scale())
  })
  
  # Reactive Natural Earth Data (Cities)
  city_data <- reactive({
    get_local_ne_data("cities", current_scale())
  })

  # NEW: Reactive Bounding Box Polygon (Aspekt-Ratio / Projection korrigiert)
  bbox_poly <- reactive({
    b <- bbox()
    req(b)
    
    target_crs <- input$map_projection
    if (is.null(target_crs) || target_crs == "") target_crs <- "EPSG:3857"
    
    # Berechne die exakten kartesischen Limits des Outputs in der Projektion
    ex_proj <- get_expanded_bbox_proj(b, target_crs, input$width_cm, input$height_cm)
    
    # Baue das Rechteck-Polygon in der Zielfläche
    exp_poly_proj <- st_polygon(list(matrix(c(
      ex_proj$xmin, ex_proj$ymin,
      ex_proj$xmax, ex_proj$ymin,
      ex_proj$xmax, ex_proj$ymax,
      ex_proj$xmin, ex_proj$ymax,
      ex_proj$xmin, ex_proj$ymin
    ), ncol=2, byrow=TRUE)))
    exp_sfc_proj <- st_sfc(exp_poly_proj, crs = target_crs)
    
    # Und drücke diese Fläche zurück nach WGS84 für alle raster & sf_intersects Abfragen
    exp_sfc_4326 <- suppressWarnings(st_transform(exp_sfc_proj, 4326))
    
    # Um sicherzugehen, dass alle Objekte abgefangen werden, als sauberes Rechteck (BBox) stülpen
    exp_bb_4326 <- st_bbox(exp_sfc_4326)
    
    st_as_sfc(st_bbox(exp_bb_4326, crs = 4326))
  })

  # NEW: Processed Countries for Plotting
  countries_plot <- reactive({
    countries <- ne_data()
    # FIX: Polygon-Fehler (horizontale Linien / Tearing)
    try({
      # 1. Datumsgrenze sauber auftrennen
      countries <- st_wrap_dateline(countries, options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"))
      # 2. Auf sichere Web-Mercator-Breitengrade zuschneiden
      countries <- suppressWarnings(st_crop(countries, st_bbox(c(xmin = -180, xmax = 180, ymin = -85, ymax = 85), crs = 4326)))
      # 3. Direkt in Web Mercator umwandeln
      target_crs <- input$map_projection
      if (is.null(target_crs) || target_crs == "") target_crs <- "EPSG:3857"
      countries <- st_transform(countries, target_crs)
      countries <- st_make_valid(countries)
    }, silent = TRUE)
    countries
  })

  # NEW: Refactored Label Logic as Reactive
  labels_df <- reactive({
    b <- bbox()
    req(b)
    poly <- bbox_poly()
    countries <- ne_data()
    cities <- city_data()
    
    width_deg <- abs(b$east - b$west)
    height_deg <- abs(b$north - b$south)
    max_dim_deg <- max(width_deg, height_deg)
    
    label_limit_land <- round(max(50, min(150, 200 * (10 / max_dim_deg))))
    label_limit_city <- round(max(20, min(100, 120 * (10 / max_dim_deg))))
    
    # Process Countries
    countries_visible <- countries[st_intersects(countries, poly, sparse = FALSE), ]
    if (nrow(countries_visible) > label_limit_land) {
      countries_visible <- countries_visible[order(countries_visible$labelrank, -countries_visible$pop_est), ][1:label_limit_land, ]
    }
    
    # Process Cities
    cities_visible <- cities[st_intersects(cities, poly, sparse = FALSE), ]
    if (nrow(cities_visible) > label_limit_city) {
      cities_visible <- cities_visible[order(cities_visible$MIN_ZOOM, cities_visible$SCALERANK, -cities_visible$POP_MAX), ][1:label_limit_city, ]
    }
    
    out_df <- NULL
    
    # Country Label Placement Logic
    if (input$show_land_labels && nrow(countries_visible) > 0) {
      try({
        visible_landmass <- suppressWarnings(st_intersection(st_make_valid(countries_visible), poly))
        visible_landmass <- st_make_valid(visible_landmass)
        visible_landmass <- visible_landmass[st_is(visible_landmass, c("POLYGON", "MULTIPOLYGON")), ]
        
        if (nrow(visible_landmass) > 0) {
          suppressWarnings({
            visible_polygons <- st_cast(st_cast(visible_landmass, "MULTIPOLYGON"), "POLYGON")
            visible_polygons$area <- st_area(visible_polygons)
            min_area_threshold <- st_area(poly) * 0.005
            visible_polygons <- visible_polygons[visible_polygons$area >= min_area_threshold, ]
            
            if (nrow(visible_polygons) > 0) {
              largest_polygons <- visible_polygons[order(visible_polygons$label_text, -as.numeric(visible_polygons$area)), ]
              largest_polygons <- largest_polygons[!duplicated(largest_polygons$label_text), ]
              land_labels <- suppressWarnings(st_point_on_surface(largest_polygons))[, "label_text"]
              land_labels$label_text <- toupper(land_labels$label_text)
              
              # Overrides
              overrides <- c("VEREINIGTE ARABISCHE EMIRATE"="VAE", "VEREINIGTE STAATEN"="USA", 
                             "VEREINIGTES KÖNIGREICH"="UK", "VOLKSREPUBLIK CHINA"="CHINA", 
                             "DEMOKRATISCHE REPUBLIK KONGO"="DR KONGO", "ZENTRALAFRIKANISCHE REPUBLIK"="ZAF")
              idx <- land_labels$label_text %in% names(overrides)
              land_labels$label_text[idx] <- overrides[land_labels$label_text[idx]]
              
              land_labels$type <- "country"
              out_df <- land_labels
            }
          })
        }
      }, silent = TRUE)
    }
    
    # City Label Logic
    if (input$show_city_labels && nrow(cities_visible) > 0) {
      city_labels <- cities_visible[, "label_text"]
      city_labels$type <- "city"
      out_df <- if (is.null(out_df)) city_labels else rbind(out_df, city_labels)
    }
    
    out_df
  })

  # NEW: Reactive Shaded Relief
  relief_layer <- reactive({
    req(input$show_relief)
    if (is.null(global_relief)) return(NULL)
    
    b <- bbox()
    req(b)
    
    # 1. Polygon für den Ausschnitt
    poly_4326 <- bbox_poly()
    
    tryCatch({
      # 2. Raster zuschneiden (in EPSG:4326)
      poly_vect <- vect(poly_4326)
      relief_crop <- crop(global_relief, poly_vect)
      
      # 3. Raster transformieren für den Plot
      target_crs <- input$map_projection
      if (is.null(target_crs) || target_crs == "") target_crs <- "EPSG:3857"
      relief_proj <- project(relief_crop, target_crs)
      
      # 4. Raster exakt auf die Landmassen begrenzen (Maske),
      # damit es nicht das Wasser-Hintergrundbild (panel.background) überdeckt
      land_trans <- vect(countries_plot())
      land_trans <- project(land_trans, crs(relief_proj))
      relief_masked <- mask(relief_proj, land_trans)
      
      return(relief_masked)
    }, error = \(e) NULL)
  })
  
  # Refactored Helper to build the ggplot
  build_vector_map <- \(b, countries_trans, labels, relief, custom_lbls = list(), curr_draw = NULL, proj = "EPSG:3857", width_cm = 15, height_cm = 10) {
    req(b)
    
    p <- ggplot()
    
    if (input$show_relief && !is.null(relief)) {
      # 1) Vektoren vorbereiten
      world_vect <- vect(countries_trans)
      
      # 2) Reiner Land-Farb-Layer (Komplett deckend als Basis, OHNE Transparenz)
      p <- p + geom_spatvector(data = world_vect, fill = input$land_color, colour = NA)
      
      # 3) Raster (Ganzflächig als dynamische Licht/Schatten Maske)
      try({
        # Downsamplen für flüssiges Rendering
        relief_agg <- spatSample(relief, size = 2e6, method = "regular", as.raster = TRUE)
        r_min <- terra::global(relief_agg, "min", na.rm=TRUE)[1,1]
        r_max <- terra::global(relief_agg, "max", na.rm=TRUE)[1,1]
        vals <- terra::values(relief_agg)[,1]
        
        if (!is.na(r_min) && !is.na(r_max) && r_min != r_max) {
          # Normieren
          norm_vals <- (vals - r_min) / (r_max - r_min)
          na_idx <- is.na(norm_vals)
          hex_colors <- rep("#FFFFFF00", length(norm_vals)) # Transparentes Wasser
          
          if (any(!na_idx)) {
            v <- norm_vals[!na_idx]
            
            # Flaches Gelände in SR_LR liegt bei ca. 60-70% Helligkeit nach MinMax-Skalierung.
            # Abweichungen davon wandeln wir dynamisch in eine Schatten- oder Lichtebene um.
            midpoint <- 0.65
            
            r_col <- ifelse(v >= midpoint, 255, 0)
            g_col <- ifelse(v >= midpoint, 255, 0)
            b_col <- ifelse(v >= midpoint, 255, 0)
            
            # Alpha-Kanal bestimmt, wie stark Licht (Weiß) oder Schatten (Schwarz) durchdringt.
            # Auf flachem Land (midpoint) ist Alpha 0 -> Die Landfarbe erstrahlt pur zu 100%.
            alpha_val <- ifelse(v < midpoint,
                                (midpoint - v) / midpoint * 180,        # Täler -> dunkler Schatten
                                (v - midpoint) / (1 - midpoint) * 160)  # Gipfel -> weißes Licht
                                
            alpha_val <- pmax(0, pmin(255, alpha_val))
            
            hex_colors[!na_idx] <- rgb(r_col, g_col, b_col, alpha_val, maxColorValue=255)
          }
          
          # Matrix für ggplot2 generieren
          rast_matrix <- matrix(hex_colors, nrow = nrow(relief_agg), ncol = ncol(relief_agg), byrow = TRUE)
          ex <- terra::ext(relief_agg)
          
          p <- p + annotation_raster(rast_matrix, xmin = ex[1], xmax = ex[2], ymin = ex[3], ymax = ex[4], interpolate = TRUE)
        }
      }, silent = TRUE)
      
      # 4) Nur die Länder-Grenzen exakt über den Schatten zeichnen (ohne Füllung)
      p <- p + 
        geom_spatvector(data = world_vect, fill = NA, colour = input$border_color, linewidth = 0.3)
      
    } else {
      # Normales Rendern ohne Relief
      p <- p + geom_sf(data = countries_trans, fill = input$land_color, color = input$border_color)
    }
    
    if (input$show_city_labels && !is.null(labels)) {
      cities_pts <- labels[labels$type == "city", ]
      if (nrow(cities_pts) > 0) {
        # suppressWarnings da geom_sf hier kein echtes Problem hat
        suppressWarnings({
          p <- p + geom_sf(data = cities_pts, size = 1.5, color = "darkred")
        })
      }
    }
    
    if (!is.null(labels) && nrow(labels) > 0) {
      suppressWarnings({
        p <- p + 
        geom_text_repel(
          data = labels,
          aes(label = label_text, geometry = geometry, color = type, size = type, fontface = type),
          stat = "sf_coordinates", box.padding = 0.1, point.padding = 0.1,
          nudge_y = ifelse(labels$type == "city", 0.3, 0),
          direction = "both", segment.color = NA, max.overlaps = Inf, force = 1,
          bg.color = "white", bg.r = 0.1, seed = 42, show.legend = FALSE
        ) +
          scale_color_manual(values = c("country" = "#555555", "city" = "black")) +
          scale_size_manual(values = c("country" = 3.5, "city" = 3)) +
          scale_discrete_manual(aesthetics = "fontface", values = c("country" = "bold", "city" = "plain"))
      })
    }
    
    # 5) Custom Interactive Labels Layer (geomtextpath)
    if (length(custom_lbls) > 0) {
      for (lbl in custom_lbls) {
        # Create sf LINESTRING or POINT from coordinates
        m <- as.matrix(lbl$coords)
        if (nrow(m) == 1) {
          is_latlon <- abs(m[1,1]) <= 180 && abs(m[1,2]) <= 90
          in_crs <- ifelse(is_latlon, 4326, 3857)
          
          pt <- st_point(m[1, 1:2])
          pt_sfc <- st_sfc(pt, crs = in_crs)
          pt_sfc <- st_transform(pt_sfc, proj)
          sf_obj <- st_sf(text = lbl$text, geometry = pt_sfc)
          
          suppressWarnings({
            p <- p + geom_sf(data = sf_obj, color = "black", size = 2)
            p <- p + geom_text_repel(
              data = sf_obj, aes(label = text, geometry = geometry),
              stat = "sf_coordinates", size = 5, fontface = "bold",
              color = "black", bg.color = "white", bg.r = 0.15, seed = 42
            )
          })
        } else if (nrow(m) >= 2) {
          # Dynamische CRS Erkennung
          is_latlon <- all(abs(m[,1]) <= 180) && all(abs(m[,2]) <= 90)
          in_crs <- ifelse(is_latlon, 4326, 3857)
          
          # Native Spline-Interpolation über Pfad-Distanz (funktioniert immer, ohne Zusatzpakete)
          orig_ls <- st_sfc(st_linestring(m), crs = in_crs)
          proj_ls <- st_transform(orig_ls, proj)
          m_proj <- st_coordinates(proj_ls)[, 1:2, drop = FALSE]
          
          if (nrow(m_proj) >= 3) {
            dists <- c(0, cumsum(sqrt(diff(m_proj[,1])^2 + diff(m_proj[,2])^2)))
            if (max(dists) > 0) {
              t_seq <- seq(0, max(dists), length.out = 200)
              x_spl <- spline(dists, m_proj[,1], xout = t_seq)$y
              y_spl <- spline(dists, m_proj[,2], xout = t_seq)$y
              m_smooth <- cbind(x_spl, y_spl)
            } else { m_smooth <- m_proj }
          } else { m_smooth <- m_proj }
          
          ls_sfc <- st_sfc(st_linestring(m_smooth), crs = proj)
          
          # Text auflockern und über den GESAMTEN Pfad verteilen
          # Leerzeichen explizit als Platzhalter-Zeichen mitzählen!
          txt_pure <- trimws(lbl$text)
          if (nchar(txt_pure) > 1 && nchar(txt_pure) <= 25) {
            chars <- strsplit(txt_pure, "")[[1]]
            chars <- chars[chars != ""] # Leerstings rausfiltern, aber Leerzeichen (" ") behalten!
            n_chars <- length(chars)
            h_seq <- seq(0.05, 0.95, length.out = n_chars)
            
            for (i in seq_len(n_chars)) {
               sf_char <- st_sf(text = chars[i], geometry = ls_sfc)
               suppressWarnings({
                 p <- p + geom_textsf(
                   data = sf_char, aes(label = text),
                   hjust = h_seq[i], vjust = 0.5,
                   family = "sans", fontface = "bold", size = 4, color = "black",
                   linetype = "blank", text_only = TRUE
                 )
               })
            }
          } else {
            sf_obj <- st_sf(text = txt_pure, geometry = ls_sfc)
            suppressWarnings({
              p <- p + geom_textsf(
                data = sf_obj, aes(label = text),
                hjust = 0.5, vjust = 0.5,
                family = "sans", fontface = "bold", size = 4, color = "black",
                linetype = "blank", text_only = TRUE
              )
            })
          }
        }
      }
    }
    
    # 6) Render in-progress drawing points and line
    if (!is.null(curr_draw) && nrow(curr_draw) > 0) {
      is_latlon <- all(abs(curr_draw$lon) <= 180) && all(abs(curr_draw$lat) <= 90)
      in_crs <- ifelse(is_latlon, 4326, 3857)
      
      draw_sf <- st_as_sf(curr_draw, coords = c("lon", "lat"), crs = in_crs)
      draw_sf <- st_transform(draw_sf, proj)
      
      suppressWarnings({
        p <- p + geom_sf(data = draw_sf, color = "red", size = 2)
      })
      
      if (nrow(curr_draw) >= 2) {
        m <- as.matrix(curr_draw)
        orig_ls <- st_sfc(st_linestring(m), crs = in_crs)
        proj_ls <- st_transform(orig_ls, proj)
        m_proj <- st_coordinates(proj_ls)[, 1:2, drop = FALSE]
        
        if (nrow(m_proj) >= 3) {
          dists <- c(0, cumsum(sqrt(diff(m_proj[,1])^2 + diff(m_proj[,2])^2)))
          if (max(dists) > 0) {
            t_seq <- seq(0, max(dists), length.out = 200)
            x_spl <- spline(dists, m_proj[,1], xout = t_seq)$y
            y_spl <- spline(dists, m_proj[,2], xout = t_seq)$y
            m_smooth <- cbind(x_spl, y_spl)
          } else { m_smooth <- m_proj }
        } else { m_smooth <- m_proj }
        
        ls_sfc <- st_sfc(st_linestring(m_smooth), crs = proj)
        
        suppressWarnings({
          p <- p + geom_sf(data = ls_sfc, color = "red", linetype = "dashed", linewidth = 1)
        })
      }
    }
    
    ex_proj <- get_expanded_bbox_proj(b, proj, width_cm, height_cm)
    
    p <- p + coord_sf(
      xlim = c(ex_proj$xmin, ex_proj$xmax), ylim = c(ex_proj$ymin, ex_proj$ymax),
      crs = proj, default_crs = proj, datum = NA, label_axes = "----", expand = FALSE
    )
    
    p + theme_void() + 
      theme(
        plot.margin = margin(0,0,0,0),
        panel.background = element_rect(fill = input$water_color, color = NA)
      )
  }
  
  output$vector_plot <- renderPlot({
    req(bbox())
    proj <- input$map_projection
    if (is.null(proj) || proj == "") proj <- "EPSG:3857"
    p <- build_vector_map(bbox(), countries_plot(), labels_df(), relief_layer(), custom_labels(), current_drawing(), proj, input$width_cm, input$height_cm)
    suppressWarnings(print(p))
  })
  
  # --- Interactive Drawing Logic ---
  
  # State
  is_drawing <- reactiveVal(FALSE)
  current_drawing <- reactiveVal(data.frame(lon=numeric(), lat=numeric()))
  custom_labels <- reactiveVal(list())
  
  observeEvent(input$start_draw_btn, {
    is_drawing(TRUE)
    current_drawing(data.frame(lon=numeric(), lat=numeric()))
    showNotification("Zeichenmodus aktiv. Klicke Punkte in die Vorschau.", type = "message")
  })
  
  observeEvent(input$plot_click, {
    if (is_drawing()) {
      raw_x <- input$plot_click$x
      raw_y <- input$plot_click$y
      
      if (!is.null(raw_x) && !is.null(raw_y)) {
        # Wenn coord_sf in Shiny fehlschlägt, liefert es relative Werte von 0 bis 1 (NDC).
        if (abs(raw_x) <= 2 && abs(raw_y) <= 2) {
          b <- bbox()
          proj <- input$map_projection
          if (is.null(proj) || proj == "") proj <- "EPSG:3857"
          
          # Nutze das exakt via Aspect Ratio berechnete Bounding-Box Fenster
          ex_proj <- get_expanded_bbox_proj(b, proj, input$width_cm, input$height_cm)
          xmin <- ex_proj$xmin
          ymin <- ex_proj$ymin
          xmax <- ex_proj$xmax
          ymax <- ex_proj$ymax
          
          # Mappen der relativen Klicks 0-1 auf die Projektionkoordinaten
          real_x_proj <- xmin + raw_x * (xmax - xmin)
          real_y_proj <- ymin + raw_y * (ymax - ymin)
          
          # Wandle IMMER nach 4326 zurück für saubere Speicherung
          pt_proj <- st_sfc(st_point(c(real_x_proj, real_y_proj)), crs = proj)
          pt_4326 <- st_transform(pt_proj, 4326)
          
          real_x <- st_coordinates(pt_4326)[1, "X"]
          real_y <- st_coordinates(pt_4326)[1, "Y"]
        } else {
          real_x <- raw_x
          real_y <- raw_y
        }
        
        curr <- current_drawing()
        new_pt <- data.frame(lon = real_x, lat = real_y)
        current_drawing(rbind(curr, new_pt))
        
        showNotification(sprintf("Gesetzt: X=%.0f, Y=%.0f", real_x, real_y), type="message", duration=2)
      }
    }
  })
  
  observeEvent(input$finish_draw_btn, {
    if (is_drawing() && nrow(current_drawing()) >= 1) {
      # Pop-up for Text
      showModal(modalDialog(
        title = "Beschriftungstext eingeben",
        textInput("custom_label_text", "Text:", placeholder = "z.B. Die Alpen..."),
        footer = tagList(
          modalButton("Abbrechen"),
          actionButton("save_custom_label", "Speichern", class = "btn-primary")
        )
      ))
    } else {
      is_drawing(FALSE)
      current_drawing(data.frame(lon=numeric(), lat=numeric()))
    }
  })
  
  observeEvent(input$save_custom_label, {
    text <- input$custom_label_text
    coords <- current_drawing()
    
    if (text != "" && nrow(coords) >= 1) {
      lbls <- custom_labels()
      new_entry <- list(
        id = format(Sys.time(), "%Y%m%d%H%M%S"),
        text = text,
        coords = coords
      )
      custom_labels(append(lbls, list(new_entry)))
    }
    
    # Reset
    is_drawing(FALSE)
    current_drawing(data.frame(lon=numeric(), lat=numeric()))
    removeModal()
  })
  
  observeEvent(input$clear_draw_btn, {
    custom_labels(list())
    current_drawing(data.frame(lon=numeric(), lat=numeric()))
    is_drawing(FALSE)
  })
  
  # --- Data Table für Custom Labels ---
  output$labels_dt <- renderDT({
    lbls <- custom_labels()
    if (length(lbls) == 0) return(data.frame(Text = character(), Koordinaten = character()))
    
    df <- do.call(rbind, lapply(lbls, function(x) {
      # Ist es Legacy (3857) oder Neu (4326)?
      m <- as.matrix(x$coords)
      is_latlon <- all(abs(m[,1]) <= 180) && all(abs(m[,2]) <= 90)
      in_crs <- ifelse(is_latlon, 4326, 3857)
      
      pts <- st_as_sf(x$coords, coords=c("lon", "lat"), crs=in_crs)
      pts_4326 <- st_transform(pts, 4326)
      c_4326 <- st_coordinates(pts_4326)
      coord_str <- paste(apply(c_4326, 1, function(row) sprintf("%.3f,%.3f", row[1], row[2])), collapse = " | ")
      
      data.frame(
        Text = x$text,
        Koordinaten = coord_str,
        stringsAsFactors = FALSE
      )
    }))
    
    # ordering = FALSE hält die Zeilenstruktur synchron zur Liste custom_labels()
    datatable(df, selection = "single", editable = TRUE, rownames = FALSE, options = list(pageLength = 5, dom = "t", ordering = FALSE))
  })
  
  observeEvent(input$labels_dt_cell_edit, {
    info <- input$labels_dt_cell_edit
    lbls <- custom_labels()
    r <- info$row # 1-indexed (Zeile)
    c <- info$col # 0-indexed (Spalte, wg. rownames=FALSE)
    val <- info$value
    
    if (r > 0 && r <= length(lbls)) {
      if (c == 0) {
        # Text editiert
        lbls[[r]]$text <- val
      } else if (c == 1) {
        # Koordinaten editiert ("10.123,45.678 | 11.23,46.1")
        parts <- strsplit(val, "\\|")[[1]]
        parts <- trimws(parts)
        valid_pts <- list()
        
        for (pt_str in parts) {
          nums <- suppressWarnings(as.numeric(strsplit(pt_str, ",")[[1]]))
          if (length(nums) == 2 && !any(is.na(nums))) valid_pts[[length(valid_pts) + 1]] <- nums
        }
        
        if (length(valid_pts) > 0) {
          # Clicks werden systemweit immer exakt als 4326 gesichert
          m_4326 <- do.call(rbind, valid_pts)
          lbls[[r]]$coords <- data.frame(lon = m_4326[,1], lat = m_4326[,2])
        }
      }
      custom_labels(lbls)
    }
  })
  
  observeEvent(input$delete_selected_label_btn, {
    sel <- input$labels_dt_rows_selected
    if (!is.null(sel) && length(sel) > 0) {
      lbls <- custom_labels()
      lbls <- lbls[-sel]
      custom_labels(lbls)
    }
  })
  
  # --- Export Handlers ---
  
  output$export_leaflet <- downloadHandler(
    filename = "map_leaflet.png",
    content = \(file) {
      b <- bbox()
      temp_map <- leaflet(options = leafletOptions(zoomControl = FALSE)) |>
        addProviderTiles(input$map_provider) |>
        fitBounds(b$west, b$south, b$east, b$north)
      
      temp_html <- tempfile(fileext = ".html")
      saveWidget(temp_map, temp_html, selfcontained = FALSE)
      
      webshot(
        url = temp_html, 
        file = file, 
        vwidth = input$width_cm * 37.8, 
        vheight = input$height_cm * 37.8,
        cliprect = "viewport"
      )
    }
  )
  
  output$export_vector_png <- downloadHandler(
    filename = "map_vector.png",
    content = \(file) {
      proj <- input$map_projection
      if (is.null(proj) || proj == "") proj <- "EPSG:3857"
      p <- build_vector_map(bbox(), countries_plot(), labels_df(), relief_layer(), custom_labels(), NULL, proj, input$width_cm, input$height_cm)
      ggsave(
        filename = file, 
        plot = p, 
        width = input$width_cm, 
        height = input$height_cm, 
        units = "cm", 
        dpi = 300
      )
    }
  )
  
  output$export_vector_emf <- downloadHandler(
    filename = "map_vector.emf",
    content = \(file) {
      proj <- input$map_projection
      if (is.null(proj) || proj == "") proj <- "EPSG:3857"
      p <- build_vector_map(bbox(), countries_plot(), labels_df(), relief_layer(), custom_labels(), NULL, proj, input$width_cm, input$height_cm)
      win.metafile(
        filename = file, 
        width = input$width_cm / 2.54, 
        height = input$height_cm / 2.54
      )
      print(p)
      dev.off()
    }
  )
}

shinyApp(ui, server)
