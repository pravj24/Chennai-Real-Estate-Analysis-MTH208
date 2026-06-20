flood_map_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(HTML("
     .btn-primary {
        background-color: darkblue !important;
        border-color: darkblue !important;
        color: white !important;
      }
      .btn-primary:hover {
        color: gold !important;
      }
    ")),
    
    fluidRow(
      column(
        12,
        div(
          style = "display: flex; align-items: flex-end; justify-content: center; gap: 10px; margin-bottom: 5px;",
          tags$div(
            style = "flex: 1; max-width: 400px; margin-bottom: -1px;",
            textInput(
              ns("search_loc"),
              label = NULL,
              placeholder = "Search Locality...",
              width = "100%"
            )
          ),
          tags$div(
            style = "padding-bottom: 13px;",
            actionButton(
              ns("search_btn"),
              "Search",
              class = "btn-primary",
              style = "height: 34px; margin-bottom: 0px;"
            )
          )
        )
      )
    ),
    leafletOutput(ns("flood_map"), height = "72vh")
  )
}

flood_map_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    flood_path <- "../data/flood_data.geojson"
    csv_path   <- "../data/final_df.csv"
    
    flood_data <- st_read(flood_path, quiet = TRUE)
    final_df <- read.csv(csv_path)
    locality_sf <- st_as_sf(final_df, coords = c("lon", "lat"), crs = 4326)
    chennai_center <- c(lng = 80.27, lat = 13.08)
    
    output$flood_map <- renderLeaflet({
      legend_html <- paste0(
        '<div style="background: white; padding: 10px; border-radius: 5px; box-shadow: 0 0 15px rgba(0,0,0,0.2); border: none;">',
        '<div style="font-weight: bold; margin-bottom: 8px; font-size: 14px;">Locality Safety Status</div>',
        '<div style="display: flex; align-items: center; margin-bottom: 5px;">',
        '<div style="width: 12px; height: 12px; border-radius: 50%; background-color: red; margin-right: 8px; border: none;"></div>',
        '<span style="font-size: 13px;">Flood-Prone (Unsafe)</span>',
        '</div>',
        '<div style="display: flex; align-items: center;">',
        '<div style="width: 12px; height: 12px; border-radius: 50%; background-color: green; margin-right: 8px; border: none;"></div>',
        '<span style="font-size: 13px;">Safe Localities</span>',
        '</div>',
        '</div>'
      )
      
      leaflet(locality_sf) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = chennai_center["lng"], lat = chennai_center["lat"], zoom = 11) %>%
        addPolylines(
          data = flood_data,
          color = "blue",
          weight = 2,
          opacity = 0.7,
          group = "Flooded Streets"
        ) %>%
        addCircleMarkers(
          color = ifelse(locality_sf$flooded == 1, "red", "green"),
          radius = 6,
          label = ~paste0(locality, ": ", ifelse(flooded == 1, "Unsafe", "Safe")),
          fillOpacity = 0.7,
          stroke = FALSE,
          group = "Localities"
        ) %>%
        addLayersControl(
          overlayGroups = c("Flooded Streets", "Localities"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>%
        addControl(html = legend_html, position = "bottomright")
    })
    
    observeEvent(input$search_btn, {
      req(input$search_loc)
      search_term <- tolower(trimws(input$search_loc))
      match_idx <- which(tolower(locality_sf$locality) == search_term)
      
      if (length(match_idx) == 1) {
        loc <- locality_sf[match_idx, ]
        status <- ifelse(loc$flooded == 1, "Unsafe (within 500m of flooded area)", "Safe")
        
        leafletProxy("flood_map", data = loc) %>%
          setView(
            lng = st_coordinates(loc)[1],
            lat = st_coordinates(loc)[2],
            zoom = 15
          ) %>%
          addPopups(
            lng = st_coordinates(loc)[1],
            lat = st_coordinates(loc)[2],
            popup = paste0("<b>", loc$locality, "</b><br>Status: ", status)
          )
      } else {
        showNotification(
          "Locality not found. Check the name and try again.",
          type = "error"
        )
      }
    })
  })
}