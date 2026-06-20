facility_need_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$style(HTML("
      .btn-primary { background-color: darkblue !important; border-color: darkblue !important; color: white !important; }
      .btn-primary:hover { color: gold !important; }
    ")),
    
    fluidPage(
      fluidRow(
        column(
          width = 3,
          selectInput(ns("facility_select"), "Select Facility Type:",
                      choices = c("School" = "school_2km", "Hospital" = "hospital_2km", "Pharmacy" = "pharmacy_2km",
                                  "Bank" = "bank_2km", "ATM" = "atm_2km", "Restaurant" = "restaurant_2km",
                                  "Cafe" = "cafe_2km", "Fuel Station" = "fuel_2km"), selected = "school_2km"),
          radioButtons(ns("view_mode"), "Select View:", 
                       choices = c("Map View", "Lollipop Chart View"), selected = "Map View"),
          actionButton(ns("run_btn"), "Run Analysis", class = "btn-primary", 
                       style = "width: 100%; margin-top: 10px;")
        ),
        column(width = 9,
               conditionalPanel(condition = paste0("input['", ns("view_mode"), "'] == 'Map View'"),
                                leafletOutput(ns("need_map"), height = "600px")),
               conditionalPanel(condition = paste0("input['", ns("view_mode"), "'] == 'Lollipop Chart View'"),
                                plotOutput(ns("lollipop_chart"), height = "600px"))
        )
      ),
      br()
    )
  )
}

facility_need_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observeEvent(input$run_btn, {
      df_path   <- "../data/final_df.csv"
      
      if (!file.exists(df_path)) {
        showNotification("Error: final_df.csv not found.", type = "error")
        return(NULL)
      }
      
      df <- read.csv(df_path)
      req_cols <- c("lat", "lon", "population", "locality", input$facility_select)
      if (!all(req_cols %in% names(df))) {
        showNotification(paste("Missing columns:", paste(setdiff(req_cols, names(df)), collapse = ", ")), type = "error")
        return(NULL)
      }
      
      facility_col <- input$facility_select
      avg_facility <- mean(df[[facility_col]], na.rm = TRUE)
      threshold <- 0.25 * avg_facility
      underserved <- df %>% filter(.data[[facility_col]] <= threshold)
      
      if (nrow(underserved) < 5) {
        showNotification("Too few underserved localities found.", type = "warning")
        return(NULL)
      }
      
      coords <- underserved %>% select(lon, lat)
      dist_matrix <- distm(coords)
      eps_val <- 3000
      db <- dbscan(as.dist(dist_matrix), eps = eps_val, minPts = 3)
      underserved$cluster <- db$cluster
      
      cluster_summary <- underserved %>%
        filter(cluster != 0) %>%
        group_by(cluster) %>%
        summarise(n_localities = n(), total_population = sum(population, na.rm = TRUE),
                  avg_facility = mean(.data[[facility_col]], na.rm = TRUE),
                  centroid_lat = mean(lat, na.rm = TRUE), centroid_lon = mean(lon, na.rm = TRUE),
                  need_score = total_population / (avg_facility + 1), .groups = "drop") %>%
        arrange(desc(need_score))
      
      if (nrow(cluster_summary) == 0) {
        showNotification("No underserved clusters found.", type = "warning")
        return(NULL)
      }
      
      n_clusters <- length(unique(cluster_summary$cluster))
      base_colors <- c("blue", "green", "purple", "red", "brown", "magenta", "cyan", "yellow", "pink")
      cluster_colors <- setNames(rep(base_colors, length.out = n_clusters), 1:n_clusters)
      underserved <- underserved %>% filter(cluster != 0)
      underserved$color <- cluster_colors[as.character(underserved$cluster)]
      
      output$need_map <- renderLeaflet({
        leaflet() %>%
          addProviderTiles("CartoDB.Positron") %>%
          setView(lng = 80.27, lat = 13.08, zoom = 11) %>%
          addCircleMarkers(data = underserved, lat = ~lat, lng = ~lon, color = ~color,
                           radius = 5, fillOpacity = 0.7, stroke = TRUE, weight = 1,
                           label = ~paste0(locality, " (Cluster ", cluster, ")"),
                           popup = ~paste0("<b>", locality, "</b><br>", facility_col, ": ", get(facility_col), "<br>Cluster: ", cluster)) %>%
          addCircleMarkers(data = cluster_summary, lat = ~centroid_lat, lng = ~centroid_lon,
                           color = "red", fillColor = "orange", radius = 10, weight = 2, fillOpacity = 0.8,
                           label = ~paste("Cluster", cluster),
                           popup = ~paste0("<b>Cluster ", cluster, "</b><br>Need Score: ", round(need_score, 1), 
                                           "<br>Population: ", total_population, "<br>Avg ", facility_col, ": ", round(avg_facility, 2)))
      })
      
      output$lollipop_chart <- renderPlot({
        cluster_summary <- cluster_summary %>%
          arrange(cluster) %>%
          mutate(cluster = factor(cluster, levels = unique(cluster)))
        
        max_need <- max(cluster_summary$need_score)
        text_offset <- max(0.05 * max_need, 0.5)
        
        ggplot(cluster_summary, aes(x = need_score, y = cluster, color = factor(cluster))) +
          geom_segment(aes(x = 0, xend = need_score, y = cluster, yend = cluster),
                       linewidth = 1.3, alpha = 0.6) +
          geom_point(aes(size = total_population), fill = "white", shape = 21, stroke = 1.3) +
          geom_text(aes(label = paste0("Population: ", format(total_population, big.mark = ",")),
                        x = need_score + text_offset), color = "black", size = 4, hjust = 0, vjust = 0.5) +
          scale_size_continuous(range = c(4, 12)) +
          scale_color_manual(values = rep(c("steelblue", "darkorange", "darkgreen", "purple", "brown", "magenta"), 
                                          length.out = nrow(cluster_summary))) +
          labs(title = paste("Need Score by Cluster —", toupper(gsub("_2km", "", facility_col))),
               x = "Need Score (higher = greater demand)", y = "Cluster",
               size = "Total Population", color = "Cluster") +
          xlim(0, max_need + text_offset * 6) +
          theme_minimal(base_size = 14) +
          theme(legend.position = "right", panel.grid.major.y = element_blank(),
                axis.text.y = element_text(size = 12), plot.title = element_text(face = "bold"))
      })
    })
  })
}