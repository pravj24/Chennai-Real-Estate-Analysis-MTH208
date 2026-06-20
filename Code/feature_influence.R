run_feature_influence <- function(distance_mode = "5km") {
  df <- read.csv("../data/final_df.csv")
  drop_cols <- c("locality", "max_price", "min_price")
  df <- df[, !(names(df) %in% drop_cols), drop = FALSE]
  
  for (col in names(df)) {
    if (is.numeric(df[[col]])) df[[col]][is.na(df[[col]])] <- median(df[[col]], na.rm = TRUE)
  }
  
  if (distance_mode == "5km") {
    df$medical_5km <- rowSums(df[, intersect(c("hospital_5km", "clinic_5km", "doctors_5km",
                                               "pharmacy_5km", "dentist_5km"), names(df))], na.rm = TRUE)
    df$school_edu_5km <- rowSums(df[, intersect(c("school_5km", "kindergarten_5km"), names(df))], na.rm = TRUE)
    df$higher_edu_5km <- rowSums(df[, intersect(c("college_5km", "university_5km"), names(df))], na.rm = TRUE)
    df$food_bev_5km <- rowSums(df[, intersect(c("cafe_5km", "restaurant_5km", "bar_5km"), names(df))], na.rm = TRUE)
    
    drop_cols2 <- c("hospital_5km", "clinic_5km", "doctors_5km", "pharmacy_5km", "dentist_5km",
                    "school_5km", "kindergarten_5km", "college_5km", "university_5km",
                    "cafe_5km", "restaurant_5km", "bar_5km", grep("_2km$", names(df), value = TRUE),
                    "lat", "lon", "max_price", "min_price")
  } else {
    df$medical_2km <- rowSums(df[, intersect(c("hospital_2km", "clinic_2km", "doctors_2km",
                                               "pharmacy_2km", "dentist_2km"), names(df))], na.rm = TRUE)
    df$school_edu_2km <- rowSums(df[, intersect(c("school_2km", "kindergarten_2km"), names(df))], na.rm = TRUE)
    df$higher_edu_2km <- rowSums(df[, intersect(c("college_2km", "university_2km"), names(df))], na.rm = TRUE)
    df$food_bev_2km <- rowSums(df[, intersect(c("cafe_2km", "restaurant_2km", "bar_2km"), names(df))], na.rm = TRUE)
    
    drop_cols2 <- c("hospital_2km", "clinic_2km", "doctors_2km", "pharmacy_2km", "dentist_2km",
                    "school_2km", "kindergarten_2km", "college_2km", "university_2km",
                    "cafe_2km", "restaurant_2km", "bar_2km", grep("_5km$", names(df), value = TRUE),
                    "lat", "lon", "max_price", "min_price")
  }
  
  df <- df[, !(names(df) %in% drop_cols2), drop = FALSE]
  if (!"avg_price" %in% names(df)) stop("avg_price column missing.")
  
  X <- df[, setdiff(names(df), "avg_price"), drop = FALSE]
  y <- df$avg_price
  X_scaled <- scale(X)
  model <- lm(y ~ ., data = as.data.frame(X_scaled))
  coefs <- coef(model)[-1]
  
  result <- data.frame(Feature = names(coefs), Standardized_Coeff = coefs,
                       Abs_Influence = abs(coefs), stringsAsFactors = FALSE)
  result <- result[order(result$Abs_Influence, decreasing = TRUE), ]
  result$Feature <- gsub("_2km$|_5km$", "", result$Feature)
  
  feature_labels <- c("aream_km2" = "Area (km²)", "area_km2" = "Area (km²)",
                      "school_edu" = "Schools & Kindergartens", "population_density" = "Population Density",
                      "food_bev" = "Food & Beverages", "dist_station_km" = "Distance to Station (km)",
                      "dist_airport_km" = "Distance to Airport (km)", "medical" = "Medical Facilities",
                      "higher_edu" = "Colleges & Universities", "flooded" = "Flood Risk", "bank" = "Banks",
                      "atm" = "ATMs", "bus_station" = "Bus Stations", "fuel" = "Fuel Stations",
                      "parking" = "Parking")
  
  for (old_name in names(feature_labels)) {
    result$Feature[result$Feature == old_name] <- feature_labels[[old_name]]
  }
  result
}

plot_feature_bar <- function(df, distance_mode) {
  ggplot(df, aes(x = reorder(Feature, Abs_Influence), y = Standardized_Coeff, fill = Standardized_Coeff)) +
    geom_col(width = 0.7) +
    scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0) +
    coord_flip() +
    labs(title = "Feature Influence on Plot Prices", y = "Standardized Coefficient",
         x = "Features (sorted by absolute influence)", fill = "Influence") +
    theme_minimal(base_size = 14) +
    theme(legend.position = "right", plot.title = element_text(face = "bold", hjust = 0.5),
          axis.text.y = element_text(size = 12), panel.border = element_blank())
}

plot_feature_heatmap <- function(df, distance_mode) {
  df$Feature <- factor(df$Feature, levels = df$Feature[order(df$Abs_Influence, decreasing = FALSE)])
  
  ggplot(df, aes(y = Feature, x = "Influence", fill = Standardized_Coeff)) +
    geom_tile() +
    geom_text(aes(label = round(Standardized_Coeff, 3)), size = 4, color = "black") +
    scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0) +
    labs(title = "Feature Influence on Plot Prices", x = NULL, 
         y = "Features (sorted by absolute influence)", fill = "Influence") +
    theme_minimal(base_size = 14) +
    theme(legend.position = "right", axis.text.x = element_blank(),
          plot.title = element_text(face = "bold", hjust = 0.5), axis.text.y = element_text(size = 12))
}

feature_influence_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$style(HTML("
      .btn-primary { background-color: darkblue !important; border-color: darkblue !important; color: white !important; }
      .btn-primary:hover { color: gold !important; }
    "))
    ,
    
    fluidRow(
      column(width = 4,
             selectInput(ns("view_mode"), "Select View Type:", choices = c("Bar Chart", "Heatmap")),
             radioButtons(ns("distance_mode"), "Select Distance Mode:", choices = c("5km", "2km"), inline = TRUE),
             actionButton(ns("analyze_btn"), "Run Analysis", class = "btn-primary"),
             br(), br(),
             uiOutput(ns("top_features_ui"))
      ),
      column(width = 8, plotOutput(ns("feature_plot"), height = "65vh"))
    )
  )
}

feature_influence_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    feature_data <- eventReactive(input$analyze_btn, {
      isolate({ run_feature_influence(distance_mode = input$distance_mode) })
    })
    
    output$feature_plot <- renderPlot({
      req(feature_data())
      df <- feature_data()
      isolate({
        if (input$view_mode == "Bar Chart") {
          plot_feature_bar(df, input$distance_mode)
        } else {
          plot_feature_heatmap(df, input$distance_mode)
        }
      })
    })
    
    output$top_features <- renderTable({
      req(feature_data())
      df <- feature_data()
      top5 <- head(df, 5)
      data.frame(Feature = top5$Feature,
                 Effect = ifelse(top5$Standardized_Coeff > 0, "Positive (↑ Price)", "Negative (↓ Price)"))
    }, striped = TRUE, bordered = TRUE, spacing = "m")
    
    output$top_features_ui <- renderUI({
      req(feature_data())
      box(width = 12, title = "Top 5 Most Influential Features",
          solidHeader = TRUE, status = "primary", tableOutput(ns("top_features")))
    })
  })
}