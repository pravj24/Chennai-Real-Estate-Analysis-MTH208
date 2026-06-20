run_investment_prediction <- function(n_localities = 10, analysis_type = "high_growth") {
  df <- read.csv("../data/final_df.csv")
  locality_col <- df$locality
  drop_cols <- c("locality", "max_price", "min_price", "lat", "lon")
  df <- df[, !(names(df) %in% drop_cols), drop = FALSE]
  
  for (col in names(df)) {
    if (is.numeric(df[[col]])) df[[col]][is.na(df[[col]])] <- median(df[[col]], na.rm = TRUE)
  }
  
  df$medical_5km <- rowSums(df[, intersect(c("hospital_5km","clinic_5km","doctors_5km",
                                             "pharmacy_5km","dentist_5km"), names(df))], na.rm = TRUE)
  df$school_edu_5km <- rowSums(df[, intersect(c("school_5km","kindergarten_5km"), names(df))], na.rm = TRUE)
  df$higher_edu_5km <- rowSums(df[, intersect(c("college_5km","university_5km"), names(df))], na.rm = TRUE)
  df$food_bev_5km <- rowSums(df[, intersect(c("cafe_5km","restaurant_5km","bar_5km"), names(df))], na.rm = TRUE)
  
  cols_to_drop <- c("hospital_5km", "clinic_5km", "doctors_5km", "pharmacy_5km", "dentist_5km",
                    "hospital_2km", "clinic_2km", "doctors_2km", "pharmacy_2km", "dentist_2km",
                    "school_5km", "kindergarten_5km", "school_2km", "kindergarten_2km",
                    "college_5km", "university_5km", "college_2km", "university_2km",
                    "cafe_5km", "restaurant_5km", "bar_5km", "cafe_2km", "restaurant_2km", "bar_2km")
  
  df <- df[, !(names(df) %in% cols_to_drop), drop = FALSE]
  df <- df[, !grepl("_2km$", names(df)), drop = FALSE]
  
  if (!"avg_price" %in% names(df)) stop("avg_price column missing.")
  
  X <- df[, names(df) != "avg_price", drop = FALSE]
  y <- df$avg_price
  X_scaled <- scale(X)
  
  model <- lm(y ~ ., data = as.data.frame(X_scaled))
  predicted_price <- predict(model, newdata = as.data.frame(X_scaled))
  
  results <- data.frame(locality = locality_col, avg_price = y, predicted_price = predicted_price)
  results$price_difference <- results$predicted_price - results$avg_price
  
  if (analysis_type == "high_growth") {
    results <- results[order(results$price_difference, decreasing = TRUE), ]
  } else {
    results <- results[order(results$price_difference, decreasing = FALSE), ]
  }
  
  n_localities <- min(n_localities, nrow(results))
  top_n <- head(results, n_localities)
  list(data = top_n, analysis_type = analysis_type)
}

investment_predictor_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$style(HTML("
      .btn-primary { background-color: darkblue !important; border-color: darkblue !important; color: white !important; }
      .btn-primary:hover { color: gold !important; }
    ")),
    
    fluidRow(
      column(width = 12,
             fluidRow(
               column(width = 4,
                      selectInput(ns("analysis_type"), "Investment Focus:",
                                  choices = c("High-Growth Potential (Best Plots)" = "high_growth",
                                              "Likely to Depreciate (Risky Plots)" = "depreciating"), selected = "high_growth")),
               column(width = 4,
                      numericInput(ns("n_localities_manual"), "Number of Localities to Display:",
                                   value = 10, min = 1, max = 100, step = 1)),
               column(width = 4, br(),
                      actionButton(ns("predict_btn"), "Run Prediction", class = "btn-primary",
                                   style = "width: 100%; margin-top: 5px;"))
             )
      ),
      column(width = 12, hr(), plotOutput(ns("investment_plot"), height = "500px"))
    )
  )
}

investment_predictor_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    investment_data <- eventReactive(input$predict_btn, {
      n_loc <- input$n_localities_manual
      if (is.null(n_loc) || is.na(n_loc) || n_loc < 1) n_loc <- 10
      run_investment_prediction(n_localities = n_loc, analysis_type = input$analysis_type)
    })
    
    output$investment_plot <- renderPlot({
      req(investment_data())
      result <- investment_data()
      df <- result$data
      analysis_type <- result$analysis_type
      
      df_long <- df %>%
        pivot_longer(cols = c(avg_price, predicted_price), names_to = "Type", values_to = "Price") %>%
        mutate(Type = factor(Type, levels = c("avg_price", "predicted_price"),
                             labels = c("Actual", "Predicted")))
      
      blue_main <- "#007BFF"
      red_accent <- "#E53935"
      light_blue <- "#B3E5FC"
      
      if (analysis_type == "high_growth") {
        title_text <- paste0("Top ", nrow(df), " High-Growth Potential Localities")
        subtitle_text <- "Predicted > Actual: These areas are poised for appreciation."
        caption_text <- "Blue slopes = promising | Red = risk"
      } else {
        title_text <- paste0("Top ", nrow(df), " Likely to Depreciate Localities")
        subtitle_text <- "Actual > Predicted: These may be overvalued currently."
        caption_text <- "Red slopes = depreciating | Blue = undervalued"
      }
      
      text_size <- if (nrow(df) <= 10) 3.5 else if (nrow(df) <= 20) 3 else 2.5
      
      ggplot(df_long, aes(x = Type, y = Price, group = locality, color = price_difference)) +
        geom_line(size = 1.3, alpha = 0.9) +
        geom_point(size = 3) +
        scale_color_gradient2(low = red_accent, mid = "white", high = blue_main, midpoint = 0,
                              name = "Price\nDifference\n(₹)") +
        geom_text_repel(data = df_long[df_long$Type == "Predicted", ], aes(label = locality),
                        hjust = 0, size = text_size, fontface = "bold", color = "black", direction = "y",
                        nudge_x = 0.15, segment.color = light_blue, segment.size = 0.3, min.segment.length = 0,
                        box.padding = 0.5, point.padding = 0.3, force = 2, max.overlaps = Inf) +
        expand_limits(x = c(1, 2.8)) +
        labs(title = title_text, subtitle = subtitle_text, x = NULL, y = "Price (₹)", caption = caption_text) +
        theme_minimal(base_size = 14) +
        theme(plot.background = element_rect(fill = "white", color = NA),
              panel.background = element_rect(fill = "white", color = NA),
              plot.title = element_text(face = "bold", hjust = 0.5, size = 16, color = "black"),
              plot.subtitle = element_text(hjust = 0.5, size = 12, color = "black"),
              axis.text.x = element_text(size = 12, face = "bold", color = "black"),
              legend.position = "right", plot.caption = element_text(size = 10, hjust = 0.5, color = "black"))
    })
  })
}