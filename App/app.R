library(dplyr)
library(tidyr)
library(leaflet) # for displaying maps
library(sf) # for handling coordinates
library(geosphere) # for accurate distance calculation using coordinates
library(ggrepel) # for marking on graphs without overlap
library(dbscan)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(htmltools)


source("../code/flood_risk_visualizer.R")
source("../code/feature_influence.R")
source("../code/investment_predictor.R")
source("../code/infrastructure_need_analyzer.R")

ui <- dashboardPage(
  dashboardHeader(
    title = div(
      "Chennai Property Intelligence Platform",
      style = "width:100%; text-align:center; font-weight:700;"
    )
  ),
  
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Flood Risk Visualizer", tabName = "flood_explorer", icon = icon("water")),
      menuItem("Feature Influence Explorer", tabName = "influence", icon = icon("chart-line")),
      menuItem("Investment Predictor", tabName = "investment_predictor", icon = icon("chart-bar")),
      menuItem("Infrastructure Need Analyzer", tabName = "facility_need", icon = icon("map-marker-alt")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        /* Dark blue theme */
        .main-header .navbar, .main-header .logo, .main-sidebar { background-color: #003366 !important; }
        .skin-blue .sidebar a { color: #fff !important; }
        .skin-blue .sidebar a:hover { background-color: #0059b3 !important; }
        .skin-blue .sidebar-menu > li.active > a { background-color: #004080 !important; font-weight: bold; }
        .box.box-primary { border-top-color: #003366 !important; }
        .box.box-primary > .box-header { background: #003366 !important; color: #fff !important; }
        
        /* Home cards */
        .home-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(280px, 1fr)); gap: 20px; margin-top: 20px; }
        .home-card { display: flex; align-items: center; gap: 15px; border-radius: 15px; padding: 25px; 
          height: 120px; color: white; font-size: 20px; font-weight: 600; cursor: pointer; 
          transition: transform 0.2s, box-shadow 0.2s; }
        .home-card:hover { transform: scale(1.03); box-shadow: 0 4px 15px rgba(0,0,0,0.2); }
        .home-icon { font-size: 40px; }
      "))
    ),
    
    tabItems(
      tabItem(
        tabName = "home",
        fluidRow(
          box(
            width = 12,
            title = "Welcome to the Chennai Property Intelligence Platform",
            status = "primary",
            solidHeader = TRUE,
            "Explore Chennai's spatial data through interactive modules. Click a section below to begin."
          )
        ),
        div(
          class = "home-grid",
          
          div(
            class = "home-card",
            style = "background: linear-gradient(135deg, deepskyblue, turquoise);",
            icon("water", class = "home-icon"),
            "Flood Risk Visualizer",
            onclick = "Shiny.setInputValue('nav', {tab: 'flood_explorer'});"
          ),
          
          div(
            class = "home-card",
            style = "background: linear-gradient(135deg, mediumseagreen, lightseagreen);",
            icon("chart-line", class = "home-icon"),
            "Feature Influence Explorer",
            onclick = "Shiny.setInputValue('nav', {tab: 'influence'});"
          ),
          
          div(
            class = "home-card",
            style = "background: linear-gradient(135deg, gold, darkorange);",
            icon("chart-bar", class = "home-icon"),
            "Investment Predictor",
            onclick = "Shiny.setInputValue('nav', {tab: 'investment_predictor'});"
          ),
          
          div(
            class = "home-card",
            style = "background: linear-gradient(135deg, mediumslateblue, violet);",
            icon("map-marker-alt", class = "home-icon"),
            "Infrastructure Need Analyzer",
            onclick = "Shiny.setInputValue('nav', {tab: 'facility_need'});"
          )
        )
      ),
      
      tabItem(
        tabName = "flood_explorer",
        box(
          width = 12,
          title = "Flood Risk Visualizer",
          status = "primary",
          solidHeader = TRUE,
          flood_map_ui("flood_map")
        )
      ),
      
      tabItem(
        tabName = "influence",
        box(
          width = 12,
          title = "Feature Influence Explorer",
          status = "primary",
          solidHeader = TRUE,
          feature_influence_ui("feature_influence")
        )
      ),
      
      tabItem(
        tabName = "investment_predictor",
        box(
          width = 12,
          title = "Investment Predictor",
          status = "primary",
          solidHeader = TRUE,
          investment_predictor_ui("investment_predictor")
        )
      ),
      
      tabItem(
        tabName = "facility_need",
        box(
          width = 12,
          title = "Infrastructure Need Analyzer — Identify Best Locations for New Amenities",
          status = "primary",
          solidHeader = TRUE,
          facility_need_ui("facility_need")
        )
      ),
      
      tabItem(
        tabName = "about",
        box(
          width = 12,
          title = "About the Chennai EDA Dashboard",
          status = "warning",
          solidHeader = TRUE,
          HTML("
      <div style='font-size:16px; line-height:1.6;'>
        <p><b>An interactive data-driven study of Chennai's real estate that uses four tools - the Flood Risk Visualizer, Feature Influence Explorer, Investment Predictor, and Infrastructure Need Analyzer - to show how floods, infrastructure, and amenities affect land prices and highlight the best areas for investment and development.</p>

        <hr style='border:1px solid #ccc;'>

        <h4><i class='fa fa-database'></i> Data Collection</h4>
        <ul>
          <li><b>Property Prices:</b> Locality-wise minimum, maximum, and average prices were scraped from MagicBricks. 
          Missing localities without valid data were excluded.</li>
          <li><b>Coordinates:</b> Geographic coordinates were obtained from the Nominatim OpenStreetMap API, 
          with manual correction for unmatched locations.</li>
          <li><b>Demographics & Proximity:</b> Population, density, and distance to airport and railway stations 
          were collected from GeoIQ using automated scraping via SerpAPI.</li>
          <li><b>Amenities:</b> Counts of 17 facility types within 2 km and 5 km radii (schools, hospitals, cafes, etc.) 
          were retrieved from the Overpass API, creating 34 amenity features per locality.</li>
        </ul>

        <hr style='border:1px solid #ccc;'>

        <h4><i class='fa fa-water'></i> Module 1: Flood Risk Visualizer</h4>
        <p>Displays <b>flooded streets and locality safety status</b> using GeoJSON flood data and spatial buffers. 
        Red markers denote flood-prone areas (within 500m of flooded streets), while green indicate safe zones. 
        An integrated search feature centers and labels any queried locality.</p>

        <h4><i class='fa fa-chart-line'></i> Module 2: Feature Influence Explorer</h4>
        <p>Uses <b>standardized linear regression</b> to identify which features most influence land prices. 
        Similar amenities were grouped into categories like medical, education, and food services. 
        Visualized through bar charts and heatmaps, positive coefficients highlight factors driving price growth 
        while negative ones indicate limiting factors.</p>

        <h4><i class='fa fa-chart-bar'></i> Module 3: Investment Predictor</h4>
        <p>Compares <b>predicted vs. actual property prices</b> to pinpoint undervalued and overvalued areas. 
        Localities where predicted prices exceed actual values are tagged as <b>high-growth</b>, while the opposite indicates 
        <b>depreciating zones</b>. Slope charts (blue for growth, red for decline) visualize this contrast for each locality.</p>

        <h4><i class='fa fa-map-marker-alt'></i> Module 4: Infrastructure Need Analyzer</h4>
        <p>Identifies <b>underserved regions</b> lacking essential facilities (schools, hospitals, banks, etc.) 
        using <b>DBSCAN spatial clustering</b>. Localities below 25% of the average facility count are grouped into clusters, 
        and each cluster's <b>Need Score</b> is computed as:</p>
        <p style='text-align:center; font-style:italic;'>Need Score = Total Population / (Average Facility Count + 1)</p>
        <p>Results are visualized as interactive <b>maps</b> and <b>lollipop charts</b>, highlighting where new infrastructure 
        investment would have the greatest impact.</p>
      </div>
    ")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  flood_map_server("flood_map")
  feature_influence_server("feature_influence")
  investment_predictor_server("investment_predictor")
  facility_need_server("facility_need")
  
  observeEvent(input$nav, {
    updateTabItems(session, "tabs", input$nav$tab)
  }, ignoreInit = TRUE)
}

shinyApp(ui, server)