# Folder Structure

**MTH208_TEAM2/**
* README.MD
* requirements.txt
* MTH208_TEAM2_REPORT.pdf
* **data/**
    * data_extraction_code.R
    * flood_data.geojson
    * final_df.csv
* **app/**
    * app.R
* **code/**
    * feature_influence.R
    * investment_predictor.R
    * infrastructure_need_analyzer.R
    * flood_risk_visualizer.R
* **guide_and_demo_videos/**
    * app_guide_and_demo.mp4
    * data_replication_guide.mp4

# Installing Requirements
1. Set the working directory as MTH208_TEAM2.
2. Run the command:
    packages <- readLines("requirements.txt", warn = FALSE)
    missing_pkgs <- setdiff(packages, rownames(installed.packages()))
    if (length(missing_pkgs) > 0) {
    install.packages(missing_pkgs)
    }


# Instructions to Run the App
1. Set the working directory as the MTH208_TEAM2 folder.
2. Run the command:
    shiny::runApp("app")
    
# Instructions to Replicate Data Generation Process
1. Go to [serpapi.com](https://serpapi.com/) and get a SerpAPI key (The free tier key is enough for our purpose.)
Here are a few API keys that can be used:
(i) a8d82d3ae00c72f83dc8d48d92797a2319d15a36b1d34a0a2f6d90d1553d1f5e
(ii) f7d8c1e03309a64c082032d11af1bf26d393468676fcb702818d7a1a07e9211e
3. Set the working directory as MTH208_TEAM2 folder.
4. Run the command:
    source("data/data_extraction_code.R")
4. When prompted, enter your SerpAPI key in the console and press Enter.
    
This will create a created_df.csv file in the data folder.