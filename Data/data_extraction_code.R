library(rvest)
library(dplyr)
library(stringr)
library(httr)
library(jsonlite)
library(tidyverse)
library(sf)

OVERPASS_DELAY_SEC <- 3
NOMINATIM_DELAY_SEC <- 3
AMENITY_RADIUS_2KM <- 2
AMENITY_RADIUS_5KM <- 5
MAX_RETRIES <- 10
API_KEY <- readline(prompt = "Enter your API key: ")


scrape_localities <- function() {
  url <- "https://www.magicbricks.com/Property-Rates-Trends/ALL-RESIDENTIAL-rates-in-Chennai"
  page <- read_html(url)
  localities <- page %>%
    html_elements("#localitybox ul li label") %>%
    html_text(trim = TRUE)
  return(localities)
}

get_coords <- function(locality) {
  coords_url <- "https://nominatim.openstreetmap.org/search"
  
  response <- try(GET(
    coords_url,
    query = list(q = locality, format = "json", limit = 1, addressdetails = 0),
    user_agent("R (Learning Project)") 
  ), silent = TRUE)
  
  if(inherits(response, "try-error") || status_code(response) != 200) {
    return(c(lat = NA, lon = NA))
  }
  
  data <- fromJSON(content(response, "text", encoding = "UTF-8"))
  
  if(length(data) > 0) {
    return(c(lat = as.numeric(data$lat[1]), lon = as.numeric(data$lon[1])))
  } else {
    return(c(lat = NA, lon = NA))
  }
}

get_all_coordinates <- function(localities) {
  
  coords_list <- lapply(seq_along(localities), function(i) {
    coords <- tryCatch({
      get_coords(localities[i])
    }, error = function(e) {
      return(c(NA, NA))
    })
    if(i < length(localities)) Sys.sleep(NOMINATIM_DELAY_SEC)
    return(coords)
  })
  
  coords_df <- data.frame(
    locality = localities,
    lat = sapply(coords_list, `[`, 1),
    lon = sapply(coords_list, `[`, 2),
    stringsAsFactors = FALSE
  )
  
  na_indices <- which(is.na(coords_df$lat) | is.na(coords_df$lon))
  
  if(length(na_indices) > 0) {
    
    for(i in seq_along(na_indices)) {
      idx <- na_indices[i]
      
      coords <- tryCatch({
        get_coords(coords_df$locality[idx])
      }, error = function(e) {
        return(c(NA, NA))
      })
      
      coords_df$lat[idx] <- coords[1]
      coords_df$lon[idx] <- coords[2]
      
      if(i < length(na_indices)) Sys.sleep(NOMINATIM_DELAY_SEC)
    }
  }
  
  manual_coords <- list(
    "Iyyappanthangal" = c(13.0381, 80.1354),
    "Kattankulathur" = c(12.8230, 80.0447),
    "Mambakkam Sriperumbudur" = c(12.9287, 79.9128),
    "East Coast Road" = c(13.0364, 80.2708),
    "Mogappair West Ambattur Industrial Estate" = c(13.075, 80.184),
    "Nandivaram Guduvancheri" = c(12.846, 80.141),
    "Nanmangalam Manikandan Nagar" = c(12.920, 80.190),
    "Purasaiwakkam" = c(13.090, 80.270),
    "Paruthippattu" = c(13.0904, 80.1291),
    "Tiruvanchery" = c(13.0524, 80.1612),
    "Vengaivasal Medavakkam" = c(12.9031, 80.1890),
    "Padapai" = c(12.8667, 80.1)
  )
  
  for(loc in names(manual_coords)) {
    if(loc %in% coords_df$locality) {
      idx <- which(coords_df$locality == loc)
      coords_df$lat[idx] <- manual_coords[[loc]][1]
      coords_df$lon[idx] <- manual_coords[[loc]][2]
    }
  }
  
  return(coords_df)
}

create_price_data <- function() {
  price_df <- data.frame(
    min_price = c(9314, 14123, 10123, 17224, 5349, 9462, 4272, 12899, 11917, 21178, 8260, 11433, 4746, 5865, 7736, 14809, 4791, 4719, 13274, 6388, 8600, 5407, 4939, 8608, 13490, 5014, 17324, 4122, 11494, 5282, 6327, 3899, 5471, 6586, 4172, 11017, 8633, 10592, 5436, 5598, 6161, 5983, 9964, 14108, 4680, 4739, 8453, 4319, 5265, 6581, 5866, 6406, 5072, 4885, 6265, 15059, 5025, 4682, 4545, 4517, 5464, 6991, 9225, 6856, 15637, 6457, 15810, 5829, 4690, 4577, 6138, 13895, 3325, 3619, 4480, 5122, 5468, 5904, 5371, 4873, 6735, 5074, 4268, 7014, 29149, 4079, 4728, 5909, 3897, 12354, 5434, 15357, 14856, 5672, 8889, 14709, 10976, 9068, 5581, 5217, 5831, 5990, 5719, 4304, 4300, 4341, 4478, 14718, 4731, 4660, 24834, 4474, 4915, 5155, 2858, 11573, 6013, 6156, 6277, 7072, 3919, 6377, 9766, 8666, 7093, 4619, 7940, 5294, 6555, 8171, 12266),
    max_price = c(10612, 20797, 13402, 23560, 7691, 13736, 6110, 18757, 17852, 28650, 14281, 15933, 6837, 7919, 10543, 19517, 6783, 6477, 19726, 7729, 13083, 8727, 8174, 13235, 19483, 7252, 25176, 6082, 19670, 9337, 9543, 5694, 7668, 9913, 6803, 18053, 13083, 14456, 7794, 8055, 8802, 8975, 14292, 22150, 7244, 6480, 15252, 6022, 7325, 8803, 8673, 8811, 6383, 6404, 9181, 22216, 7329, 7119, 6372, 6224, 8125, 10546, 13604, 10423, 22224, 7537, 22725, 7013, 7133, 7607, 7526, 22917, 5071, 5693, 5698, 7873, 9339, 9183, 8504, 6836, 9740, 7270, 6703, 10651, 41836, 5168, 8001, 8814, 6782, 18009, 6711, 22385, 21370, 7294, 12359, 19441, 14281, 13026, 6649, 7559, 7784, 8309, 8674, 6335, 6835, 6058, 6153, 21918, 7302, 6506, 40117, 6526, 6910, 7214, 4314, 16942, 7115, 9787, 8220, 9385, 6042, 9472, 14511, 13028, 8613, 5758, 11522, 8488, 8808, 12426, 15570),
    avg_price = c(9963, 17460, 11762, 20392, 6520, 11599, 5191, 15828, 14884, 24914, 11270, 13683, 5792, 6892, 9094, 17163, 5787, 5598, 16500, 7059, 10842, 7067, 6557, 10922, 16487, 6133, 21250, 5102, 15582, 7310, 7935, 4797, 6570, 8250, 5487, 14535, 10858, 12524, 6615, 6826, 7482, 7479, 12128, 18129, 5962, 5609, 11853, 5170, 6295, 7692, 7270, 7608, 5705, 5644, 7723, 18638, 6177, 5900, 5459, 5371, 6794, 8769, 11414, 8639, 18930, 6997, 19267, 6421, 5911, 6092, 6832, 18406, 4198, 4656, 5089, 6497, 7403, 7544, 6937, 5854, 8237, 6172, 5486, 8832, 35492, 4624, 6364, 7362, 5789, 15181, 6072, 18871, 18113, 6483, 10624, 17075, 12629, 11047, 6115, 6388, 6807, 7150, 7196, 5319, 5568, 5199, 5315, 18318, 6016, 5583, 32475, 5500, 5912, 6185, 3586, 14257, 6564, 7972, 7248, 8229, 4980, 7925, 12138, 10847, 7853, 5189, 9731, 6891, 7682, 10298, 13918)
  )
  return(price_df)
}


locality_analytics <- function(locality) {
  serp_url <- "https://serpapi.com/search.json"
  
  params <- list(
    q = paste(locality, "Chennai geoiq"),
    engine = "google",
    api_key = API_KEY
  )
  
  response <- GET(serp_url, query = params)
  
  if(status_code(response) != 200) return(NULL)
  
  data <- fromJSON(content(response, "text", encoding = "UTF-8"), simplifyVector = FALSE)
  organic <- data$organic_results
  
  if(is.null(organic) || length(organic) == 0) return(NULL)
  
  idx <- which(sapply(organic, function(x) grepl("geoiq", x$link)))[1]
  if(is.na(idx)) return(NULL)
  
  url <- organic[[idx]]$link
  page <- read_html(url)
  
  json_text <- page %>%
    html_element("script#__NEXT_DATA__") %>%
    html_text()
  
  json_data <- fromJSON(json_text, simplifyVector = TRUE)
  primary_data <- json_data$props$pageProps$poiPrimaryData
  
  return(list(
    population = as.numeric(primary_data$total_population),
    area_km2 = as.numeric(primary_data$area),
    population_density = as.numeric(primary_data$population_density),
    dist_airport_km = as.numeric(primary_data$dist_airport),
    dist_station_km = as.numeric(primary_data$dist_station)
  ))
}

get_all_locality_analytics <- function(localities) {
  results_list <- lapply(seq_along(localities), function(i) {
    res <- tryCatch({
      locality_analytics(localities[i])
    }, error = function(e) {
      return(NULL)
    })
    if (i < length(localities)) Sys.sleep(NOMINATIM_DELAY_SEC)
    if (is.null(res)) {
      return(list(
        population = NA,
        area_km2 = NA,
        population_density = NA,
        dist_airport_km = NA,
        dist_station_km = NA
      ))
    }
    return(res)
  })
  
  locality_analytics_df <- bind_rows(lapply(seq_along(results_list), function(i) {
    res <- results_list[[i]]
    if (is.null(res)) {
      res <- list(
        population = NA,
        area_km2 = NA,
        population_density = NA,
        dist_airport_km = NA,
        dist_station_km = NA
      )
    }
    tibble(
      locality = localities[i],
      population = as.numeric(res$population),
      area_km2 = as.numeric(res$area_km2),
      population_density = as.numeric(res$population_density),
      dist_airport_km = as.numeric(res$dist_airport_km),
      dist_station_km = as.numeric(res$dist_station_km)
    )
  }))
  
  for (pass in 1:5) {
    na_indices <- which(is.na(locality_analytics_df$population))
    if (length(na_indices) == 0) break
    for (i in seq_along(na_indices)) {
      idx <- na_indices[i]
      loc <- locality_analytics_df$locality[idx]
      res <- tryCatch({
        locality_analytics(loc)
      }, error = function(e) {
        return(NULL)
      })
      if (!is.null(res)) {
        locality_analytics_df[idx, 2:6] <- as.list(unlist(res))
      }
      if (i < length(na_indices)) Sys.sleep(NOMINATIM_DELAY_SEC)
    }
  }
  
  manual_data <- list(
    "Akkarai" = c(8675, 1.36, 6395, 9.98, 6.18),
    "Alwarpet" = c(36202, 0.98, 36753, 7.66, 0.88),
    "Chengalpattu" = c(39599, 28.02, 1413, 30.6, 0.62)
  )
  
  for (loc in names(manual_data)) {
    if (loc %in% locality_analytics_df$locality) {
      idx <- which(locality_analytics_df$locality == loc)
      locality_analytics_df[idx, 2:6] <- as.list(manual_data[[loc]])
    }
  }
  
  na_count <- sum(is.na(locality_analytics_df$population))
  success_count <- nrow(locality_analytics_df) - na_count
  
  cat("\nLocality analytics collected successfully for", success_count, "of", nrow(locality_analytics_df), "localities.\n")
  if (na_count > 0) {
    failed_locs <- locality_analytics_df$locality[is.na(locality_analytics_df$population)]
    cat("Failed for:", paste(failed_locs, collapse = ", "), "\n")
  }
  
  return(locality_analytics_df)
}


safe_count <- function(tbl, name) {
  if(name %in% names(tbl)) return(as.numeric(tbl[[name]])) else return(0)
}

get_amenity_counts <- function(lat, lon, radius_km = 1) {
  radius_m <- radius_km * 1000
  
  amenities <- c(
    "hospital", "clinic", "doctors",
    "pharmacy", "dentist", "school", "kindergarten",
    "college", "university", "bus_station", "parking",
    "fuel", "restaurant", "cafe", "bar", "bank",
    "atm"
  )
  
  query <- paste0(
    "[out:json][timeout:25];(",
    paste0("node[amenity=", amenities, "](around:", radius_m, ",", lat, ",", lon, ");", collapse = ""),
    ");out;"
  )
  
  overpass_url <- "https://overpass-api.de/api/interpreter"
  response <- POST(overpass_url, body = list(data = query), encode = "form")
  
  if(status_code(response) != 200) stop("Overpass API request failed")
  
  data <- fromJSON(content(response, "text", encoding = "UTF-8"), flatten = TRUE)
  elements <- data$elements
  
  counts <- table(elements$tags.amenity)
  result <- setNames(lapply(amenities, function(a) safe_count(counts, a)), amenities)
  
  return(result)
}

get_all_amenities <- function(coords_df, radius_km, delay_sec = 5) {
  
  valid_coords_df <- coords_df %>% filter(!is.na(lat) & !is.na(lon))
  n <- nrow(valid_coords_df)
  
  amenities <- c(
    "hospital", "clinic", "doctors",
    "pharmacy", "dentist", "school", "kindergarten",
    "college", "university", "bus_station", "parking",
    "fuel", "restaurant", "cafe", "bar", "bank",
    "atm"
  )
  col_names <- paste0(amenities, "_", radius_km, "km")
  
  amenities_list <- vector("list", n)
  
  for (i in seq_len(n)) {
    loc <- valid_coords_df$locality[i]
    lat <- valid_coords_df$lat[i]
    lon <- valid_coords_df$lon[i]
    
    result <- tryCatch({
      get_amenity_counts(lat, lon, radius_km)
    }, error = function(e) {
      return(NULL)
    })
    
    amenities_list[[i]] <- result
    if (i < n) Sys.sleep(delay_sec)
  }
  
  amenities_df <- bind_rows(lapply(seq_len(n), function(i) {
    res <- amenities_list[[i]]
    loc <- valid_coords_df$locality[i]
    if (is.null(res) || all(is.na(res))) {
      return(c(locality = loc, setNames(as.list(rep(NA, length(col_names))), col_names)))
    }
    return(c(locality = loc, setNames(res, col_names)))
  })) %>%
    mutate(across(-locality, as.numeric))
  
  for(pass in 1:5) {
    na_rows <- which(rowSums(is.na(amenities_df[,-1])) == ncol(amenities_df) - 1)
    
    if (length(na_rows) == 0) {
      break
    }
    
    for (j in seq_along(na_rows)) {
      idx <- na_rows[j]
      loc <- amenities_df$locality[idx]
      lat <- valid_coords_df$lat[valid_coords_df$locality == loc]
      lon <- valid_coords_df$lon[valid_coords_df$locality == loc]
      
      new_result <- tryCatch({
        get_amenity_counts(lat, lon, radius_km)
      }, error = function(e) {
        return(NULL)
      })
      
      if (!is.null(new_result) && !all(is.na(new_result))) {
        amenities_df[idx, -1] <- as.list(as.numeric(unlist(new_result)))
      }
      
      if (j < length(na_rows)) Sys.sleep(delay_sec)
    }
    
    remaining_na <- sum(rowSums(is.na(amenities_df[,-1])) == ncol(amenities_df) - 1)
  }
  
  na_count <- sum(rowSums(is.na(amenities_df[,-1])) == ncol(amenities_df) - 1)
  success_count <- n - na_count
  
  if(na_count > 0) {
    failed_idx <- which(rowSums(is.na(amenities_df[,-1])) == ncol(amenities_df) - 1)
    failed_locs <- amenities_df$locality[failed_idx]
  }
  
  return(amenities_df)
}

final_df <- read_csv("data/final_df.csv")
final_cols <- names(final_df)

COL_MAP <- list(
  "locality_analytics_df" = c("locality", "population", "area_km2", "population_density", "dist_airport_km", "dist_station_km"),
  "coords_df"             = c("locality", "lat", "lon"),
  "amenities_2km"         = c("locality", "hospital_2km", "clinic_2km", "doctors_2km", "pharmacy_2km", "dentist_2km", "school_2km", 
                              "kindergarten_2km", "college_2km", "university_2km", "bus_station_2km", "parking_2km", 
                              "fuel_2km", "restaurant_2km", "cafe_2km", "bar_2km", "bank_2km", "atm_2km"),
  "amenities_5km"         = c("locality", "hospital_5km", "clinic_5km", "doctors_5km", "pharmacy_5km", "dentist_5km", "school_5km", 
                              "kindergarten_5km", "college_5km", "university_5km", "bus_station_5km", "parking_5km", 
                              "fuel_5km", "restaurant_5km", "cafe_5km", "bar_5km", "bank_5km", "atm_5km")
)

fallback_function <- function(input_df, final_df, columns) {
  if (!is.data.frame(input_df)) {
    if (is.vector(input_df) || is.list(input_df)) {
      input_df <- data.frame(locality = unlist(input_df), stringsAsFactors = FALSE)
      message("Converted input to data frame with column: locality")
    } else {
      stop("input_df must be a data frame, vector, or list.")
    }
  }
  
  df_columns <- names(input_df)
  
  if (!"locality" %in% df_columns) {
    stop("'locality' must be included in input_df")
  }
  
  missing_cols <- setdiff(columns, names(final_df))
  if (length(missing_cols) > 0) {
    stop("The following columns are not in final_df: ", paste(missing_cols, collapse = ", "))
  }
  
  input_df <- input_df %>%
    dplyr::filter(locality %in% final_df$locality)
  
  missing_localities <- final_df %>%
    dplyr::filter(!locality %in% input_df$locality) %>%
    dplyr::select(all_of(columns))
  
  input_df <- dplyr::bind_rows(input_df, missing_localities)
  
  input_df <- input_df %>%
    dplyr::distinct(locality, .keep_all = TRUE) %>%
    dplyr::arrange(locality)
  
  return(input_df)
}

cat("CHENNAI REAL ESTATE DATA COLELCTION PIPELINE\n")

cat("Scraping localities...\n")
localities <- scrape_localities()
localities <- fallback_function(localities, final_df, c("locality"))

cat("Getting prices of localities...\n")
price_df <- create_price_data()

cat("Getting the coordinates of the localities...\n")
coords_df <- fallback_function(coords_df, final_df, c("locality", "lat", "lon"))

cat("Getting locality analytics...\n")
locality_analytics_df <- get_all_locality_analytics(localities$locality)
locality_analytics_df <- fallback_function(locality_analytics_df, final_df,
                                         COL_MAP$locality_analytics_df)

cat("Counting amenities within 2km...\n")
amenities_2km <- get_all_amenities(coords_df, AMENITY_RADIUS_2KM, OVERPASS_DELAY_SEC)
amenities_2km <- fallback_function(amenities_2km, final_df, c("locality",
                                                            COL_MAP$amenities_2km))

cat("Counting amenities within 5km...\n")
amenities_5km <- get_all_amenities(coords_df, AMENITY_RADIUS_5KM, OVERPASS_DELAY_SEC)
amenities_5km <- fallback_function(amenities_5km, final_df, c("locality",
                                                            COL_MAP$amenities_5km))


created_df <- list(coords_df, locality_analytics_df, amenities_2km, amenities_5km) %>%
  reduce(left_join, by = "locality")

created_df <- created_df %>%
  relocate(locality)

loc_index <- match("locality", names(created_df))

final_dff <- bind_cols(
  created_df[ , 1:loc_index, drop = FALSE],
  price_df[ , setdiff(names(price_df), "locality"), drop = FALSE],
  created_df[ , (loc_index + 1):ncol(created_df), drop = FALSE]
)

cat("Getting flooded streets data...\n")
flood_data <- st_read("data/flood_data.geojson", quiet = TRUE)

locality_sf <- st_as_sf(final_dff, coords = c("lon", "lat"), crs = 4326)
flood_utm <- st_transform(flood_data, 32644)
locality_utm <- st_transform(locality_sf, 32644)

dist_matrix <- st_distance(locality_utm, flood_utm)
min_dist <- apply(dist_matrix, 1, min)
final_dff$flooded <- ifelse(min_dist < 500, 1, 0)

write.csv(final_dff, "data/created_df.csv", row.names = FALSE)

cat("\n")
cat("Final localities: ", nrow(final_dff), "\n")
cat("Final columns: ", ncol(final_dff))