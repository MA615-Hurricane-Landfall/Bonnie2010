map_counties_j<-function (storm="Bonnie-2010", metric = "distance", wind_var = "vmax_sust", 
                          days_included = c(-2, -1, 0, 1), add_track = TRUE, wind_source = "modeled") 
{
  if (metric == "distance") {
    map_data <- filter_storm_data(storm = storm, output_vars = c("fips", 
                                                                 "storm_dist")) %>% dplyr::rename(value = .data$storm_dist)
  }
  else if (metric == "rainfall") {
    map_data <- filter_storm_data(storm = storm, include_rain = TRUE, 
                                  days_included = days_included, output_vars = c("fips", 
                                                                                 "tot_precip")) %>% dplyr::rename(value = .data$tot_precip)
  }
  else if (metric == "wind") {
    map_data <- filter_wind_data(storm = storm, output_vars = c("fips", 
                                                                wind_var), wind_var = wind_var) %>% `colnames<-`(c("fips", 
                                                                                                                   "value"))
  }
  else {
    stop("`metric` must be either `distance`, `rainfall`, or `wind`")
  }
  map_data <- map_data %>% dplyr::tbl_df()
  out <- hurr_chorop(map_data, metric = metric, wind_var = wind_var, 
                     wind_source = wind_source)
  if (add_track) {
    out <- map_tracks(storm, plot_object = out)
  }
  return(out)
}



hurr_chorop<-function (map_data, metric = "wind", wind_var = "vmax_sust", 
                       wind_source = "modeled") 
{
  if (metric == "rainfall") {
    breaks <- seq(0, 50, by = 10)
    palette_name <- "Blues"
    exposure_legend <- "Rainfall (mm)"
  }
  else if (metric == "distance") {
    breaks <- seq(0, 200, by = 25)
    palette_name <- "Greens"
    exposure_legend <- "Distance (km)"
  }
  else if (metric == "wind") {
    palette_name <- "Reds"
    if (wind_var %in% c("vmax_gust", "vmax_sust")) {
      exposure_legend <- "Wind speed (m/s)"
      if (wind_source == "modeled") {
        breaks <- seq(0,16,3)
        exposure_legend <- "Wind speed (m/s)"
      }
      else if (wind_source == "ext_tracks") {
        if (wind_var == "vmax_sust") {
          breaks <- seq(0,15,3)
        }
        else if (wind_var == "vmax_gust") {
          breaks <- c(0, 26, 38.3, 49)
        }
      }
    }
    else {
      breaks <- seq(0, 60, by = 5)
      exposure_legend <- "Wind duration\n(minutes)"
    }
  }
  if (wind_source == "ext_tracks") {
    exposure_palette <- c("#feb24c", "#fc4e2a", 
                          "#b10026")
  }
  else {
    exposure_palette <- RColorBrewer::brewer.pal(length(breaks) - 
                                                   2, name = palette_name)
  }
  if (max(map_data$value) > max(breaks)) {
    breaks <- c(breaks, max(map_data$value))
  }
  exposure_palette <- c("#f7f7f7", exposure_palette, 
                        "#1a1a1a")
  if (metric == "distance") {
    exposure_palette <- rev(exposure_palette)
  }
  map_data <- map_data %>% dplyr::mutate_(value = ~cut(value, 
                                                       breaks = breaks, include.lowest = TRUE))
  if (metric == "distance") {
    level_names <- levels(map_data$value)
    level_names[length(level_names)] <- ">200"
    map_data$value <- factor(map_data$value, levels = levels(map_data$value), 
                             labels = level_names)
    exposure_palette <- utils::tail(exposure_palette, length(unique(map_data$value)))
  }
  out_data <- get_eastern_map() %>% dplyr::left_join(map_data, 
                                                     by = "fips")
  out <- ggplot2::ggplot() + ggplot2::geom_polygon(data = out_data, 
                                                   ggplot2::aes(x = .data$long, y = .data$lat, group = .data$group, 
                                                                fill = .data$value), color = "lightgray", size = 0.2) + 
    ggplot2::borders("state", regions = c( 
      "north carolina", "south carolina", "georgia", 
      "florida", "alabama",  
      "tennessee", "mississippi", 
      "louisiana", "texas", "oklahoma", 
      "arkansas"
      
    ), 
    colour = "black", fill = NA, size = 0.2, alpha = 0.5) + 
    ggplot2::theme_void() + ggplot2::scale_fill_manual(name = exposure_legend, 
                                                       values = exposure_palette)
  if (!("CoordMap" %in% class(out$coordinates))) {
    out <- out + ggplot2::coord_map()
  }
  return(out)
}


get_eastern_map <- function(map  = "county"){
  
  eastern_states <- c("north carolina", "south carolina", "georgia", 
                      "florida", "alabama",  
                      "tennessee", "mississippi", 
                      "louisiana", "texas", "oklahoma", 
                      "arkansas")
  
  map_data <- ggplot2::map_data(map = map) %>%
    dplyr::filter(.data$region %in% eastern_states)
  
  if(map == "county"){
    county.fips <- maps::county.fips %>%
      dplyr::mutate(polyname = as.character(.data$polyname)) %>%
      dplyr::mutate(polyname = stringr::str_replace(.data$polyname,
                                                    ":.+", ""))
    map_data <- map_data %>%
      tidyr::unite(col = "polyname", .data$region:.data$subregion,
                   sep = ",") %>%
      dplyr::left_join(county.fips, by = "polyname") %>%
      dplyr::mutate(fips = stringr::str_pad(.data$fips, 5,
                                            side = "left", pad = "0"))
  }
  
  return(map_data)
}
