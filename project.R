library(tidyverse)
library(ggplot2)
library(grid)
library(ggthemes)
library(lubridate)
library(roxygen2)

#' Reading code below are given. However, a small change was made to speed
#' up the tidy process later on. radius_34 will be 34 only and so on.
#' Create a vector of the width of each column
ext_tracks_widths <- c(7, 10, 2, 2, 3, 5, 5, 6, 4, 5, 4, 4, 5, 3, 4, 3, 3, 3,
                       4, 3, 3, 3, 4, 3, 3, 3, 2, 6, 1)
#' Create a vector of column names, based on the online documentation for this data
ext_tracks_colnames <- c("storm_id", "storm_name", "month", "day",
                         "hour", "year", "latitude", "longitude",
                         "max_wind", "min_pressure", "rad_max_wind",
                         "eye_diameter", "pressure_1", "pressure_2",
                         paste("34", c("ne", "se", "sw", "nw"), sep = "_"),
                         paste("50", c("ne", "se", "sw", "nw"), sep = "_"),
                         paste("64", c("ne", "se", "sw", "nw"), sep = "_"),
                         "storm_type", "distance_to_land", "final")
#' Read the file in from its url
ext_tracks <- read_fwf("./data/ebtrk_atlc_1988_2015.txt",
                       fwf_widths(ext_tracks_widths, ext_tracks_colnames),
                       na = "-99")
saveRDS(ext_tracks, file = "ext_tracks")

#' Tidy Data 
ext_tracks_clean <- ext_tracks %>%
    dplyr::mutate_(year2 = ~year) %>%
    tidyr::unite(date, year, month, day, hour) %>%
    dplyr::mutate_(date = ~lubridate::ymd_h(date)) %>%
    dplyr::select(-1) %>%
    dplyr::unite(storm_id, storm_name, year2, sep = "-") %>%
    dplyr::select(storm_id, date, latitude, longitude, 
           starts_with("34"), starts_with("50"), starts_with("64")) %>%
    dplyr::gather(speed_direction, value, 5:16) %>%
    tidyr::separate(speed_direction, c("speed", "direction"), sep = "_") %>%
    tidyr::spread(direction, value)
    
saveRDS(ext_tracks_clean, "ext_tracks_clean")

#' Get Hurricane Ike which last for 15 days
#' @param df a clean dataframe
#' @param name name of the hurricane
#' @return a dataframe of the chosen hurricane
get_hurricane <- function(df = ext_tracks_clean, name){
    name <- toupper(name)
    index <- grepl(name, df$storm_id)
    out <- df[index,]
}
ike <- get_hurricane(ext_tracks_clean, "ike")
ike$longitude <- -ike$longitude
saveRDS(ike, "ike")

#' Get duration of the storm
#' @param data a dataframe of the chosen hurricane
#' @return a vector of number giving the duration of the hurricane
get_duration <- function(data){
    unique(lubridate::day(data$date))    
}

#' The record includes data at 0, 6, 12, 18 hour. And the hurricance lasts
#' 15 days. The function below extract a single observation 
#' 
#' Get the observation about the hurricane
#' @param data a clean dataset of the chosen hurricane
#' @param day the day of the observation
#' @param hour the hour of the observation
#' @return the final observation as shown on the assignment page
get_obs <- function(data, day, hour){
    out <- data %>%
        dplyr::filter_(~lubridate::day(data$date) == day & 
                       lubridate::hour(data$date) == hour)
}
ike2 <- get_obs(ike, 10, 6)

#' Building the geom hurricane class.
#' @importFrom ggplot2 ggproto
#' @importFrom tidyr gather
#' @importFrom dplyr select mutate bind_rows bind_cols
#' @importFrom geosphere destPoint
#' @importFrom grid polygonGrob
#' @export
GeomHurricane <- ggplot2::ggproto("GeomHurricane", Geom,
                                  required_aes = c("x", "y", "r_ne", "r_se", "r_nw", "r_sw"),
                                  default_aes = aes(colour = "red", fill = "red", size = 1, linetype = 1, alpha = 0.5, scale_radii = 1),
                                  draw_key = ggplot2::draw_key_polygon,
                                  draw_group = function(data, panel_scales, coord) {
                                      
                                      color <- data[1,]$colour
                                      fill <- data[1,]$fill
                                      alpha <- data[1,]$alpha
                                      
                                      # Tidy data into long form 
                                      arc <- data.frame(start_arc = c(0, 90, 180, 270), 
                                                        end_arc = c(90, 180, 270, 360))
                                      data <- data %>%
                                          tidyr::gather("direction", "wind_radius", c("r_ne", "r_se", "r_sw", "r_nw")) %>%
                                          dplyr::mutate(wind_radius = 1852*wind_radius*scale_radii) %>%
                                          dplyr::bind_cols(arc)
                                      
                                      # Creating dataframe of points
                                      df <- data.frame()
                                      for (i in 1:nrow(data)){
                                          temp <- data[i,]
                                          out <- geosphere::destPoint(p = c(temp$x, temp$y),
                                                                      b = temp$start_arc:temp$end_arc,
                                                                      d = temp$wind_radius)
                                          out <- as.data.frame(out)
                                          names(out) <- c("x", "y")
                                          df <- dplyr::bind_rows(df, out)
                                      }
                                      
                                      coords <- coord$transform(df, panel_scales)
                                      
                                      grid::polygonGrob(x = coords$x,
                                                        y = coords$y,
                                                        gp = grid::gpar(col = color, fill = fill, alpha = alpha))
                                  }
)


#' This is the wrapper of the above class
#' 
geom_hurricane <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity",
                            na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, ...){
    ggplot2::layer(data = data, mapping = mapping, stat = stat, geom = GeomHurricane,
                   position = position, show.legend = show.legend, inherit.aes = inherit.aes,
                   params = list(na.rm = na.rm,...))
}
