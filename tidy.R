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
#' 
get_hurricane <- function(df = ext_tracks_clean, name){
    name <- toupper(name)
    index <- grepl(name, df$storm_id)
    out <- df[index,]
}
ike <- get_hurricane(ext_tracks_clean, "ike")
ike$longitude <- -ike$longitude
saveRDS(ike, "ike")

#' Get duration of the storm
#' 
get_duration <- function(data){
    unique(lubridate::day(data$date))    
}

#' The record includes data at 0, 6, 12, 18 hour. And the hurricance lasts
#' 15 days. The function below extract a single observation 
#'
get_obs <- function(data, day, hour){
    out <- data %>%
        dplyr::filter_(~lubridate::day(data$date) == day & 
                       lubridate::hour(data$date) == hour)
}
ike2 <- get_obs(ike, 10, 6)
