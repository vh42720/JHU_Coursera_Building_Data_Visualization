library(geosphere)
library(grid)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(roxygen2)

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

geom_hurricane <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity",
                          na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, ...){
    ggplot2::layer(data = data, mapping = mapping, stat = stat, geom = GeomHurricane,
                   position = position, show.legend = show.legend, inherit.aes = inherit.aes,
                   params = list(na.rm = na.rm,...))
}
















