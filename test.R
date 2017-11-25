map_plot <- ggmap::get_map("Lousiana", zoom = 5, maptype = "toner-background") 

map_plot %>%
    ggmap::ggmap(extent = "device") +
    geom_hurricane(data = ike2,
                   ggplot2::aes(x = longitude, y = latitude, 
                                r_ne = ne, r_se = se, r_nw = nw, r_sw = sw,
                                color = wind_speed, fill = wind_speed)) +
    ggplot2::scale_color_manual(name = "Wind speed (kts)", 
                                values = c("red", "orange", "yellow")) + 
    ggplot2::scale_fill_manual(name = "Wind speed (kts)", 
                               values = c("red", "orange", "yellow"))
