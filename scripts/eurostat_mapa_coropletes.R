library(tidyverse)  
library(sf)        
library(giscoR)
library(eurostat) 
library(gganimate)

my_table <- "prc_hicp_aind"

eurostat::label_eurostat_tables(my_table)

df <- eurostat::get_eurostat(my_table, time_format = "raw") 

df_alquileres_anual <- df %>%
                        filter(coicop == "CP041",
                               unit == "RCH_A_AVG",
                        !geo %in% c("EU27_2020", "EA19", "EU28", 
                                     "EA", "EA20","EA19", "US", 
                                     "XK", "TR", "RS", "AL", "MK", 
                                     "ME", "UK", "EEA", "EU")) %>%
                         rename(país = geo,
                                año = TIME_PERIOD) %>%
                         select(-freq) %>%
                         mutate(año = as.numeric(año))

mapa_europa <- gisco_get_nuts(year = "2021", 
                              resolution = "20", 
                              nuts_level = 0) %>%
                rename(país = NUTS_ID)

mapa_animado_data <- mapa_europa %>%
                      inner_join(df_alquileres_anual, by = "país") %>%
                      mutate(incremento_alquileres_grupos = factor(ntile(values, 5),
                             labels = c("Muy baja", 
                                        "Baja", 
                                        "Moderado",
                                        "Alta", 
                                        "Muy alta")))

animacion <- ggplot(mapa_animado_data) +
              geom_sf(aes(fill = incremento_alquileres_grupos), 
                      color = "white", 
                      size = 0.1) +
              scale_fill_viridis_d(option = "magma", direction = -1, 
                       name = "Incremento del precio del alquiler (%)") + 
              theme_void() +
              coord_sf(xlim = c(-25, 45), ylim = c(34, 72)) +
              labs(title = "Evolución del Incremento del alquiler entre 1996-2024 en la UE",
                   subtitle = "Año: {current_frame}",
                   caption = "Eurostat 2025") +
              transition_manual(año) +
              ease_aes('linear') 

animate(animacion, renderer = gifski_renderer(), 
        duration = 35, 
        fps = 25, 
        width = 500,
        height = 300)

## La evolución del incremento anual del precio del alquiler en la Unión Europea durante el periodo 1996-2024 muestra una trayectoria de encarecimiento progresivo, caracterizada por una heterogeneidad geográfica marcada y una aceleración reciente. Mientras que en las primeras décadas se observan tasas de variación moderadas y estables en gran parte de Europa Occidental, la animación revela cómo, tras la crisis financiera y especialmente en el periodo post-pandemia (2021-2024), la intensidad de las subidas se dispara. Los países de Europa del Este y los Estados Bálticos (Estonia, Lituania) tienden a ocupar frecuentemente las categorías de crecimiento "Muy alta" (colores más oscuros en tu escala magma) debido a un efecto de convergencia económica y alta inflación reciente, mientras que mercados más regulados como Alemania o Francia suelen mostrar un comportamiento más contenido, aunque con una tendencia alcista generalizada en los últimos fotogramas debido a la presión inflacionaria global.