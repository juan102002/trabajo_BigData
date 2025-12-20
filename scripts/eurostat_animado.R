library(eurostat)
library(tidyverse)
library(ggplot2)
library(gganimate)
library(ggthemes)
library(gt)
library(gtExtras)
library(gganimate)
library(gifski)

#Escogemos los datos de la página de Eurostat
my_table <- "prc_hicp_aind"

eurostat::label_eurostat_tables(my_table)

#Creamos el dataframe asociado al índice de IPC de Eurostat y filtramos
df <- eurostat::get_eurostat(my_table, time_format = "raw")   

df_alquileres_index <- df %>%
                        filter(coicop == "CP041",
                        !geo %in% c("EU27_2020", "EA19", "EU28", 
                                    "EA", "EA20","EA19", "US", 
                                    "XK", "TR", "RS", "AL", "MK", 
                                    "ME", "UK", "EEA", "EU")) %>%
                        label_eurostat() %>%
                        filter(unit == "Annual average index") %>%
                        rename(paises = geo,
                               año = TIME_PERIOD) %>%
                        mutate(año = as.numeric(año)) %>%
                        select(-freq) 

##Top (Los 6 que más han crecido desde 2015 hasta 2024)
df_alquileres_top_6_2024 <- df_alquileres_index %>%
                             filter(año == 2024) %>%
                             slice_max(values, n=6) 

tabla_df_alquileres_top_6_2024 <- df_alquileres_top_6_2024 %>%
                                   gt() %>% 
                                   gt_theme_excel() %>% 
                                   tab_header(title = "Incremento del precio del alquiler en los 6 países que más ha crecido",
                                               subtitle = "TOP 6") %>% 
                                   tab_footnote(footnote = "Eurostat 2025")

tabla_df_alquileres_top_6_2024

top_paises <- df_alquileres_top_6_2024$paises

df_top <- df_alquileres_index %>%
           filter(paises %in% top_paises)

#Gráfico del top
grafico_top <- ggplot(df_top, 
                      aes(x = año, y = values, color = paises)) +
                geom_line(size = 1.4) +
                labs(title = "Incremento del precio del alquiler (Base 2015) del TOP 6 Países", 
                    x = "Año", y = "Porcentaje (Base 2015)",
                    caption = "Fuente: Eurostat 2025") +
                scale_x_continuous(breaks = seq(1996, 2024, by = 2)) +
                scale_y_continuous(breaks = seq(0, 200, by = 25)) +
                theme_stata() +
                theme(legend.position = "none", 
                      axis.text.y = element_text(angle = 0),
                      axis.text.x = element_text(angle = 90, vjust = 0.5)) +
                geom_text(aes(label = paises))

grafico_top_animado <- grafico_top + transition_reveal(año) 

animate(grafico_top_animado, 
        renderer = gifski_renderer())

grafico_top_animado

##En los países de Europa del Este han experimentado un crecimiento económico rápido que ha elevado los salarios, pero la oferta de vivienda no ha seguido el ritmo. Además, han tenido las tasas de inflación general más altas de la UE tras la crisis energética (súbida del precio desde 2021). También está la presencia de Irlanda por la escasez crónica de oferta: la alta demanda (impulsada por el sector tecnológico y la inmigración) choca con un mercado inmobiliario que no construye lo suficiente, provocando una de las crisis de alquiler más agudas de Europa occidental. Islandia es un caso particular por el Efecto Airbnb (Turismo masivo): Gran parte del parque de viviendas se ha destinado al alquiler turístico, reduciendo drásticamente la oferta para residentes y disparando los precios, sumado a su muy limitado terreno edificable y su alta inflación.

#Nontop (Los 6 que menos han crecido desde 2015 hasta 2024)
df_alquileres_nontop_6_2024 <- df_alquileres_index %>%
                                filter(año == 2024) %>%
                                slice_min(values, n=6)

tabla_df_alquileres_nontop_6_2024 <- df_alquileres_nontop_6_2024 %>%
                                gt() %>% 
                                gt_theme_excel() %>% 
                                tab_header(title = "Incremento del precio del alquiler en los 6 países que menos ha crecido",
                                           subtitle = "NONTOP 6") %>% 
                                tab_footnote(footnote = "Eurostat 2025")

tabla_df_alquileres_nontop_6_2024

nontop_paises <- df_alquileres_nontop_6_2024$paises

df_nontop <- df_alquileres_index %>%
             filter(unit == "Annual average index",
                    paises %in% nontop_paises)

#Gráfico del nontop
grafico_nontop <- ggplot(df_nontop, 
                         aes(x = año, y = values, color = paises)) +
                   geom_line(size = 1.4) +
                   labs(title = "Incremento del precio del alquiler (Base 2015) en los 6 Países que menos ha crecido", 
                        x = "Año", y = "Porcentaje (Base 2015)",
                   caption = "Fuente: Eurostat 2025") +
                   scale_x_continuous(breaks = seq(1996, 2024, by = 2)) +
                   scale_y_continuous(breaks = seq(0, 200, by = 25)) +
                   theme_stata() +
                   theme(legend.position = "none", 
                         axis.text.y = element_text(angle = 0),
                         axis.text.x = element_text(angle = 90, vjust = 0.5)) +
                   geom_text(aes(label = paises))

grafico_nontop_animado <- grafico_nontop + transition_reveal(año) 

animate(grafico_nontop_animado, 
        renderer = gifski_renderer())

grafico_nontop_animado

##El grupo de los países con menor incremento en el precio, se puede observar un escenario de estancamiento y corrección post-crisis que contrasta radicalmente con el dinamismo del este de Europa. En este conjunto destaca Grecia como una anomalía bajista, donde los alquileres son nominalmente más bajos que en la década anterior debido al colapso de la demanda interna tras su crisis de deuda, mientras que economías mediterráneas como Italia y Chipre muestran una trayectoria casi plana fruto de un crecimiento económico débil. Por otro lado, la presencia de países con mercados tensos como España o Francia en esta parte baja del ranking responde en gran medida a la metodología del índice HICP, que al medir el stock completo de contratos (incluyendo rentas antiguas y renovaciones limitadas por ley) y no solo los nuevos alquileres de mercado, suaviza la curva de precios, reflejando así una Europa donde la regulación y la inercia contractual han blindado estadísticamente a los inquilinos estables frente a la volatilidad inflacionaria reciente.
