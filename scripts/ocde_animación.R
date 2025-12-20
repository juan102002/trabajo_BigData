library(tidyverse)
library(gganimate)
library(ggthemes)

url_ocde <- "https://sdmx.oecd.org/public/rest/data/OECD.SDD.TPS,DSD_PRICES@DF_PRICES_ALL,1.0/CAN+AUS+BEL+FRA+DEU+ITA+JPN+NLD+ESP+CHE+SWE+GBR+USA+OECD+EU27_2020.A.N.CPI.IX.CP041.N.?startPeriod=1996&endPeriod=2024&dimensionAtObservation=AllDimensions&format=csvfilewithlabels"

#Creamos el archivo y descargamos los datos
dir.create("datos")

ruta_ocde <- "./datos/alquileres_ocde.csv"

curl::curl_download(url_ocde, ruta_ocde) 

alquileres_ocde <- read_csv(ruta_ocde)

#Filtramos los datos
alquileres_ocde <- alquileres_ocde %>%
                   select(Measure, BASE_PER, EXPENDITURE, Expenditure,                                         `Reference area`, TIME_PERIOD, OBS_VALUE) %>%
                   mutate(año = TIME_PERIOD,
                          values = OBS_VALUE,
                          país = `Reference area`)
  
#Creamos el gráfico 
gráfico <- ggplot(alquileres_ocde, aes(x = año, y = values, color = país)) +
            geom_line(size = 1.4) +
            labs(title = "Incremento del precio del alquiler (Base 2015) G-12", 
                  x = "Año", y = "Porcentaje (Base 2015)",
                 caption = "Fuente: OCDE 2025") +
            scale_x_continuous(breaks = seq(1996, 2024, by = 2)) +
            scale_y_continuous(breaks = seq(50, 150, by = 12.5),
                               limits = c(50, 150),
                               expand = c(0, 0)) +
            theme_stata() +
            theme(legend.position = "none",
                  axis.text.x = element_text(angle = 90, vjust = 0.5),
                  axis.text.y = element_text(angle = 0)) +
            geom_text(aes(label = país))
  
#Animámos el gráfico
gráfico_animado <- gráfico + transition_reveal(año)+
  enter_fade() + 
  exit_fade()

gráfico_animado  

##El gráfico evidencia una tendencia alcista estructural y sostenida en el precio de los alquileres para las principales economías de la OCDE entre 1996 y 2024. Tomando 2015 como año base (índice 100), se observa cómo la totalidad de los países analizados parten de niveles inferiores (cercanos a los 50-60 puntos) a mediados de los noventa y experimentan un crecimiento constante e ininterrumpido a lo largo de las tres décadas. Aunque la pendiente varía según el país —reflejando distintas presiones inflacionarias locales—, la imagen general confirma un encarecimiento progresivo y universal del acceso a la vivienda en alquiler en el bloque occidental, situándose los valores actuales muy por encima de los registros históricos.
