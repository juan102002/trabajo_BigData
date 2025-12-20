library(tidyverse)
library(ggthemes)

url_ocde <- "https://sdmx.oecd.org/public/rest/data/OECD.SDD.TPS,DSD_PRICES@DF_PRICES_ALL,1.0/CAN+AUS+BEL+FRA+DEU+ITA+JPN+NLD+ESP+CHE+SWE+GBR+USA+OECD+EU27_2020.A.N.CPI.PA.CP041.N.?startPeriod=1996&endPeriod=2024&dimensionAtObservation=AllDimensions&format=csvfilewithlabels"

#Creamos el archivo y descargamos los datos
dir.create("datos")

ruta_ocde <- "./datos/alquileres_ocde_anual.csv"

curl::curl_download(url_ocde, ruta_ocde) 

alquileres_ocde <- read_csv(ruta_ocde)

#Filtramos los datos

alquileres_ocde_anual <- alquileres_ocde %>%
                          select(Measure, BASE_PER, EXPENDITURE, Expenditure,                                         `Reference area`, TIME_PERIOD, OBS_VALUE) %>%
                          mutate(año = TIME_PERIOD,
                                 values = OBS_VALUE,
                                 país = `Reference area`) 
#Creamos el gráfico
gráfico <- ggplot(alquileres_ocde_anual, aes(x = año,  y = values)) +
      geom_boxplot(aes(group = año, fill = as.factor(año)), outlier.shape = 8) + 
      geom_jitter(width = 0.15, alpha = 1/4, color = "brown") +
      labs(title = "Diagrama de cajas del inncremento anual del precio del alquiler G-12", 
           x = "Año", y = "Porcentaje",
           caption = "FUente: OCDE 2025") +
      theme_solarized() + 
      stat_summary(fun = mean, geom = "line", color = "black", size = 1.2) +               scale_x_continuous(breaks = seq(1996, 2024, by = 1)) +
      theme(axis.text.x = element_text(angle = 90),
            legend.position = "none")

gráfico 

##Este diagrama de cajas ilustra la dinámica de la inflación de los alquileres (el ritmo al que suben los precios anualmente), revelando un comportamiento cíclico dramático. Tras dos décadas de relativa estabilidad y una fase de enfriamiento notable entre 2010 y 2020 —donde las subidas anuales se moderaron hasta caer cerca del 1% de mediana—, se observa una ruptura estructural a partir de 2021. En los últimos tres años (2022-2024), la tendencia se invierte bruscamente con un ascenso vertical, llevando la mediana de incremento anual por encima del 4%, niveles inéditos en la serie histórica. Además, el alargamiento vertical de las cajas en este último periodo indica una mayor volatilidad y disparidad: mientras antes los países se comportaban de forma homogénea, ahora las diferencias en la intensidad de las subidas entre unos mercados y otros son extremas.