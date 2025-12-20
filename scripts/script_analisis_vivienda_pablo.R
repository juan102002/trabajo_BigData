
#Cargamos paquetes

library("pjpv.curso.R.2022")
library("tidyverse")
library("dplyr")
library("lubridate")
library("ggplot2")
library("gganimate")
library("DT")
library("sf")
library("classInt")
library("stringr")


#Importamos los datos

my_url <- "https://www.ine.es/jaxiT3/files/t/es/px/50914.px?nocab=1"

fs::dir_create("datos")

my_destino <- "./datos/50914.px"

curl::curl_download(my_url, my_destino)

#Obtenemos el DataFrame completo

df <- pxR::read.px(my_destino) %>% as_tibble()

#Filtramos el df y nos quedamos con el subgrupo y el tipo de dato a estudiar. Tambien quitamos los NA de la columna value

names(df)
nombres_actuales <- names(df)
nombres_actuales[4] <- "CCAA"
names(df) <- nombres_actuales


df_VIVIENDA <- df %>% 
  filter(Subgrupos == "041 Alquiler de vivienda", Tipo.de.dato == "Variación anual")  %>%
  filter(!is.na(value)) 
  

#Cambiamos el formato de la fecha con el paquete lubridate

df_VIVIENDA_Fechas <- df_VIVIENDA %>%
    arrange(CCAA, Periodo) %>%
    mutate(Fecha_Base = gsub("M", "-", Periodo),
    Fecha_Completa = paste0(Fecha_Base, "-01"),
    Periodo_Fecha = ymd(Fecha_Completa)) %>%
    select(-Fecha_Base, -Fecha_Completa,-Periodo)

df__VIVIENDA <- df_VIVIENDA_Fechas %>%
  filter(month(Periodo_Fecha) == 1) %>%
  arrange(Periodo_Fecha) %>%
  mutate(Periodo_Fecha = as.Date(Periodo_Fecha, format = "%Y-%m-%d"))

#Obtenemos el data frame final "bonito" con el que trabajar

df_FINAL_VIVIENDA <- df__VIVIENDA %>%
  select(-Subgrupos, -Tipo.de.dato) %>%
  dplyr::rename(var_anual_precio_alquiler = value)

str(df_FINAL_VIVIENDA)

#Dejamos el global limpio...

rm(list = ls()[ls() != "df_FINAL_VIVIENDA"])




#-Cómo ha evolucionado la variación anual del precio del alquiler NACIONAL desde 2002 a 2025 ####
#Creamos un gráfico que muestra como ha evolucionado la variación anual del precio de alquiler desde 2002 a 2025 en territiorio Nacional
#Este es un gráfico introductorio para explicar los datos del dataframe y como vamos a analizar el mercado de la vivienda en España y sus diferentes CCAA

df_vivienda_nacional <- df_FINAL_VIVIENDA %>% filter(CCAA == "Nacional")

p1 <- ggplot(df_vivienda_nacional, aes(x = Periodo_Fecha, y = var_anual_precio_alquiler)) +
  geom_line(aes(color = "Nacional"), linewidth = 0.8, linetype = "dashed") +
  geom_point() +
  scale_color_manual(values = c("Nacional" = "#004488")) +
  labs(title = "Gráfico 1: Evolución de la variación anual del precio del alquiler a nivel NACIONAL",
       subtitle = "Periodo comprendido entre 2002-2025",
       caption = "Fuente INE",
       x = "Periodo (años)",
       y = "Variación anual (%)",
       color = "Región") + 
  scale_x_date(date_breaks = "2 year", date_labels = "%Y") 

p1

#-Qué 5 comunidades han experimentado una mayor variación anual promedio del precio de alquiler en el periodo 2002-2025####
#Aquí analizamos las 5 comunidades que tienen una mayor variación anual media del precio del alquiler
#Comentar que las 5 se encuentran por encima del 2% de variación anual media y lideran Cataluña y Baleares.
#Se muestra en un plot de barras con las coordenas invertidas y con el paquete forcats utilizamos la funcion fct_reorder para ordenar de mayor a menor

df_vivienda_promedios_maxs <- df_FINAL_VIVIENDA %>% 
                         group_by(CCAA) %>%
                         summarise(media_var_anual = mean(var_anual_precio_alquiler))%>%
                         ungroup() %>%
                         slice_max(media_var_anual, n = 5)
                      
p2 <- ggplot(df_vivienda_promedios_maxs, aes(x = forcats::fct_reorder(CCAA, media_var_anual), y = media_var_anual)) +
  geom_col(fill = "lightblue") +
  coord_flip() +
  labs(title = "Gráfico 2: Las 5 comunidades con una mayor variación anual media del precio de alquiler",
       subtitle = "Periodo comprendido entre 2002-2025",
       caption = "Fuente INE",
       x = "Comunidades Autónomas",
       y = "Variación anual media (%)")
p2


#-Vemos la evolución de las 5 comunidades que han experimentado una mayor variación anual media del precio de alquiler durante los periodos 2002-2025####
#Primero hacemos el gráfico de lineas y luego con el paquete gganimate lo hacemos animado
#Se puede ver una disminución de la variación anual en las 5 CCAA en el año 2010 debido a la crisis inmobiliaria.


df_vivienda_maxs <- df_FINAL_VIVIENDA %>% 
                    filter(CCAA %in% 
                    c("04 Balears, Illes","09 Cataluña","02 Aragón","13 Madrid, Comunidad de","16 País Vasco"))

p4 <-  ggplot(df_vivienda_maxs, aes(x = Periodo_Fecha, y = var_anual_precio_alquiler, color = CCAA)) +
       geom_line() +
       labs(title = "Gráfico 3: Evolución de la variación anual del precio del alquiler de las 5 comunidades",
        subtitle = "Periodo comprendido entre 2002-2025",
        caption = "Fuente INE",
        x = "Periodo (años)",
        y = "Variación anual (%)",
        color = "Comunidades Autónomas") 
 
p4

grafico_ani_top5 <- p4 +
  transition_reveal(Periodo_Fecha) +  
  enter_fade() +
  exit_fade()

grafico_ani_top5

#-Qué 8 comunidades han experimentado una menor variación anual promedio del precio de alquiler en el periodo 2002-2025#####
#En este hacemos lo mismo pero con las 8 comunidades con menor variación anual media
#Aquí ningún valor supera el 1.75% de variación media y con el condicional ifelse destacamos que la comunidad que menos ha variado es Navarra
#También utilizamos la función fct_reorder para ordenar de mayor a menor y quitamos la leyenda 


df_vivienda_promedios_mins <- df_FINAL_VIVIENDA %>% 
  group_by(CCAA) %>%
  summarise(media_var_anual = mean(var_anual_precio_alquiler))%>%
  ungroup() %>%
  slice_min(media_var_anual, n = 8)

p3 <- ggplot(df_vivienda_promedios_mins, 
      aes(x = forcats::fct_reorder(CCAA, media_var_anual), y = media_var_anual, 
      fill = ifelse (CCAA == "15 Navarra, Comunidad Foral de", "darkred", "lightblue"))) +
      geom_col() +
      coord_flip() +
      labs(title = "Gráfico 4: Las 5 comunidades que han experimentado una menor variación anual media del precio de alquiler",
       subtitle = "Periodo comprendido entre 2002-2025",
       caption = "Fuente INE",
       x = "Comunidades Autónomas",
       y = "Variación anual media (%)") +
      theme(legend.position = "none")
p3


#-Mostramos los datos en una tabla con el paquete DT ####
#Utilizamos las opciones para dejarla mas limpia y la función formatRound para limitar la columna media_var_anual a 2 decimales

DT::datatable(df_vivienda_promedios_mins,
  options = list(dom = "t", columnDefs = list(list(targets = "_all", className = "dt-center"))),
  caption = "Tabla 1: Las 8 comunidades han experimentado una menor variacion anual promedio del precio de alquiler") %>% 
  formatRound('media_var_anual', 2) 

#-Mapa de coropletas de la variacion anual promedio del precio de alquiler del periodo 2002-2025####

#Obtenemos el mapa del paquete del curso 

df_mapa <- pjpv.curso.R.2022::LAU2_prov_2020_canarias

#Primero agregamos provincias para obtener geometrías para las CCAA (script)

ccaa <- df_mapa %>% 
  group_by(ine_ccaa, ine_ccaa.n, ine_ccaa.n.pjp) %>% 
  summarise() %>% 
  ungroup()

plot(ccaa, max.plot = 1)

str(ccaa)

#Modificamos nuestro dataframe creando la media de todas las CCAAs. También creamos la columna común

df_vivienda_promedios <- df_FINAL_VIVIENDA %>% 
  group_by(CCAA) %>%
  summarise(media_var_anual = mean(var_anual_precio_alquiler))%>%
  filter(CCAA != "Nacional") %>%
  ungroup()%>%
  mutate( ine_ccaa = str_pad( string = row_number(), 
    width = 2,            
    pad = "0",           
    side = "left"), 
    .before = CCAA)

str(df_vivienda_promedios)


#Con la función left_join creamos nuestro dataframe preparado para hacer el plot de coropletas

df_mapa <- left_join(ccaa, df_vivienda_promedios, by = "ine_ccaa") %>%
           select(ine_ccaa,ine_ccaa.n,geometry,media_var_anual)

#Con print podemos ver la estructura del df en la consola

print(df_mapa)

#Creamos el dataframe con los cuartiles 

df_mapa_ntiles <- df_mapa %>%
   mutate(CLASE_COROPLETAS_NTILE = ntile(media_var_anual, 4)) %>%
   mutate(
    CLASE_COROPLETAS_NTILE = case_when(
      CLASE_COROPLETAS_NTILE == 1 ~ "Cuartil 1 - Variación Baja",
      CLASE_COROPLETAS_NTILE == 2 ~ "Cuartil 2 - Variación Media-Baja",
      CLASE_COROPLETAS_NTILE == 3 ~ "Cuartil 3 - Variación Media-Alta",
      CLASE_COROPLETAS_NTILE == 4 ~ "Cuartil 4 - Variación Alta"))
 
#Convertimos la nueva columna a factor (Variable categórica)

df_mapa_ntiles$CLASE_COROPLETAS_NTILE <- as.factor(df_mapa_ntiles$CLASE_COROPLETAS_NTILE)

#Mapeamos

ggplot(data = df_mapa_ntiles) +
  geom_sf(aes(fill = CLASE_COROPLETAS_NTILE),
          color = "Black",
          linewidth = 0.3) +
  scale_fill_brewer(palette = "Blues",
          name = "Variación Anual Media\n(Clasificación por Cuartiles)") +
  labs(title = "Mapa 1: Variación Anual Media del precio del alquiler por CCAA",
          subtitle = "Promedio obtenido de 2002-2025 (Expresado en variación porcentual)",
          caption = "Fuente: Instituto Nacional de Estadística") +
  theme_void()
