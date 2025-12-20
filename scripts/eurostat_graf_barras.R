library(tidyverse)
library(eurostat)
library(ggthemes)

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
                  select(-freq) 


#TOP (Porcentaje del incremento anual del precio del alquiler del páis donde más ha crecido ese año)
df_top <- df_alquileres_anual %>%
           group_by(año) %>%
           slice_max(order_by = values, n = 1) %>%
           ungroup() %>%
           arrange(año)

gráfico_barras_TOP <- ggplot(df_top, aes(x = año, y = values, fill = país)) +
                       geom_col() +
                       labs(title = "Porcentaje del incremento anual del precio\ndel alquiler del páis donde más ha crecido ese año (UE)",
                            x = "Año", y = "Porcentaje del incremento anual",
                            caption = "Eurostat 2025") +
                       theme_solarized() +
                       geom_text(aes(label = país), angle = 90, hjust = 0) +
                       scale_y_continuous(breaks = seq(0, 170, by = 12.5)) +
                       theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
                             legend.position = "none")

gráfico_barras_TOP

##La evolución de los precios muestra una clara transición de inestabilidad extrema a madurez de mercado, protagonizada casi exclusivamente por las economías de Europa del Este y los países bálticos. Se distinguen dos grandes etapas: un primer periodo de ajuste agresivo (1996-2008), marcado por la apertura post-soviética y la inflación previa a la entrada en el euro, que provocó subidas desorbitadas como el pico histórico de Bulgaria en 1998 (>150%) o el de Rumanía en 2007; seguido de una segunda etapa de estabilización estructural (2009-2024) tras la crisis financiera, donde las subidas se han moderado drásticamente (situándose habitualmente por debajo del 25%) al haberse completado la convergencia económica de estos países con el resto de la Unión Europea.

#NONTOP
df_nontop <- df_alquileres_anual %>%
  group_by(año) %>%
  slice_min(order_by = values, n = 1) %>%
  ungroup() %>%
  arrange(año)

gráfico_barras_NONTOP <- ggplot(df_nontop, aes(x = año, y = values, fill = país)) +
                          geom_col() +
                          labs(title = "Porcentaje del incremento anual del precio\ndel alquiler del páis donde menos ha crecido ese año (UE)",
       x = "Año", y = "Porcentaje del incremento anual",
                          caption = "Eurostat 2025") +
                          theme_solarized() +
                          geom_text(aes(label = país), angle = 90, hjust = 0) +
                          scale_y_continuous(breaks = seq(-40, 10, by = 5)) +
                          theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
                          legend.position = "none")

gráfico_barras_NONTOP

##Este gráfico traza la historia de las crisis inmobiliarias europeas a través de los descensos de precios, identificando tres fases muy diferenciadas. Primero, una etapa de corrección violenta post-2008, donde las economías bálticas (Estonia y Lituania) sufrieron desplomes masivos de hasta un 30% al estallar sus burbujas de crédito. Segundo, un largo periodo de deflación estructural en el sur (2012-2018), protagonizado casi en exclusiva por Grecia (EL), que encadenó casi una década de caídas constantes debido a su crisis de deuda y austeridad. Finalmente, el gráfico revela una tendencia actual preocupante tras 2021: la desaparición de las bajadas, ya que en los últimos años (2022-2024) las barras vuelven a ser positivas, indicando que hoy en día los precios suben incluso en los mercados más estancados de Europa.

library(patchwork) #- utiliza el pkg "patchwork" para mostrar los 2 gráficos (p1 y p2) side-by-side
gráfico_barras_TOP + gráfico_barras_NONTOP
