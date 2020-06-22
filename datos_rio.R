rm(list = ls())

library(tidyverse)
library(rvest)
#============================================================================================
#Asuncion
url1 <- 'https://www.meteorologia.gov.py/nivel-rio/vermascalendario.php?estacion=2000086218&fechadesde=01-01-2017&fechahasta=31-12-2018'
h1 <- read_html(url1)
class(h1)
html_text(h1)
nivel_ASU <- h1 %>% html_nodes("table")
nivel_ASU
nivel_ASU[[1]]
nivel_ASU <- nivel_ASU[[1]] %>% html_table
#============================================================================================

#Bahia Negra
url1 <- 'https://www.meteorologia.gov.py/nivel-rio/vermascalendario.php?estacion=2000086033&fechadesde=01-01-2017&fechahasta=31-12-2018'
h1 <- read_html(url1)
class(h1)
html_text(h1)
nivel_BAHIA <- h1 %>% html_nodes("table")
nivel_BAHIA
nivel_BAHIA[[1]]
nivel_BAHIA <- nivel_BAHIA[[1]] %>% html_table
#============================================================================================

#Pilar
url1 <- 'https://www.meteorologia.gov.py/nivel-rio/vermascalendario.php?estacion=2000086255&fechadesde=01-01-2017&fechahasta=31-12-2018'
h1 <- read_html(url1)
class(h1)
html_text(h1)
nivel_PILAR <- h1 %>% html_nodes("table")
nivel_PILAR
nivel_PILAR[[1]]
nivel_PILAR <- nivel_PILAR[[1]] %>% html_table
#============================================================================================
#unir bases  2017 al 2018

nivel <- left_join(nivel_BAHIA, nivel_ASU, by = "Fecha") %>% 
  rename(Nivel.rio_BAHIA = `Nivel del día.x`, Nivel.rio_ASU = `Nivel del día.y`)

nivel <- left_join(nivel, nivel_PILAR, by = "Fecha") %>% 
  rename(Nivel.rio_PILAR = `Nivel del día`)

rm(nivel_BAHIA, nivel_ASU,nivel_PILAR, h1, url1)
#============================================================================================
nivel$Niv_rio.Bhahia <- str_sub(nivel$Nivel.rio_BAHIA, 1,4)
nivel$Niv_rio.Asu <- str_sub(nivel$Nivel.rio_ASU, 1,4)
nivel$Niv_rio.Pilar <- str_sub(nivel$Nivel.rio_PILAR, 1,4)

nivel <- nivel[, -(2:4)]
str(nivel)
#============================================================================================
nivel$Niv_rio.Bhahia <- as.numeric(nivel$Niv_rio.Bhahia)
nivel$Niv_rio.Asu <- as.numeric(nivel$Niv_rio.Asu)
nivel$Niv_rio.Pilar <- as.numeric(nivel$Niv_rio.Pilar)

nivel$Fecha <- dmy(nivel$Fecha)
class(nivel$Fecha)
#============================================================================================
FECHA_INICIO <-  as.Date('2017-01-01')
FECHA_FIN <- as.Date('2018-12-31')
library(lubridate)
Fecha <- seq(FECHA_INICIO, FECHA_FIN, by=1)
x <- as.data.frame(Fecha)
str(x)
#============================================================================================
riopy <- left_join(x, nivel, by = "Fecha")
rm(Fecha,  FECHA_FIN, FECHA_INICIO, nivel, x)
#============================================================================================
#agregar variable epidemiologica
riopy$semana <- c(rep(1:52, each = 7), 52,  rep(53:104, each = 7), 104)

riopy <- riopy %>% arrange(Fecha)

riopy <- riopy[,-1]
#============================================================================================
rios_semanal <- riopy %>% group_by(semana) %>% 
  summarise(RiopyBahia = mean(Niv_rio.Bhahia, na.rm = T),
            RiopyAsu = mean(Niv_rio.Asu, na.rm = T),
            RiopyPilar = mean(Niv_rio.Pilar, na.rm = T))
rios_semanal <- as.data.frame(rios_semanal)
write.csv2(rios_semanal, file = 'riopy_semanal.csv')
#============================================================================================


