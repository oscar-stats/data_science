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
#write.csv2(rios_semanal, file = 'riopy_semanal.csv')
rm(riopy)
#============================================================================================
#============================================================================================
#Cargo paquete
library(tabulizer)
# Location of WARN notice pdf file
location <- 'C:/Users/59598/OneDrive - Ministerio de Educacion y Ciencias/Proyectos/Dengue/anuario2017.pdf'
# Extract the table
out <- extract_tables(location, output = "data.frame")
#============================================================================================
#Precipitación Diaria Por Mes (mm)
precip2017 <- out[[1]]
precip2017 <- precip2017[-32, ]

x1 <- precip2017 %>%
  gather(key= Mes, value=Precipitacion, `ENE`:`DIC`)
#============================================================================================

#Temperatura Media Diaria Del Aire Por Mes (°C)
tempMedia2017 <- out[[2]]
tempMedia2017 <- tempMedia2017[-32, ]

x2 <- tempMedia2017 %>%
  gather(key= Mes, value=Temp_Aire_med, `ENE`:`DIC`)
#============================================================================================

#Temperatura Máxima Diaria Del Aire Por Mes (°C)
tempMax2017 <- out[[3]]
tempMax2017 <- tempMax2017[-33:-32, ]

x3 <- tempMax2017 %>%
  gather(key= Mes, value=Temp_Aire_max, `ENE`:`DIC`)
#============================================================================================
#Temperatura Mínima Diaria Del Aire Por Mes (°C)
tempMin2017 <- out[[4]]
tempMin2017 <- tempMin2017[-33:-32, ]

x4 <- tempMin2017 %>%
  gather(key= Mes, value=Temp_Aire_min, `ENE`:`DIC`)
#============================================================================================
#Temperatura De Rocío Media Diaria Por Mes (°C)
tempRoc2017 <- out[[5]]
tempRoc2017 <- tempRoc2017[-32, ]

x5 <- tempRoc2017 %>%
  gather(key= Mes, value=Temp_Rocio_med, `ENE`:`DIC`)
#============================================================================================
#Humedad Relativa Media Diaria Por Mes (°C)
HUM_M2017 <- out[[6]]
HUM_M2017 <- HUM_M2017[-32, ]

x6 <- HUM_M2017 %>%
  gather(key= Mes, value=Humedad_med, `ENE`:`DIC`)
#============================================================================================

#Presión atmosférica media diaria Año:2017    Media por Mes (hPa)
presAt_M2017 <- out[[7]]
presAt_M2017 <- presAt_M2017[-32, ]

x7 <- presAt_M2017 %>%
  gather(key= Mes, value=Presion_Atm_med, `ENE`:`DIC`)
#============================================================================================

#Insolación diaria (horas)
inso_2017 <- out[[8]]
inso_2017 <- inso_2017[-32, ]

x8 <- inso_2017 %>%
  gather(key= Mes, value=Insolacion, `ENE`:`DIC`)
#============================================================================================

#Evaporación diaria (mm).
eva_2017 <- out[[9]]
eva_2017 <- eva_2017[-32, ]

x9 <- eva_2017 %>%
  gather(key= Mes, value=Evaporacion, `ENE`:`DIC`)
#============================================================================================
datos_2017 <-left_join(x1, x2, by=c('DIAS', 'Mes'))
datos_2017 <-left_join(datos_2017, x3, by=c('DIAS', 'Mes'))
datos_2017 <-left_join(datos_2017, x4, by=c('DIAS', 'Mes'))
datos_2017 <-left_join(datos_2017, x5, by=c('DIAS', 'Mes'))
datos_2017 <-left_join(datos_2017, x6, by=c('DIAS', 'Mes'))
datos_2017 <-left_join(datos_2017, x7, by=c('DIAS', 'Mes'))
datos_2017 <-left_join(datos_2017, x8, by=c('DIAS', 'Mes'))
datos_2017 <-left_join(datos_2017, x9, by=c('DIAS', 'Mes'))
datos_2017$Anho <- rep('2017', 372)
str(datos_2017)
datos_2017$DIAS <- as.numeric(datos_2017$DIAS)
#============================================================================================
#Eliminar filas que no corresponde como feb 29, 30 , 31 , etc
datos_2017 <- datos_2017[-which(datos_2017$Mes=='FEB' & datos_2017$DIAS > 28), ]
datos_2017 <- datos_2017[-which(datos_2017$Mes=='ABR' & datos_2017$DIAS > 30), ]
datos_2017 <- datos_2017[-which(datos_2017$Mes=='JUN' & datos_2017$DIAS > 30), ]
datos_2017 <- datos_2017[-which(datos_2017$Mes=='SEP' & datos_2017$DIAS > 30), ]
datos_2017 <- datos_2017[-which(datos_2017$Mes=='NOV' & datos_2017$DIAS > 30) , ]
#============================================================================================
dim(datos_2017)
datos_2017 <- datos_2017[ , c(1:2, 12, 3:11) ]#ordenar 

rm("eva_2017" ,     "HUM_M2017" ,    "inso_2017"  ,   "location"  ,    "out"   ,       
   "precip2017" ,   "presAt_M2017",  "tempMax2017"  , "tempMedia2017", "tempMin2017" ,  "tempRoc2017" , 
   "x1" ,           "x2"       ,     "x3"         ,   "x4"         ,   "x5"     ,       "x6"  ,         
   "x7"   ,         "x8"   ,         "x9"  )
#agregar semana epidemiologica
datos_2017$semana <- c(rep(1:52, each = 7), 52)

#eliminar variable 
datos_2017 <- datos_2017[,-c(1:3)]
str(datos_2017)
#write.csv2(datos_2017, file = 'datos_2017.csv')

#rm(list = ls())

#Cargo paquete
library(tabulizer)
library(tidyverse)
#============================================================================================
# Location of WARN notice pdf file
location <- 'C:/Users/59598/OneDrive - Ministerio de Educacion y Ciencias/Proyectos/Dengue/anuario2018.pdf'
# Extract the table
out <- extract_tables(location, output = "data.frame")
#============================================================================================
#Precipitación Diaria Por Mes (mm)
precip2018 <- out[[1]]
precip2018 <- precip2018[-32, ]

x1 <- precip2018 %>%
  gather(key= Mes, value=Precipitacion, `ENE`:`DIC`)
#============================================================================================

#Temperatura Media Diaria Del Aire Por Mes (°C)
tempMedia2018 <- out[[2]]
tempMedia2018 <- tempMedia2018[-32, ]

x2 <- tempMedia2018 %>%
  gather(key= Mes, value=Temp_Aire_med, `ENE`:`DIC`)
#============================================================================================

#Temperatura Máxima Diaria Del Aire Por Mes (°C)
tempMax2018 <- out[[3]]
tempMax2018 <- tempMax2018[-33:-32, ]

x3 <- tempMax2018 %>%
  gather(key= Mes, value=Temp_Aire_max, `ENE`:`DIC`)
#============================================================================================
#Temperatura Mínima Diaria Del Aire Por Mes (°C)
tempMin2018 <- out[[4]]
tempMin2018 <- tempMin2018[-33:-32, ]

x4 <- tempMin2018 %>%
  gather(key= Mes, value=Temp_Aire_min, `ENE`:`DIC`)
#============================================================================================
#Temperatura De Rocío Media Diaria Por Mes (°C)
tempRoc2018 <- out[[5]]
tempRoc2018 <- tempRoc2018[-32, ]

x5 <- tempRoc2018 %>%
  gather(key= Mes, value=Temp_Rocio_med, `ENE`:`DIC`)
#============================================================================================
#Humedad Relativa Media Diaria Por Mes (°C)
HUM_M2018 <- out[[6]]
HUM_M2018 <- HUM_M2018[-32, ]

x6 <- HUM_M2018 %>%
  gather(key= Mes, value=Humedad_med, `ENE`:`DIC`)
#============================================================================================

#Presión atmosférica media diaria Año:2017    Media por Mes (hPa)
presAt_M2018 <- out[[7]]
presAt_M2018 <- presAt_M2018[-32, ]

x7 <- presAt_M2018 %>%
  gather(key= Mes, value=Presion_Atm_med, `ENE`:`DIC`)
#============================================================================================

#Insolación diaria (horas)
inso_2018 <- out[[8]]
inso_2018 <- inso_2018[-32, ]

x8 <- inso_2018 %>%
  gather(key= Mes, value=Insolacion, `ENE`:`DIC`)
#============================================================================================

#Evaporación diaria (mm).
eva_2018 <- out[[9]]
eva_2018 <- eva_2018[-32, ]

x9 <- eva_2018 %>%
  gather(key= Mes, value=Evaporacion, `ENE`:`DIC`)
#============================================================================================
datos_2018 <-left_join(x1, x2, by=c('DIAS', 'Mes'))
datos_2018 <-left_join(datos_2018, x3, by=c('DIAS', 'Mes'))
datos_2018 <-left_join(datos_2018, x4, by=c('DIAS', 'Mes'))
datos_2018 <-left_join(datos_2018, x5, by=c('DIAS', 'Mes'))
datos_2018 <-left_join(datos_2018, x6, by=c('DIAS', 'Mes'))
datos_2018 <-left_join(datos_2018, x7, by=c('DIAS', 'Mes'))
datos_2018 <-left_join(datos_2018, x8, by=c('DIAS', 'Mes'))
datos_2018 <-left_join(datos_2018, x9, by=c('DIAS', 'Mes'))
datos_2018$Anho <- rep('2018', 372)
str(datos_2018)
datos_2018$DIAS <- as.numeric(datos_2018$DIAS)
#============================================================================================
#Eliminar filas que no corresponde como feb 29, 30 , 31 , etc
datos_2018 <- datos_2018[-which(datos_2018$Mes=='FEB' & datos_2018$DIAS > 28), ]
datos_2018 <- datos_2018[-which(datos_2018$Mes=='ABR' & datos_2018$DIAS > 30), ]
datos_2018 <- datos_2018[-which(datos_2018$Mes=='JUN' & datos_2018$DIAS > 30), ]
datos_2018 <- datos_2018[-which(datos_2018$Mes=='SEP' & datos_2018$DIAS > 30), ]
datos_2018 <- datos_2018[-which(datos_2018$Mes=='NOV' & datos_2018$DIAS > 30) , ]
#============================================================================================
dim(datos_2018)
datos_2018 <- datos_2018[ , c(1:2, 12, 3:11) ]#ordenar 

rm("eva_2018" ,     "HUM_M2018" ,    "inso_2018"  ,   "location"  ,    "out"   ,       
   "precip2018" ,   "presAt_M2018",  "tempMax2018"  , "tempMedia2018", "tempMin2018" ,  "tempRoc2018" , 
   "x1" ,           "x2"       ,     "x3"         ,   "x4"         ,   "x5"     ,       "x6"  ,         
   "x7"   ,         "x8"   ,         "x9"  , 'datos_clima_2018')
#agregar semana epidemiologica
datos_2018$semana <- c(rep(1:52, each = 7), 52)

#agrupar por semanas y hallar el promedio semanal
datos_2018 <- datos_2018[,-c(1:3)]
str(datos_2018)
write.csv2(datos_clima_2018, file = 'datos_2018.csv')

#===========================================================================================
#Año 2017
#valores vacios 
datos_2017 %>% map_lgl(.f = function(x){any(!is.na(x) & x == "s/d")})

# La variable Cabin está almacenada como character
datos_2017[datos_2017 == ""] <- NA
datos_2017[datos_2017 == "s/d"] <- NA
datos_2017[datos_2017 == "sd"] <- NA
#===========================================================================================
#decimales 
datos_2017$Temp_Aire_med <- sub(",", ".", datos_2017$Temp_Aire_med, fixed = TRUE)
datos_2017$Temp_Aire_max <- sub(",", ".", datos_2017$Temp_Aire_max, fixed = TRUE)
datos_2017$Temp_Rocio_med <- sub(",", ".", datos_2017$Temp_Rocio_med, fixed = TRUE)
datos_2017$Humedad_med <- sub(",", ".", datos_2017$Humedad_med, fixed = TRUE)
datos_2017$Presion_Atm_med <- sub(",", ".", datos_2017$Presion_Atm_med, fixed = TRUE)
datos_2017$Insolacion <- sub(",", ".", datos_2017$Insolacion, fixed = TRUE)
datos_2017$Evaporacion <- sub(",", ".", datos_2017$Evaporacion, fixed = TRUE)
datos_2017$Precipitacion <- sub(",", ".", datos_2017$Precipitacion, fixed = TRUE)
#===========================================================================================
datos_2017$Temp_Aire_med <- as.numeric(datos_2017$Temp_Aire_med)
datos_2017$Temp_Rocio_med <- as.numeric(datos_2017$Temp_Rocio_med)
datos_2017$Humedad_med <- as.numeric(datos_2017$Humedad_med)
datos_2017$Presion_Atm_med <- as.numeric(datos_2017$Presion_Atm_med)
datos_2017$Insolacion <- as.numeric(datos_2017$Insolacion)
datos_2017$Evaporacion <- as.numeric(datos_2017$Evaporacion)
datos_2017$Precipitacion <- as.numeric(datos_2017$Precipitacion)

#para agrupar por semana
datos_2017$semana <- as.factor(datos_2017$semana)

datos_2017$Insolacion[datos_2017$semana == '9'] <- 10

names(datos_2017)
datos_2017 <- datos_2017 %>% group_by(semana) %>% 
  summarise(Precipitacion = mean(Precipitacion, na.rm = T),
            Temp_Aire_med = mean(Temp_Aire_med, na.rm = T),
            Temp_Aire_max = max(Temp_Aire_max, na.rm = T),
            Temp_Aire_min = min(Temp_Aire_min, na.rm = T),
            Temp_Rocio_med = mean(Temp_Rocio_med, na.rm = T),
            Humedad_med = mean(Humedad_med, na.rm = T),
            Presion_Atm_med = mean(Presion_Atm_med, na.rm = T),
            Insolacion = mean(Insolacion, na.rm = T),
            Evaporacion = mean(Evaporacion, na.rm = TRUE)
  )
datos_2017 <- as.data.frame(datos_2017)    
datos_2017$Anho <- rep('2017', 52)
#============================================================================================
#Año 2018
#valores vacios 
datos_2018 %>% map_lgl(.f = function(x){any(!is.na(x) & x == "s/d")})

# La variable Cabin está almacenada como character
datos_2018[datos_2018 == ""] <- NA
datos_2018[datos_2018 == "s/d"] <- NA
datos_2018[datos_2018 == "sd"] <- NA
#===========================================================================================
#decimales 
datos_2018$Temp_Aire_med <- sub(",", ".", datos_2018$Temp_Aire_med, fixed = TRUE)
datos_2018$Temp_Aire_max <- sub(",", ".", datos_2018$Temp_Aire_max, fixed = TRUE)
datos_2018$Temp_Rocio_med <- sub(",", ".", datos_2018$Temp_Rocio_med, fixed = TRUE)
datos_2018$Humedad_med <- sub(",", ".", datos_2018$Humedad_med, fixed = TRUE)
datos_2018$Presion_Atm_med <- sub(",", ".", datos_2018$Presion_Atm_med, fixed = TRUE)
datos_2018$Insolacion <- sub(",", ".", datos_2018$Insolacion, fixed = TRUE)
datos_2018$Evaporacion <- sub(",", ".", datos_2018$Evaporacion, fixed = TRUE)
datos_2018$Precipitacion <- sub(",", ".", datos_2018$Precipitacion, fixed = TRUE)
#===========================================================================================
datos_2018$Temp_Aire_med <- as.numeric(datos_2018$Temp_Aire_med)
datos_2018$Temp_Rocio_med <- as.numeric(datos_2018$Temp_Rocio_med)
datos_2018$Humedad_med <- as.numeric(datos_2018$Humedad_med)
datos_2018$Presion_Atm_med <- as.numeric(datos_2018$Presion_Atm_med)
datos_2018$Insolacion <- as.numeric(datos_2018$Insolacion)
datos_2018$Evaporacion <- as.numeric(datos_2018$Evaporacion)
datos_2018$Precipitacion <- as.numeric(datos_2018$Precipitacion)

#para agrupar por semana
datos_2018$semana <- as.factor(datos_2018$semana)

names(datos_2018)
datos_2018 <- datos_2018 %>% group_by(semana) %>% 
  summarise(Precipitacion = mean(Precipitacion, na.rm = T),
            Temp_Aire_med = mean(Temp_Aire_med, na.rm = T),
            Temp_Aire_max = max(Temp_Aire_max, na.rm = T),
            Temp_Aire_min = min(Temp_Aire_min, na.rm = T),
            Temp_Rocio_med = mean(Temp_Rocio_med, na.rm = T),
            Humedad_med = mean(Humedad_med, na.rm = T),
            Presion_Atm_med = mean(Presion_Atm_med, na.rm = T),
            Insolacion = mean(Insolacion, na.rm = T),
            Evaporacion = mean(Evaporacion, na.rm = TRUE)
  )
datos_2018 <- as.data.frame(datos_2018)    
datos_2018$Anho <- rep('2018', 52)
#============================================================================================
#uniendo
names(datos_2017)
names(datos_2018)
identical(names(datos_2017), names(datos_2018))

datos_2017_2018 <- rbind(datos_2017, datos_2018)
#write.csv(datos_2017, file = 'dengue_clima.csv')
rm(datos_2017, datos_2018)

#Crear una variable semana 1 al 104
datos_2017_2018$semana <- 1:104
#============================================================================================
#unir nivel del rio y el clima
rio_clima <- left_join(rios_semanal, datos_2017_2018, by = 'semana')
rm(datos_2017_2018, rios_semanal)
#============================================================================================
library(readxl)
dengue <- read_excel("dengue_data.xlsx")
str(dengue)

dengue$`Total_Casos_Dengue(b)` <- NULL

#ordenar por semana epidemiologica
dengue <- dengue %>% arrange(Anho, `Semana_Epidem_(a)`)

dengue$semana <- 1:104

dengue$`Semana_Epidem_(a)` <- NULL
dengue <- as.data.frame(dengue)
str(dengue)
dengue$semana <- as.factor(dengue$semana)
dengue$Anho <- as.factor(dengue$Anho)
#============================================================================================
str(dengue)
str(rio_clima)
rio_clima$Anho <- as.factor(rio_clima$Anho)
rio_clima$semana <- as.factor(rio_clima$semana)
#unir bases
 dengue_rio_clima <- left_join(dengue, rio_clima, by= c('semana', 'Anho'))
rm(dengue, rio_clima)
#============================================================================================


