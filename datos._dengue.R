
#============================================================================================
library(readxl)
#Datos clima
datos2017 <- read_excel("datos2017.xlsx")

#valores vacios 
datos2017 %>% map_lgl(.f = function(x){any(!is.na(x) & x == "s/d")})

# La variable Cabin está almacenada como character
datos2017[datos2017 == ""] <- NA
datos2017[datos2017 == "s/d"] <- NA
datos2017[datos2017 == "sd"] <- NA

str(datos2017)

datos2017$Temp_Aire_med <- as.numeric(datos2017$Temp_Aire_med)
datos2017$Temp_Rocio_med <- as.numeric(datos2017$Temp_Rocio_med)
datos2017$Humedad_med <- as.numeric(datos2017$Humedad_med)
datos2017$Presion_Atm_med <- as.numeric(datos2017$Presion_Atm_med)
datos2017$Insolacion <- as.numeric(datos2017$Insolacion)
datos2017$Evaporacion <- as.numeric(datos2017$Evaporacion)
#para agrupar por semana
datos2017$semana <- as.factor(datos2017$semana)

datos2017$Insolacion[datos2017$semana == '9'] <- 10

names(datos2017)
datos <- datos2017 %>% group_by(semana) %>% 
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
datos <- as.data.frame(datos)    
datos$Anho <- rep('2017', 52)
rm(datos2017)
#============================================================================================
#Datos clima
datos2018 <- read_excel("datos2018.xlsx")

#valores vacios 
datos2018 %>% map_lgl(.f = function(x){any(!is.na(x) & x == "s/d")})

# La variable Cabin está almacenada como character
datos2018[datos2018 == ""] <- NA
datos2018[datos2018 == "s/d"] <- NA
datos2018[datos2018 == "sd"] <- NA

str(datos2018)

datos2018$Temp_Aire_med <- as.numeric(datos2018$Temp_Aire_med)
datos2018$Temp_Aire_max <- as.numeric(datos2018$Temp_Aire_max)
datos2018$Temp_Aire_min <- as.numeric(datos2018$Temp_Aire_min)
datos2018$Temp_Rocio_med <- as.numeric(datos2018$Temp_Rocio_med)
datos2018$Humedad_med <- as.numeric(datos2018$Humedad_med)
datos2018$Presion_Atm_med <- as.numeric(datos2018$Presion_Atm_med)
datos2018$Insolacion <- as.numeric(datos2018$Insolacion)
datos2018$Evaporacion <- as.numeric(datos2018$Evaporacion)
#para agrupar por semana
datos2018$semana <- as.factor(datos2018$semana)


names(datos2018)

datos01 <- datos2018 %>% group_by(semana) %>% 
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
datos01 <- as.data.frame(datos01)    
datos01$Anho <- rep('2018', 52)
rm(datos2018)
#============================================================================================
datos_1718 <- rbind.data.frame(datos, datos01)
datos_1718$Anho <- as.factor(datos_1718$Anho)
rm(datos, datos01)
#============================================================================================
library(readxl)
dengue <- read_excel("dengue_data.xlsx")
str(dengue)

#2017
dengue <- dengue[which(dengue$Anho %in% c(2017, 2018)), ]

dengue <- dengue %>% arrange(Anho, `Semana_Epidem_(a)`)

dengue <- dengue %>% rename(semana = `Semana_Epidem_(a)`)
dengue <- as.data.frame(dengue)
str(dengue)
dengue$semana <- as.factor(dengue$semana)
dengue$Anho <- as.factor(dengue$Anho)
#============================================================================================
#uniendo

datos_2017_2018 <- left_join(dengue, datos_1718, by = c('semana', 'Anho'))
#write.csv(datos_2017, file = 'dengue_clima.csv')
rm(datos_1718, dengue)
#write.csv(datos_2017_2018, file = 'dengue_clima_2017_2018.csv')
#============================================================================================




