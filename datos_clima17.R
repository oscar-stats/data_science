
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
#agregar variable epidemiologica
datos_2017$semana <- c(rep(1:52, each = 7), 52)

#agrupar por semanas y hallar el promedio semanal
datos_2017 <- datos_2017[,-c(1:3)]
str(datos_2017)
write.csv2(datos_2017, file = 'datos_2017.csv')
