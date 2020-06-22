library(readxl)
library(tidyverse)
datos_2017 <- read_excel("datos_2017.xlsx", 
                         col_types = c("numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric"))
datos_2018 <- read_excel("datos_2018.xlsx", 
                         col_types = c("numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric"))

#===========================================================================================
clima <- bind_rows(datos_2017, datos_2018)
str(clima)
FECHA_INICIO <-  as.Date('2017-01-01')
FECHA_FIN <- as.Date('2018-12-31')
library(lubridate)
clima$Fecha <- seq(FECHA_INICIO, FECHA_FIN, by=1)
#===========================================================================================
rio_clima <- left_join(riopy, clima, by = "Fecha")

write.csv2(rio_clima, file = 'rio_clima.csv')
#===========================================================================================
