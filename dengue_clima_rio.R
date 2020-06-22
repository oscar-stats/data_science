library(readr)
dengue_clima_2017_2018 <- read_csv("dengue_clima_2017_2018.csv")
dengue_clima_2017_2018 <- dengue_clima_2017_2018[, -4]
dengue_clima_2017_2018 <- dengue_clima_2017_2018 %>% rename(semana = X1)


riopy_semanal <- read_delim("riopy_semanal.csv", 
                            ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                            trim_ws = TRUE)
riopy_semanal <- riopy_semanal[,-1]
#============================================================================================
 dengue_clima_rio <- left_join(riopy_semanal, dengue_clima_2017_2018, by = 'semana')

write.csv2(dengue_clima_rio, file = 'dengue_clima_rio.csv', row.names = F, sep = ',')
