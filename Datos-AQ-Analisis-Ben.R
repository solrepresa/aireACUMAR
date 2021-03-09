## 09/03/2021
## Autor: Sol Represa
## Analisis para informe convenio sobre nueva normativa encalidad de aire ACUMAR


library(dplyr)
library(ggplot2)
library(lubridate)
library(openair)
library(data.table)
library(openxlsx)

# BENCENO

getwd()

datos <- read.xlsx("C:/Users/solre/Desktop/ACUMAR/Monitoreos Continuos y Automáticos/DOCK SUD/- EMC1 DS (2010-2020)/EMC1 DS Historico Benceno 2012-2023.xlsx")

datos$Fecha.y.hora <- convertToDateTime(datos$Fecha.y.hora)
tabla <- datos[c(3,9)]
tabla$estacion <- "EMC1 DS"
names(tabla)  <- c("Fecha-Hora", "Ben-1h", "Estacion")

rm(datos)


tabla$Anio  <- year(tabla$`Fecha-Hora`)
tabla$Mes <- month(tabla$`Fecha-Hora`)

# Quitar datos de años que no usamos
tabla <- tabla[-which(tabla$Anio == 2010 |tabla$Anio == 2011 |tabla$Anio == 2012 | tabla$Anio == 2013 |  
                        tabla$Anio == 2014 | tabla$Anio == 2021 | tabla$Anio == 2022 | 
                        tabla$Anio == 2023 | tabla$Anio == 2024),]

tabla$`Fecha-Hora` <- tabla$`Fecha-Hora` - 01 #Le resto 1 hora para que coincida con criterio ACUMAR 24hs
tabla$Dia <- day(tabla$`Fecha-Hora`)
tabla$Hora <- hour(tabla$`Fecha-Hora`)

tabla$Mes <- factor(tabla$Mes)
tabla$Anio <- factor(tabla$Anio)
tabla$Dia <- factor(tabla$Dia)
tabla$Hora <-factor(tabla$Hora)

# cuántos datos por debajo del limite de detcción?
value_zero <- tabla[which(tabla$`Ben-1h` == 0), 2]
length(value_zero) * 100 / nrow(tabla)  # 74.7%

tabla[which(tabla$`Ben-1h` == 0), 2] <- 0.01



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# # Serie de tiempo HORARIO #### 

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


ggplot(tabla, aes( x = `Fecha-Hora`, y = `Ben-1h`)) + 
  geom_line() + theme_bw() + 
  labs(x ="", y = expression(Benceno~(ug.m^-3)), 
       title = "DS - horarias") 




# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Analisis Descriptivo - Superacion limite anuales ####

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


tt <- tabla[c(1,2)]
  
# Cantidad de dias con datos disponibles
NA_dia <- aggregate( is.na(tt[2]) , list(format(tt$`Fecha-Hora`, "%Y-%m-%d")), FUN = sum ) 
Count_dia <- aggregate( tt[2] , list(format(tt$`Fecha-Hora`, "%Y-%m-%d")), FUN = NROW )
tabla_na_dia <- cbind(NA_dia[1], round(NA_dia[-1]/Count_dia[-1] * 100, digits = 0))
names(tabla_na_dia) <- c("fecha", "Porc_NA")
  
# aplicar limpieza 
tabla_na_dia[tabla_na_dia$Porc_NA > 25, 2] <- NA    #criterio = más de 25% NA al dia es removido
tabla_na_dia <- tabla_na_dia[complete.cases(tabla_na_dia),]
  
# generar indices 
tabla_na_dia$Mes <- month(tabla_na_dia$fecha)
tabla_na_dia$Anio <- year(tabla_na_dia$fecha)
tabla_na_dia$Dia <- day(tabla_na_dia$fecha)
  
tabla_na_dia$Mes <- factor(tabla_na_dia$Mes)
tabla_na_dia$Anio <- factor(tabla_na_dia$Anio)
tabla_na_dia$Dia <- factor(tabla_na_dia$Dia)
  
# unir ambas tablas
df <- merge(tabla, tabla_na_dia, by = c("Dia", "Mes", "Anio"))
  
df <- df[-c(8,9)]
  


tabla_anual_limp_dia <- df %>% 
  group_by(Estacion, Anio) %>%
  summarise(media = round(mean(`Ben-1h`, na.rm = TRUE), 2),
            n_faltantes = sum(is.na(`Ben-1h`)),
            n = n())

tabla_anual_limp_dia$media[tabla_anual_limp_dia$n < 274] <- NA # recorte en Anios 75% de datos
tabla_anual_limp_dia$media[tabla_anual_limp_dia$n_faltantes > 91] <- NA #condicion de exclusion > datos faltantes







# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# # Serie de tiempo ANUAL #### 

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


ggplot(tabla_anual_limp_dia, aes( x = as.numeric(as.character(Anio)), 
               y = media)) + 
  geom_line() + theme_bw() + 
  labs(title = "EMC1 DS") +
  geom_hline(yintercept=7, col = "brown") + 
  geom_hline(yintercept=6, col = "red") + 
  geom_hline(yintercept=5, col = "orange") + 
  geom_hline(yintercept=4, col = "yellow") + 
  geom_hline(yintercept=3, col = "green") +
  labs(x = "", y = expression(Benceno~(ug.m^-3)))
