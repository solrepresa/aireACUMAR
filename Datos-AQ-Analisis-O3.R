## 03/03/2021
## Autor: Sol Represa
## Analisis para informe convenio sobre nueva normativa encalidad de aire ACUMAR


library(dplyr)
library(ggplot2)
library(lubridate)
library(openair)
library(data.table)


## O3

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Preparacion de datos

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

getwd()
setwd("C:/Users/solre/Desktop/ACUMAR/")

contaminantes <- readRDS("contaminantes.rds")

nombres_contaminantes <- c("CO", "SO2", "NO2",
                           "NOx", "O3", "SH2",
                           "PM")

tabla <- contaminantes[[5]]

names(tabla)
#i = 2


tabla$Estacion <- factor(tabla$Estacion)
levels(tabla$Estacion)

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


# # # SERIE DE TIEMPO - 24hs # # # 
## CON 75 % de los datos >> 18 datos / 6 faltantes
## 75 % de los datos diarios son 18 datos horarios
## es decir,  6 datos faltantes horarios
## entonces, la condición es q el día se ANULE si tiene datos faltantes MAYOR a 6 datos horarios


tabla_dia_max <- tabla %>% 
  group_by(Estacion, Anio, Mes, Dia) %>%
  summarise(max = round(max(`O3-8h`, na.rm = TRUE), 2),
            n_faltantes = sum(is.na(`O3-8h`)))

tabla_dia_max$max[tabla_dia_max$n_faltantes > 6] <- NA #condicion de exclusion
tabla_dia_max$Fecha <- as.Date(paste(tabla_dia$Anio, tabla_dia$Mes, tabla_dia$Dia, sep = "-"))


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# # Serie de tiempo 8hs #### 

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# O3

ggplot(tabla, aes( x = `Fecha-Hora`, y = `O3-8h`)) + 
    geom_line() + theme_bw() + 
    labs(x ="", y = expression(O[3]~(ug.m^-3)), 
         title = paste0("DS EMC1", " - 8hs")) +
    geom_hline(yintercept= 157, col = "red") + 
    geom_hline(yintercept=137, col = "orange") + 
    geom_hline(yintercept=120, col = "yellow") + 
    geom_hline(yintercept=100, col = "green")  # +
  # scale_x_date(date_breaks = "1 year", date_labels = "%Y")


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Analisis Descriptivo - Superacion limite HORARIO ####

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Cuantos horas supera el limite de horario por año y estacion? #

tabla %>% 
  group_by(Estacion) %>%
  .[which( .$`O3-8h` > 137),] %>%  # modificar ACA
  group_by(Estacion, Anio, Mes, Dia) %>% 
  summarise(mediciones_dia = n()) %>%
  group_by(Estacion, Anio) %>% 
  summarise(n = n()) %>%
  ggplot( aes(x = Anio , y = Estacion, fill = n )) + 
  geom_tile() +
  geom_text(aes(label= n)) +
  theme_bw() +  
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 1),
        legend.position = "none") + 
  scale_fill_continuous(low="pink", high ="red4", na.value = "transparent") + 
  labs(x = "", y ="", title= "Número de superacion 8hs - 137 - O3") # modificar ACA


tabla %>% 
  group_by(Estacion) %>%
  .[which( .$`O3-8h` > 120),] %>%  # modificar ACA
  group_by(Estacion, Anio, Mes, Dia) %>% 
  summarise(mediciones_dia = n()) %>%
  group_by(Estacion, Anio) %>% 
  summarise(n = n()) %>%
  ggplot( aes(x = Anio , y = Estacion, fill = n )) + 
  geom_tile() +
  geom_text(aes(label= n)) +
  theme_bw() +  
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 1),
        legend.position = "none") + 
  scale_fill_continuous(low="lightsalmon", high ="lightsalmon4", na.value = "transparent") + 
  labs(x = "", y ="", title= "Número de superacion 8hs - 120 - O3") # modificar ACA


tabla %>% 
  group_by(Estacion) %>%
  .[which( .$`O3-8h` > 157),] %>%  # modificar ACA
  group_by(Estacion, Anio, Mes, Dia) %>% 
  summarise(mediciones_dia = n()) %>%
  group_by(Estacion, Anio) %>% 
  summarise(n = n()) %>%
  ggplot( aes(x = Anio , y = Estacion, fill = n )) + 
  geom_tile() +
  geom_text(aes(label= n)) +
  theme_bw() +  
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 1),
        legend.position = "none") + 
  scale_fill_continuous(low="khaki", high ="khaki3", na.value = "transparent") + 
  labs(x = "", y ="", title= "Número de superacion 8hs - 157 - O3") # modificar ACA



tabla %>% 
  group_by(Estacion) %>%
  .[which( .$`O3-8h` > 100),] %>%  # modificar ACA
  group_by(Estacion, Anio, Mes, Dia) %>% 
  summarise(mediciones_dia = n()) %>%
  group_by(Estacion, Anio) %>% 
  summarise(n = n()) %>%
  ggplot( aes(x = Anio , y = Estacion, fill = n )) + 
  geom_tile() +
  geom_text(aes(label= n)) +
  theme_bw() +  
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 1),
        legend.position = "none") + 
  scale_fill_continuous(low="darkolivegreen2", high ="darkolivegreen4", na.value = "transparent") + 
  labs(x = "", y ="", title= "Número de superacion 8hs - 100 - O3") # modificar ACA


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Analisis Descriptivo - Superacion limite ANUAL ####

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

tabla_anio_max <- tabla_dia_max %>% 
  group_by(Estacion, Anio) %>%
  summarise(p98 = round(quantile(max, probs = 0.98, na.rm = TRUE), 2),
            n_faltantes = sum(is.na(max)), #cuantos NA hay?
            n = n()) # cuantos datos hay?


tabla_anio_max %>% 
  group_by(Estacion) %>%
  .[which( .$p98> 100),] %>%
  group_by(Estacion, Anio) %>% 
  summarise(n = n()) %>%
  ggplot( aes(x = Anio , y = Estacion, fill = n )) + 
  geom_tile() +
  geom_text(aes(label= n)) +
  theme_bw() +  
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 1),
        legend.position = "none") + 
  scale_fill_continuous(low="pink", high ="red4", na.value = "transparent") + 
  labs(x = "", y ="", title= "Número de superacion anual - 100 - O3")

