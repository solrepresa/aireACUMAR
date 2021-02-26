## 26/02/2021
## Autor: Sol Represa
## Analisis para informe convenio sobre nueva normativa encalidad de aire ACUMAR


library(dplyr)
library(ggplot2)
library(lubridate)
library(openair)
#library(data.table)


## SH2

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Preparacion de datos

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

getwd()
setwd("C:/Users/solre/Desktop/ACUMAR/")

contaminantes <- readRDS("contaminantes.rds")

nombres_contaminantes <- c("CO", "SO2", "NO2",
                           "NOx", "O3", "SH2",
                           "PM")

tabla <- contaminantes[[6]]
rm(contaminantes)

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


tabla_dia <- tabla %>% 
  group_by(Estacion, Anio, Mes, Dia) %>%
  summarise(mean = round(mean(`SH2-1h`, na.rm = TRUE), 2),
            n_faltantes = sum(is.na(`SH2-1h`)))

tabla_dia$mean[tabla_dia$n_faltantes > 6] <- NA #condicion de exclusion
tabla_dia$Fecha <- as.Date(paste(tabla_dia$Anio, tabla_dia$Mes, tabla_dia$Dia, sep = "-"))





a <- tabla_dia[which(tabla_dia$Estacion == "DS EMC1")  ,]
a[which(a$mean == max(a$mean, na.rm = TRUE)),]


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# # Serie de tiempo DIARIO #### 

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# SH2: 24 hs


plotList <- list()

for(j in 1:length(levels(tabla_dia$Estacion))){
  a <- tabla_dia[which(tabla_dia$Estacion == levels(tabla_dia$Estacion)[j]),]
  
  plotList[[j]] <- ggplot(a, aes( x = Fecha, y = mean)) + 
    geom_line() + theme_bw() + 
    labs(x ="", y = expression(SH[2]~(ug.m^-3)), 
         title = paste0(as.character(levels(tabla_dia$Estacion)[j]), " - diarias")) +
    geom_hline(yintercept = 150, col = "red") 
  # scale_x_date(date_breaks = "1 year", date_labels = "%Y")
}


plotList[[1]]
plotList[[2]]
plotList[[3]]
plotList[[4]]
#plotList[[5]]




# Cuantos dias se supera el limite de 24hs por año y estacion? #

tabla_dia %>% 
  group_by(Estacion) %>%
  .[which( .$mean > 150),] %>%   # modificar ACA
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
  labs(x = "", y ="", title= "Número de superacion 24hs - 150 - SH2")  # modificar ACA
