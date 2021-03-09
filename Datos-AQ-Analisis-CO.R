## 03/03/2021
## Autor: Sol Represa
## Analisis para informe convenio sobre nueva normativa encalidad de aire ACUMAR


library(dplyr)
library(ggplot2)
library(lubridate)
library(openair)
library(data.table)


## CO

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Preparacion de datos

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

getwd()
setwd("C:/Users/solre/Desktop/ACUMAR/")

contaminantes <- readRDS("contaminantes.rds")

nombres_contaminantes <- c("CO", "SO2", "NO2",
                           "NOx", "O3", "SH2",
                           "PM")

tabla <- contaminantes[[1]]

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


## Modificamos la unidad
tabla$`CO-1h` <- tabla$`CO-1h` * 1000
tabla$`CO-8h` <- tabla$`CO-8h` * 1000


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# # Serie de tiempo HORARIO #### 

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# CO: hs = 40000, 30000


plotList <- list()

for(j in 1:length(levels(tabla$Estacion))){
  a <- tabla[which(tabla$Estacion == levels(tabla$Estacion)[j]),]
  
  plotList[[j]] <- ggplot(a, aes( x = `Fecha-Hora`, y = `CO-1h`)) + 
    geom_line() + theme_bw() + 
    labs(x ="", y = expression(CO~(ug.m^-3)), 
         title = paste0(as.character(levels(tabla$Estacion)[j]), " - horarias")) +
    geom_hline(yintercept=40000, col = "red") + 
    geom_hline(yintercept=30000, col = "green")  # +
  # scale_x_date(date_breaks = "1 year", date_labels = "%Y")
}

plotList[[1]]
plotList[[2]]
plotList[[3]]
plotList[[4]]


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# # Serie de tiempo >>> 8hs #### 

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# CO: 8hs = 10000


plotList <- list()

for(j in 1:length(levels(tabla$Estacion))){
  a <- tabla[which(tabla$Estacion == levels(tabla$Estacion)[j]),]
  
  plotList[[j]] <- ggplot(a, aes( x = `Fecha-Hora`, y = `CO-8h`)) + 
    geom_line() + theme_bw() + 
    labs(x ="", y = expression(CO~(ug.m^-3)), 
         title = paste0(as.character(levels(tabla$Estacion)[j]), " - 8hs")) +
    geom_hline(yintercept=10000, col = "red")  
   # geom_hline(yintercept=30000, col = "green")  # +
  # scale_x_date(date_breaks = "1 year", date_labels = "%Y")
}

plotList[[1]]
plotList[[2]]
plotList[[3]]
plotList[[4]]
