## 26/02/2021
## Autor: Sol Represa
## Analisis para informe convenio sobre nueva normativa encalidad de aire ACUMAR


library(dplyr)
library(ggplot2)
library(lubridate)
library(openair)


library(data.table)

## PM2.5

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Preparacion de datos

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

getwd() # ver directorio de trabajo
setwd("C:/Users/solre/Desktop/ACUMAR/") #setear directorio de trabajo

contaminantes <- readRDS("contaminantes.rds")  #archivo con todos los datos

nombres_contaminantes <- c("CO", "SO2", "NO",
                           "NOx", "O3", "SH2",
                           "PM")

tabla <- contaminantes[[7]]

names(tabla) #nombres de las columnas
#i = 2

## Acomodo formato de las variables
tabla$Estacion <- factor(tabla$Estacion)
levels(tabla$Estacion)

tabla$`Fecha-Hora` <- tabla$`Fecha-Hora` - 1 #Le resto 1 hora para que coincida con criterio ACUMAR 24hs

tabla$Anio  <- year(tabla$`Fecha-Hora`)
tabla$Mes <- month(tabla$`Fecha-Hora`)
tabla$Dia <- day(tabla$`Fecha-Hora`)

# Quitar datos de años que no usamos
tabla <- tabla[-which(tabla$Anio == 2021 | tabla$Anio == 2022 | tabla$Anio == 2023 | tabla$Anio == 2024),]

tabla$Mes <- factor(tabla$Mes)
tabla$Anio <- factor(tabla$Anio)
tabla$Dia <- factor(tabla$Dia)



# # # SERIE DE TIEMPO - 24hs # # #
## CON 75 % de los datos >> 18 datos / 6 faltantes
## 75 % de los datos diarios son 18 datos horarios
## es decir,  6 datos faltantes horarios
## entonces, la condición es q el día se ANULE si tiene datos faltantes MAYOR a 6 datos horarios


tabla_dia <- tabla %>% 
  group_by(Estacion, Anio, Mes, Dia) %>%
  summarise(mean = round(mean(`PM25-1h`, na.rm = TRUE), 2),
            n_faltantes = sum(is.na(`PM25-1h`)))

tabla_dia$mean[tabla_dia$n_faltantes > 6] <- NA #condicion de exclusion
tabla_dia$Fecha <- as.Date(paste(tabla_dia$Anio, tabla_dia$Mes, tabla_dia$Dia, sep = "-"))


# # # SERIE DE TIEMPO ANUAL # # # 
## 75 % de los datos anuales son 282 datos diarios (6768 datos horarios)
## es decir,  93 datos faltantes diarios (o 2232 horarios)
## entonces, la condición es q el año se anule si tiene datos faltantes MAYOR a 2232 datos horarios

tabla_anio <- tabla %>% 
  group_by(Estacion, Anio) %>%
  summarise(mean = round(mean(`PM25-1h`, na.rm = TRUE), 2),
            n_faltantes = sum(is.na(`PM25-1h`)),
            n = n())

tabla_anio$mean[tabla_anio$n < 6768] <- NA #condicion de exclusion > dias del año
tabla_anio$mean[tabla_anio$n_faltantes > 2232] <- NA #condicion de exclusion > datos faltantes




# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# # Serie de tiempo DIARIO #### 

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# PM10: 24 hs = 150, 100, 75, 50


plotList <- list()

for(j in 1:length(levels(tabla_dia$Estacion))){
  a <- tabla_dia[which(tabla_dia$Estacion == levels(tabla_dia$Estacion)[j]),]
  
  plotList[[j]] <- ggplot(a, aes( x = Fecha, y = mean)) + 
    geom_line() + theme_bw() + 
    labs(x ="", y = expression(MP[2.5]~(ug.m^-3)), 
         title = paste0(as.character(levels(tabla_dia$Estacion)[j]), " - 24hs")) +
    geom_hline(yintercept=65, col = "red") + 
    geom_hline(yintercept=50, col = "orange") + 
    geom_hline(yintercept=37.5, col = "yellow") + 
    geom_hline(yintercept=25, col = "green")  # +
  # scale_x_date(date_breaks = "1 year", date_labels = "%Y")
}

plotList[[1]]
plotList[[2]]
plotList[[3]]
plotList[[4]]
plotList[[5]] # da error porq no hay tantas estaciones



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Analisis Descriptivo - Superacion limite DIARIO ####

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


# Qué años el p99 se supera el limite de 24hs por año y estacion? #


tabla_dia_corr <- tabla_dia %>% 
  group_by(Estacion, Anio) %>%
  summarise(p99 = round(quantile(mean, probs = 0.99, na.rm = TRUE), 2),
            n_faltantes = sum(is.na(mean)),
            n = n())

tabla_dia_corr$p99[tabla_dia_corr$n < 274] <- NA # recorte en Anios 75% de datos
tabla_dia_corr$p99[tabla_dia_corr$n_faltantes > 91] <- NA #condicion de exclusion > datos faltantes


tabla_dia_corr %>% 
  group_by(Estacion) %>%
  .[which( .$p99 > 65),] %>%  # cambiar valor ACA
  group_by(Estacion, Anio) %>% 
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
  labs(x = "", y ="", title= "Número de superacion 24hs - p99 - 65 - MP2.5") # cambiar valor ACA


tabla_dia_corr %>% 
  group_by(Estacion) %>%
  .[which( .$p99 > 50),] %>%  # cambiar valor ACA
  group_by(Estacion, Anio) %>% 
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
  labs(x = "", y ="", title= "Número de superacion 24hs p99 - 50 - MP2.5") # cambiar valor ACA


tabla_dia_corr %>% 
  group_by(Estacion) %>%
  .[which( .$p99 > 37.5),] %>%  # cambiar valor ACA
  group_by(Estacion, Anio) %>% 
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
  labs(x = "", y ="", title= "Número de superacion 24hs p99 - 37.5 - MP2.5") # cambiar valor ACA


tabla_dia_corr %>% 
  group_by(Estacion) %>%
  .[which( .$p99 > 25),] %>%  # cambiar valor ACA
  group_by(Estacion, Anio) %>% 
  summarise(mediciones_dia = n()) %>%
  group_by(Estacion, Anio) %>% 
  summarise(n = n()) %>%
  ggplot( aes(x = Anio , y = Estacion, fill = n )) + 
  geom_tile() +
  geom_text(aes(label= n)) +
  theme_bw() +  
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 1),
        legend.position = "none") + 
  scale_fill_continuous(low="palegreen", high ="palegreen3", na.value = "transparent") + 
  labs(x = "", y ="", title= "Número de superacion 24hs p99 - 25 - MP2.5") # cambiar valor ACA



# Qué años la media se supera el limite de 24hs por año y estacion? #

## ATENTI:
## Creo tabla anual de medias diarias 

tabla_anio_med_dia <- tabla_dia %>% 
  group_by(Estacion, Anio) %>%
  summarise(mean1 = round(mean(mean, na.rm = TRUE), 2),
            n_faltantes = sum(is.na(mean)), #cuantos NA hay?
            n = n()) # cuantos datos hay?

names(tabla_anio_med_dia)[3] <- "mean" #recupero nombre de la variable

tabla_anio_med_dia$mean[tabla_anio_med_dia$n < 274] <- NA # recorte en Anios 75% de datos
tabla_anio_med_dia$mean[tabla_anio_med_dia$n_faltantes > 91] <- NA #condicion de exclusion > datos faltantes


tabla_anio_med_dia %>% 
  group_by(Estacion) %>%
  .[which( .$mean > 65),] %>%  # cambiar valor ACA
  group_by(Estacion, Anio) %>% 
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
  labs(x = "", y ="", title= "Número de superacion 24hs - media - 65 - MP2.5") # cambiar valor ACA


tabla_anio_med_dia %>% 
  group_by(Estacion) %>%
  .[which( .$mean > 50),] %>%  # cambiar valor ACA
  group_by(Estacion, Anio) %>% 
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
  labs(x = "", y ="", title= "Número de superacion 24hs media - 50 - MP2.5") # cambiar valor ACA


tabla_anio_med_dia %>% 
  group_by(Estacion) %>%
  .[which( .$mean > 37.5),] %>%  # cambiar valor ACA
  group_by(Estacion, Anio) %>% 
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
  labs(x = "", y ="", title= "Número de superacion 24hs media - 37.5 - MP2.5") # cambiar valor ACA


tabla_anio_med_dia %>% 
  group_by(Estacion) %>%
  .[which( .$mean > 25),] %>%  # cambiar valor ACA
  group_by(Estacion, Anio) %>% 
  summarise(mediciones_dia = n()) %>%
  group_by(Estacion, Anio) %>% 
  summarise(n = n()) %>%
  ggplot( aes(x = Anio , y = Estacion, fill = n )) + 
  geom_tile() +
  geom_text(aes(label= n)) +
  theme_bw() +  
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 1),
        legend.position = "none") + 
  scale_fill_continuous(low="palegreen", high ="palegreen3", na.value = "transparent") + 
  labs(x = "", y ="", title= "Número de superacion 24hs media - 25 - MP2.5") # cambiar valor ACA





# Cuantos dias se supera el limite de 24hs por año y estacion? #

tabla_dia %>% 
  group_by(Estacion) %>%
  .[which( .$mean > 65),] %>%  # cambiar valor ACA
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
  labs(x = "", y ="", title= "Número de superacion 24hs - 65 - PM2.5") # cambiar valor ACA


tabla_dia %>% 
  group_by(Estacion) %>%
  .[which( .$mean > 50),] %>% # cambiar valor ACA
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
  labs(x = "", y ="", title= "Número de superacion 24hs - 50 - PM2.5") # cambiar valor ACA


tabla_dia %>% 
  group_by(Estacion) %>%
  .[which( .$mean > 37.5),] %>%  # cambiar valor ACA
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
  labs(x = "", y ="", title= "Número de superacion 24hs - 37.5 - PM2.5") # cambiar valor ACA


tabla_dia %>% 
  group_by(Estacion) %>%
  .[which( .$mean > 25),] %>%   # cambiar valor ACA
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
  scale_fill_continuous(low="palegreen", high ="palegreen3", na.value = "transparent") + 
  labs(x = "", y ="", title= "Número de superacion 24hs - 25 - PM2.5")  # cambiar valor ACA


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# # Estadisticos descriptivos DIARIO ####

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


for(j in 1:length(levels(tabla_dia$Estacion))){
  a <- tabla_dia[which(tabla_dia$Estacion == levels(tabla_dia$Estacion)[j]),]
  
  #min = min(a$mean, na.rm = TRUE)
  #Q1 = round(quantile(a$mean, probs = 0.25, na.rm = TRUE), 2)
  #mean = round(mean(a$mean, na.rm = TRUE),2)
  #median = round(median(a$mean, na.rm = TRUE),2)
  p75 = round(quantile(a$mean, probs = 0.75, na.rm = TRUE), 2)
  #p99 = round(quantile(a$mean, probs = 0.99, na.rm = TRUE), 2)
  #max = round(max(a$mean, na.rm = TRUE),2)
  
  print(p75)
  #print(c(levels(tabla_dia$Estacion)[j], min, Q1[[1]], mean, median, p99[[1]], max))
}



ggplot(data = tabla_dia, aes(x= Estacion, y = mean )) + 
  geom_boxplot() +
  labs(x = "", y = expression(MP[10]~(ug.m^-3))) +
  theme_bw() +  
  theme(axis.title = element_text(size = rel(0.8) ),
        axis.text.x = element_text(angle = 90, vjust = 0.5 , hjust = 1))


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Estudio de tendencias ####

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


Theil <- tabla_dia %>% 
  rename( site = Estacion) %>%
  rename( PM10 = mean) %>%
  rename( date = Fecha) %>%
  TheilSen(., pollutant = "PM10", ylab = "PM10", 
           deseason = TRUE, 
           slope.percent = TRUE,
           type = "site",
           #layout = c(3, 8),
           scales = list(y = list(rot=45), x = list(rot = 45)))

a <-Theil$data$res2
a <- a[,c(1,2,12,16, 17)]
a <- data.frame( site = a$site,
                 p.stars = a$p.stars,
                 tendencia = paste(round(a$slope, 3), 
                                   " [", 
                                   round(a$lower, 3), ", ", 
                                   round(a$upper, 3), "]", sep="" ))


a <- a[,c(1,2,18:20)]
a <- data.frame( site = a$site,
                 p.stars = a$p.stars,
                 tendencia = paste(round(a$slope, 3), 
                                   " [", 
                                   round(a$lower, 3), ", ", 
                                   round(a$upper, 3), "]", sep="" ))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# # Serie de tiempo ANUAL #### 

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


# PM25 1 año > 25, 15, 12 , 10


# Usando la media diaria


plotList <- list()

for(j in 1:length(levels(tabla_anio_med_dia$Estacion))){
  b <- tabla_anio_med_dia[which(tabla_anio_med_dia$Estacion == levels(tabla_anio_med_dia$Estacion)[j]),]
  
  plotList[[j]] <- ggplot(b, aes( x = as.numeric(as.character(Anio)), 
                                  y = mean)) + 
    geom_line() + theme_bw() + 
    labs(title = as.character(levels(tabla_anio_med_dia$Estacion)[j])) +
    geom_hline(yintercept=25, col = "red") + 
    geom_hline(yintercept= 15, col = "orange") + 
    geom_hline(yintercept=12, col = "yellow") + 
    geom_hline(yintercept=10, col = "green") +
    labs(x = "", y = expression(Media~anual~MP[2.5]~(ug.m^-3)))
  
}

plotList[[1]]
plotList[[2]]
plotList[[3]]
plotList[[4]]
plotList[[5]]


# Usando datos horarios

plotList <- list()

for(j in 1:length(levels(tabla_anio$Estacion))){
  b <- tabla_anio[which(tabla_anio$Estacion == levels(tabla_anio$Estacion)[j]),]
  
  plotList[[j]] <- ggplot(b, aes( x = as.numeric(as.character(Anio)), 
                                  y = mean)) + 
    geom_line() + theme_bw() + 
    labs(title = as.character(levels(tabla_anio$Estacion)[j])) +
    geom_hline(yintercept=25, col = "red") + 
    geom_hline(yintercept= 15, col = "orange") + 
    geom_hline(yintercept=12, col = "yellow") + 
    geom_hline(yintercept=10, col = "green") +
    labs(x = "", y = expression(Media~anual~MP[2.5]~(ug.m^-3)))
  
}

plotList[[1]]
plotList[[2]]
plotList[[3]]
plotList[[4]]
plotList[[5]]


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Analisis Descriptivo - Superacion limite ANUAL ####

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Medias anuales
tabla_anio %>% group_by(Estacion, Anio) %>%
  summarise(mean = mean(mean, na.rm = TRUE) )


# Numero de superacion
tabla_anio %>% 
  group_by(Estacion) %>%
  .[which( .$mean> 50),] %>%
  group_by(Estacion, Anio) %>% 
  summarise(n = n()) %>%
  ggplot( aes(x = Anio , y = Estacion, fill = n )) + 
  geom_tile() +
  geom_text(aes(label= n)) +
  theme_bw() +  
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 1),
        legend.position = "none") + 
  scale_fill_continuous(low="pink", high ="red4", na.value = "transparent") + 
  labs(x = "", y ="", title= "Número de superacion anual - 50 - PM10")



tabla_anio %>% 
  group_by(Estacion) %>%
  .[which( .$mean> 30),] %>%
  group_by(Estacion, Anio) %>% 
  summarise(n = n()) %>%
  ggplot( aes(x = Anio , y = Estacion, fill = n )) + 
  geom_tile() +
  geom_text(aes(label= n)) +
  theme_bw() +  
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 1),
        legend.position = "none") + 
  scale_fill_continuous(low="khaki", high ="khaki3", na.value = "transparent") + 
  labs(x = "", y ="", title= "Número de superacion anual - 30 - PM10")


tabla_anio %>% 
  group_by(Estacion) %>%
  .[which( .$mean> 20),] %>%
  group_by(Estacion, Anio) %>% 
  summarise(n = n()) %>%
  ggplot( aes(x = Anio , y = Estacion, fill = n )) + 
  geom_tile() +
  geom_text(aes(label= n)) +
  theme_bw() +  
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 1),
        legend.position = "none") + 
  scale_fill_continuous(low="palegreen", high ="palegreen3", na.value = "transparent") + 
  labs(x = "", y ="", title= "Número de superacion anual - 20 - PM10")


print(tabla_anio[complete.cases(tabla_anio),])

# Estadisticos


for(j in 1:length(levels(tabla_anio$Estacion))){
  a <- tabla_dia[which(tabla_anio$Estacion == levels(tabla_anio$Estacion)[j]),]
  
  min = min(a$mean, na.rm = TRUE)
  Q1 = round(quantile(a$mean, probs = 0.25, na.rm = TRUE), 2)
  mean_ = round(mean(a$mean, na.rm = TRUE),2)
  median = round(median(a$mean, na.rm = TRUE),2)
  p75 = round(quantile(a$mean, probs = 0.75, na.rm = TRUE), 2)
  p99 = round(quantile(a$mean, probs = 0.99, na.rm = TRUE), 2)
  max = round(max(a$mean, na.rm = TRUE),2)
  
  print(c(levels(tabla_dia$Estacion)[j], min, Q1[[1]], mean, median, p99[[1]], max))
}


# Calculo de p98 
a <- tabla_dia[which(tabla_anio$Estacion == "DS EMC1")]
round(quantile(a$mean[which(a$Anio == 2016)], probs = 0.98, na.rm = TRUE), 2)
round(quantile(a$mean[which(a$Anio == 2017)], probs = 0.98, na.rm = TRUE), 2)
round(quantile(a$mean[which(a$Anio == 2018)], probs = 0.98, na.rm = TRUE), 2)
round(quantile(a$mean[which(a$Anio == 2019)], probs = 0.98, na.rm = TRUE), 2)


ggplot(data = tabla_anio, aes(x= Estacion, y = mean )) + 
  geom_boxplot() +
  labs(x = "", y = expression(Media~anual~MP[10]~(ug.m^-3))) +
  theme_bw() +  
  theme(axis.title = element_text(size = rel(0.8) ),
        axis.text.x = element_text(angle = 90, vjust = 0.5 , hjust = 1))
