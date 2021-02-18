## 08/02/2021
## Autor: Sol Represa
## Analisis para informe convenio sobre nueva normativa encalidad de aire ACUMAR


library(dplyr)
library(ggplot2)
library(lubridate)
library(openair)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Preparacion de datos

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

nombreas_contaminantes <- c("CO", "SO2", "NO",
                            "NOx", "O3", "SH2",
                            "PM")

tabla <- contaminantes[[7]]

names(tabla)
#i = 2

tabla$Estacion <- factor(tabla$Estacion)
levels(tabla$Estacion)

tabla$Anio  <- year(tabla$`Fecha-Hora`)
tabla$Mes <- month(tabla$`Fecha-Hora`)

# Quitar datos de años que no usamos
tabla <- tabla[-which(tabla$Anio == 2021 | tabla$Anio == 2022 | tabla$Anio == 2023 | tabla$Anio == 2024),]

tabla$Mes <- factor(tabla$Mes)
tabla$Anio <- factor(tabla$Anio)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# # SERIE DE TIEMPO HORARIA #### 

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# PM10: 24 hs = 150, 100, 75, 50


plotList <- list()

for(j in 1:length(levels(tabla$Estacion))){
  a <- tabla[which(tabla$Estacion == levels(tabla$Estacion)[j]),]
  
  plotList[[j]] <- ggplot(a, aes( x = `Fecha-Hora`, y = `PM10-1h`)) + 
    geom_line() + theme_bw() + 
    labs(title = as.character(levels(tabla$Estacion)[j])) +
    geom_hline(yintercept=150, col = "red") + 
    geom_hline(yintercept=100, col = "orange") + 
    geom_hline(yintercept=75, col = "yellow") + 
    geom_hline(yintercept=50, col = "green")
  
}


plotList <- list()

for(j in 1:length(levels(tabla$Estacion))){
  a <- tabla[which(tabla$Estacion == levels(tabla$Estacion)[j]),]
  
  plotList[[j]] <- ggplot(a, aes( x = `Fecha-Hora`, y = `PM10-1h`)) + 
    geom_line() + theme_bw() + 
    labs(title = as.character(levels(tabla$Estacion)[j])) +
    geom_hline(yintercept=150, col = "red") + 
    geom_hline(yintercept=100, col = "orange") + 
    geom_hline(yintercept=75, col = "yellow") + 
    geom_hline(yintercept=50, col = "green")
  
}



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Analisis Descriptivo - Superacion limite HORARIO ####

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

library(data.table)

# Cuantos dias se supera el limite de 24hs por año y estacion? #

tabla %>% 
  group_by(Estacion) %>%
  .[which( .$`PM10-1h`> 150),] %>%
  mutate(Dia = day(`Fecha-Hora`)) %>%
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
  labs(x = "", y ="", title= "Número de superacion 24hs - 150 - PM10")


tabla %>% 
  group_by(Estacion) %>%
  .[which( .$`PM10-1h`> 100),] %>%
  mutate(Dia = day(`Fecha-Hora`)) %>%
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
  labs(x = "", y ="", title= "Número de superacion 24hs - 100 - PM10")


tabla %>% 
  group_by(Estacion) %>%
  .[which( .$`PM10-1h`> 75),] %>%
  mutate(Dia = day(`Fecha-Hora`)) %>%
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
  labs(x = "", y ="", title= "Número de superacion 24hs - 75 - PM10")


tabla %>% 
  group_by(Estacion) %>%
  .[which( .$`PM10-1h`> 50),] %>%
  mutate(Dia = day(`Fecha-Hora`)) %>%
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
  labs(x = "", y ="", title= "Número de superacion 24hs - 50 - PM10")


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# # Estadisticos descriptivos HORARIO ####

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


tabla %>% 
  group_by(Estacion) %>%
  summarise( min = min(`PM10-1h`, na.rm = TRUE),
             Q1 = quantile(`PM10-1h`, probs = 0.25, na.rm = TRUE),
             mean = mean(`PM10-1h`, na.rm = TRUE),
             median = median(`PM10-1h`, na.rm = TRUE),
             Q3 = quantile(`PM10-1h`, probs = 0.25, na.rm = TRUE),
             p90 = quantile(`PM10-1h`,probs = 0.90, na.rm = TRUE),
             p99 = quantile(`PM10-1h`,probs = 0.99, na.rm = TRUE),
             max = max(`PM10-1h`, na.rm = TRUE),
             sd = sd(`PM10-1h`, na.rm = TRUE))




# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Estudio de tendencias ####

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


Theil <- tabla %>% 
  rename( site = Estacion) %>%
  rename( PM10 = `PM10-1h`) %>%
  rename( date = `Fecha-Hora`) %>%
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

