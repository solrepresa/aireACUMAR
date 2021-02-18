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
tabla$Dia <- day(tabla$`Fecha-Hora`)

# Quitar datos de años que no usamos
tabla <- tabla[-which(tabla$Anio == 2021 | tabla$Anio == 2022 | tabla$Anio == 2023 | tabla$Anio == 2024),]

tabla$Mes <- factor(tabla$Mes)
tabla$Anio <- factor(tabla$Anio)
tabla$Dia <- factor(tabla$Dia)



# # # SERIE DE TIEMPO - 24hs # # # 
## CON 75 % de los datos >> 18 datos / 6 faltantes

tabla_dia <- tabla %>% 
  group_by(Estacion, Anio, Mes, Dia) %>%
  summarise(mean = round(mean(`PM10-1h`, na.rm = TRUE), 2),
            n = sum(is.na(`PM10-1h`)))

tabla_dia$mean[tabla_dia$n > 6] <- NA #condicion de exclusion
tabla_dia$Fecha <- as.Date(paste(tabla_dia$Anio, tabla_dia$Mes, tabla_dia$Dia, sep = "-"))


# # # SERIE DE TIEMPO ANUAL # # # 
## CON 75 % de los datos >> 282 datos diarios (6768 horarios) / 93 faltantes diarios (2232 horarios)

tabla_anio <- tabla %>% 
  group_by(Estacion, Anio) %>%
  summarise(mean = round(mean(`PM10-1h`, na.rm = TRUE), 2),
            n = sum(is.na(`PM10-1h`)))

tabla_anio$mean[tabla_anio$n > 2232] <- NA #condicion de exclusion




# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# # Serie de tiempo DIARIO #### 

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# PM10: 24 hs = 150, 100, 75, 50


plotList <- list()

for(j in 1:length(levels(tabla_dia$Estacion))){
  a <- tabla_dia[which(tabla_dia$Estacion == levels(tabla_dia$Estacion)[j]),]
  
  plotList[[j]] <- ggplot(a, aes( x = Fecha, y = mean)) + 
    geom_line() + theme_bw() + 
    labs(x = "", y ="",
         title = paste0(as.character(levels(tabla_dia$Estacion)[j]), " - 24hs")) +
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

tabla_dia %>% 
  group_by(Estacion) %>%
  .[which( .$mean > 150),] %>%
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


tabla_dia %>% 
  group_by(Estacion) %>%
  .[which( .$mean > 100),] %>%
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


tabla_dia %>% 
  group_by(Estacion) %>%
  .[which( .$mean > 75),] %>%
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


tabla_dia %>% 
  group_by(Estacion) %>%
  .[which( .$mean > 50),] %>%
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


for(j in 1:length(levels(tabla_dia$Estacion))){
  a <- tabla_dia[which(tabla_dia$Estacion == levels(tabla_dia$Estacion)[j]),]
  
  min = min(a$mean, na.rm = TRUE)
  Q1 = round(quantile(a$mean, probs = 0.25, na.rm = TRUE), 2)
  mean = round(mean(a$mean, na.rm = TRUE),2)
  median = round(median(a$mean, na.rm = TRUE),2)
  p99 = round(quantile(a$mean, probs = 0.99, na.rm = TRUE), 2)
  max = round(max(a$mean, na.rm = TRUE),2)
  
  print(c(levels(tabla_dia$Estacion)[j], min, Q1[[1]], mean, median, p99[[1]], max))
}





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


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# # Serie de tiempo ANUAL #### 

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


# PM10 1 año > 50, 30, 20


plotList <- list()

for(j in 1:length(levels(tabla_anio$Estacion))){
  a <- tabla_anio[which(tabla_anio$Estacion == levels(tabla_anio$Estacion)[j]),]
  
  plotList[[j]] <- ggplot(a, aes( x = as.numeric(as.character(Anio)), 
                                  y = mean)) + 
    geom_line() + theme_bw() + 
    labs(title = as.character(levels(tabla_anio$Estacion)[j])) +
    geom_hline(yintercept=50, col = "red") + 
    geom_hline(yintercept= 30, col = "orange") + 
    geom_hline(yintercept=20, col = "green") +
    labs(x = "", y = "Media anual")
  
}



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Analisis Descriptivo - Superacion limite ANUAL ####

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


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



# Estadisticos


for(j in 1:length(levels(tabla_anio$Estacion))){
  a <- tabla_dia[which(tabla_anio$Estacion == levels(tabla_anio$Estacion)[j]),]
  
  min = min(a$mean, na.rm = TRUE)
  Q1 = round(quantile(a$mean, probs = 0.25, na.rm = TRUE), 2)
  mean = round(mean(a$mean, na.rm = TRUE),2)
  median = round(median(a$mean, na.rm = TRUE),2)
  p99 = round(quantile(a$mean, probs = 0.99, na.rm = TRUE), 2)
  max = round(max(a$mean, na.rm = TRUE),2)
  
  print(c(levels(tabla_dia$Estacion)[j], min, Q1[[1]], mean, median, p99[[1]], max))
}

