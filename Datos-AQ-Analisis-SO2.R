## 18/02/2021
## Autor: Sol Represa
## Analisis para informe convenio sobre nueva normativa encalidad de aire ACUMAR


library(dplyr)
library(ggplot2)
library(lubridate)
library(openair)
library(data.table)


## SO2

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Preparacion de datos

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

getwd()
setwd("C:/Users/solre/Desktop/ACUMAR/")

contaminantes <- readRDS("contaminantes.rds")

nombres_contaminantes <- c("CO", "SO2", "NO2",
                           "NOx", "O3", "SH2",
                           "PM")

tabla <- contaminantes[[2]]

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
  summarise(mean = round(mean(`SO2-1h`, na.rm = TRUE), 2),
            n_faltantes = sum(is.na(`SO2-1h`)))

tabla_dia$mean[tabla_dia$n_faltantes > 6] <- NA #condicion de exclusion
tabla_dia$Fecha <- as.Date(paste(tabla_dia$Anio, tabla_dia$Mes, tabla_dia$Dia, sep = "-"))


# # # SERIE DE TIEMPO ANUAL # # # <<< NO SE USA !!!!
## 75 % de los datos anuales son 282 datos diarios (6768 datos horarios)
## es decir,  93 datos faltantes diarios (o 2232 horarios)
## entonces, la condición es q el año se anule si tiene datos faltantes MAYOR a 2232 datos horarios

tabla_anio <- tabla %>% 
  group_by(Estacion, Anio) %>%
  summarise(mean = round(mean(`SO2-1h`, na.rm = TRUE), 2),
            n_faltantes = sum(is.na(`SO2-1h`)),
            n = n())

tabla_anio$mean[tabla_anio$n < 6768] <- NA #condicion de exclusion > dias del año
tabla_anio$mean[tabla_anio$n_faltantes > 2232] <- NA #condicion de exclusion > datos faltantes



# # # SERIE DE TIEMPO - 3hs # # # 

tabla$T3[tabla$Hora == 1 | tabla$Hora == 2 | tabla$Hora == 3] <- "I"
tabla$T3[tabla$Hora == 4 | tabla$Hora == 5 | tabla$Hora == 6] <- "II"
tabla$T3[tabla$Hora == 7 | tabla$Hora == 8 | tabla$Hora == 9] <- "III"
tabla$T3[tabla$Hora == 10 | tabla$Hora == 11 | tabla$Hora == 12] <- "IV"
tabla$T3[tabla$Hora == 13 | tabla$Hora == 14 | tabla$Hora == 15] <- "V"
tabla$T3[tabla$Hora == 16 | tabla$Hora == 17 | tabla$Hora == 18] <- "VI"
tabla$T3[tabla$Hora == 19 | tabla$Hora == 20 | tabla$Hora == 21] <- "VII"
tabla$T3[tabla$Hora == 22 | tabla$Hora == 23 | tabla$Hora == 0] <- "VIII"


tabla_3h <- tabla %>% 
  group_by(Estacion, Anio, Mes, Dia, T3) %>%
  summarise(mean = round(mean(`SO2-1h`, na.rm = TRUE), 2))

#tabla_3h$Fecha <- as.Date(paste(tabla_3h$Anio, tabla_dia$Mes, tabla_dia$Dia, sep = "-"))



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# # Serie de tiempo DIARIO #### 

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# SO2: 24 hs = 200, 160, 125, 50, 20


plotList <- list()

for(j in 1:length(levels(tabla_dia$Estacion))){
  a <- tabla_dia[which(tabla_dia$Estacion == levels(tabla_dia$Estacion)[j]),]
  
  plotList[[j]] <- ggplot(a, aes( x = Fecha, y = mean)) + 
    geom_line() + theme_bw() + 
    labs(x ="", y = expression(SO[2]~(ug.m^-3)), 
         title = paste0(as.character(levels(tabla_dia$Estacion)[j]), " - horarias")) +
    #geom_hline(yintercept=367, col = "brown") + 
    geom_hline(yintercept=200, col = "red") + 
    geom_hline(yintercept=160, col = "orange") + 
    geom_hline(yintercept=125, col = "yellow") + 
    geom_hline(yintercept=50, col = "cyan") +
    geom_hline(yintercept=20, col = "green")  # +
  # scale_x_date(date_breaks = "1 year", date_labels = "%Y")
}

plotList[[1]]
plotList[[2]]
plotList[[3]]
plotList[[4]]

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Analisis Descriptivo - Superacion limite HORARIO ####

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Cuantos horas supera el limite de horario por año y estacion? #

tabla %>% 
  group_by(Estacion) %>%
  .[which( .$`SO2-1h` > 250),] %>%  # modificar ACA
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
  labs(x = "", y ="", title= "Número de superacion horaria - 250 - SO2") # modificar ACA


tabla %>% 
  group_by(Estacion) %>%
  .[which( .$`SO2-1h` > 230),] %>%  # modificar ACA
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
  labs(x = "", y ="", title= "Número de superacion horaria - 230 - SO2") # modificar ACA


tabla %>% 
  group_by(Estacion) %>%
  .[which( .$`SO2-1h` > 196),] %>%  # modificar ACA
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
  labs(x = "", y ="", title= "Número de superacion horaria - 196 - SO2") # modificar ACA



# Qué años el p99 se supera el limite horario por año y estacion? #


tabla_corr <- tabla %>% 
  group_by(Estacion, Anio) %>%
  summarise(p99 = round(quantile(`SO2-1h`, probs = 0.99, na.rm = TRUE), 2))


tabla_corr %>% 
  group_by(Estacion) %>%
  .[which( .$p99 > 150),] %>%  # modificar ACA
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
  labs(x = "", y ="", title= "Número de superacion horaria - p99 - 150 - SO2") # modificar ACA


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Analisis Descriptivo - Superacion limite DIARIO ####

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 



# Qué años el p98 se supera el limite de 24hs por año y estacion? #


tabla_dia_corr <- tabla_dia %>% 
  group_by(Estacion, Anio) %>%
  summarise(p98 = round(quantile(mean, probs = 0.98, na.rm = TRUE), 2),
            n_faltantes = sum(is.na(mean)),
            n = n())

tabla_dia_corr$p98[tabla_dia_corr$n < 274] <- NA # recorte en Anios 75% de datos
tabla_dia_corr$p98[tabla_dia_corr$n_faltantes > 91] <- NA #condicion de exclusion > datos faltantes


tabla_dia_corr %>% 
  group_by(Estacion) %>%
  .[which( .$p98 > 200),] %>%  # modificar ACA
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
  labs(x = "", y ="", title= "Número de superacion 24hs - p98 - 200 - SO2") # modificar ACA


tabla_dia_corr %>% 
  group_by(Estacion) %>%
  .[which( .$p98 > 160),] %>%
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
  labs(x = "", y ="", title= "Número de superacion 24hs p98 - 160 - SO2")


tabla_dia_corr %>% 
  group_by(Estacion) %>%
  .[which( .$p98 > 125),] %>%
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
  scale_fill_continuous(low="tan1", high ="tan3", na.value = "transparent") +
  labs(x = "", y ="", title= "Número de superacion 24hs p98 - 125 - SO2")


tabla_dia_corr %>% 
  group_by(Estacion) %>%
  .[which( .$p98 > 50),] %>% # modificar ACA
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
  labs(x = "", y ="", title= "Número de superacion 24hs p98 - 50 - SO2") # modificar ACA


tabla_dia_corr %>% 
  group_by(Estacion) %>%
  .[which( .$p98 > 20),] %>% # modificar ACA
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
  labs(x = "", y ="", title= "Número de superacion 24hs p98 - 20 - SO2") # modificar ACA




# Cuantos dias se supera el limite de 24hs por año y estacion? #

tabla_dia %>% 
  group_by(Estacion) %>%
  .[which( .$mean > 200),] %>%   # modificar ACA
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
  labs(x = "", y ="", title= "Número de superacion 24hs - 200 - SO2")  # modificar ACA


tabla_dia %>% 
  group_by(Estacion) %>%
  .[which( .$mean > 160),] %>%  # modificar ACA
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
  labs(x = "", y ="", title= "Número de superacion 24hs - 160 - SO2")  # modificar ACA


tabla_dia %>% 
  group_by(Estacion) %>%
  .[which( .$mean > 125),] %>%  # modificar ACA
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
  scale_fill_continuous(low="tan1", high ="tan3", na.value = "transparent") + 
  labs(x = "", y ="", title= "Número de superacion 24hs - 125 - SO2")  # modificar ACA


tabla_dia %>% 
  group_by(Estacion) %>%
  .[which( .$mean > 50),] %>%  # modificar ACA
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
  labs(x = "", y ="", title= "Número de superacion 24hs - 50 - SO2")  # modificar ACA

tabla_dia %>% 
  group_by(Estacion) %>%
  .[which( .$mean > 20),] %>%  # modificar ACA
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
  labs(x = "", y ="", title= "Número de superacion 24hs - 20 - SO2")  # modificar ACA



