## 22/01/2021
## Autor: Sol Represa
## Analisis descriptivo de datos de calidad de aire ACUMAR

# Objetivo: Revision de base de datos y 
# generación de tabla unica con contaminantes


library(openxlsx)



getwd()
setwd("C:/Users/solre/Desktop/ACUMAR/Monitoreos Continuos y Automáticos/")



nombres_est <- data.frame( DS =c("DS EMC1", "DS OP1", "DS OP2"),
                           LM = c("LM EMC2 MB", "LM2 EMC2 AER", NA), 
                           LE = c("LE EMC2", NA, NA)) 



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

## DATOS DE CO  ####

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

carpetas <- dir(full.names = TRUE)

tabla_cont <- data.frame()


for( c in 1:length(carpetas)){
  
  carpetas_estaciones <- dir(carpetas[c], full.names = TRUE)
  
  for( ce in 1:length(carpetas_estaciones)){
    
    ls <- list.files(carpetas_estaciones[ce], pattern = ".xlsx", full.names = TRUE)
    f <- grep(" CO ", ls)
    
    if(length(f) != 0){
      datos <- read.xlsx(ls[f])
      
      datos$Fecha.y.hora <- convertToDateTime(datos$Fecha.y.hora)
      
      tabla_int <- datos[c(3,9, 10)]
      tabla_int$estacion <- as.character(nombres_est[ce, c])
      names(tabla_int)  <- c("Fecha-Hora", "CO-1h", "CO-8h", "Estacion")
      
      tabla_cont <- rbind(tabla_cont, tabla_int) 
      
      rm(datos)
    }
    
  }
  
}

tabla_CO <- tabla_cont

rm(tabla_cont, tabla_int)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

## DATOS DE SO2  ####

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

carpetas <- dir(full.names = TRUE)

tabla_cont <- data.frame()

for( c in 1:length(carpetas)){
  
  carpetas_estaciones <- dir(carpetas[c], full.names = TRUE)
  
  for( ce in 1:length(carpetas_estaciones)){
    
    ls <- list.files(carpetas_estaciones[ce], pattern = ".xlsx", full.names = TRUE)
    f <- grep(" SO2 ", ls)
    
    if(length(f) != 0){
      datos <- read.xlsx(ls[f])
      
      datos$Fecha.y.hora <- convertToDateTime(datos$Fecha.y.hora)
      
      tabla_int <- datos[c(3, 9, 10, 11)]
      tabla_int$estacion <- as.character(nombres_est[ce, c])
      names(tabla_int)  <- c("Fecha-Hora", "SO2-1h", "SO2-3h", "SO2-24h", "Estacion")
      
      tabla_cont <- rbind(tabla_cont, tabla_int) 
      
      rm(datos)
    }
    
  }
  
}


tabla_SO2 <- tabla_cont

rm(tabla_cont, tabla_int)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

## DATOS DE NO2  ####

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

carpetas <- dir(full.names = TRUE)

tabla_cont <- data.frame()


for( c in 1:length(carpetas)){
  
  carpetas_estaciones <- dir(carpetas[c], full.names = TRUE)
  
  for( ce in 1:length(carpetas_estaciones)){
    
    ls <- list.files(carpetas_estaciones[ce], pattern = ".xlsx", full.names = TRUE)
    f <- grep(" NO2 ", ls)
    
    if(length(f) != 0){
      datos <- read.xlsx(ls[f])
      
      datos$Fecha.y.hora <- convertToDateTime(datos$Fecha.y.hora)
      
      tabla_int <- datos[c(3,9)]
      tabla_int$estacion <- as.character(nombres_est[ce, c])
      names(tabla_int) <- c("Fecha-Hora", "NO2-1h", "Estacion")
      
      tabla_cont <- rbind(tabla_cont, tabla_int) 
      
      rm(datos)
    }
    
  }
  
}


tabla_NO2 <- tabla_cont

rm(tabla_cont, tabla_int)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

## DATOS DE NOx  ####

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

carpetas <- dir(full.names = TRUE)

tabla_cont <- data.frame()

for( c in 1:length(carpetas)){
  
  carpetas_estaciones <- dir(carpetas[c], full.names = TRUE)
  
  for( ce in 1:length(carpetas_estaciones)){
    
    ls <- list.files(carpetas_estaciones[ce], pattern = ".xlsx", full.names = TRUE)
    f <- grep(" NOx ", ls)
    
    if(length(f) != 0){
      datos <- read.xlsx(ls[f])
      
      datos$Fecha.y.hora <- convertToDateTime(datos$Fecha.y.hora)
      
      tabla_int <- datos[c(3,9)]
      tabla_int$estacion <- as.character(nombres_est[ce, c])
      names(tabla_int) <- c("Fecha-Hora", "NOx-1h", "Estacion")
      
      tabla_cont <- rbind(tabla_cont, tabla_int) 
      
      rm(datos)
    }
    
  }
  
}


tabla_NOx <- tabla_cont

rm(tabla_cont, tabla_int)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

## DATOS DE O3  ####

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

carpetas <- dir(full.names = TRUE)


tabla_cont <- data.frame()

for( c in 1:length(carpetas)){
  
  carpetas_estaciones <- dir(carpetas[c], full.names = TRUE)
  
  for( ce in 1:length(carpetas_estaciones)){
    
    ls <- list.files(carpetas_estaciones[ce], pattern = ".xlsx", full.names = TRUE)
    f <- grep(" O3 ", ls)
    
    if(length(f) != 0){
      datos <- read.xlsx(ls[f])
      
      datos$Fecha.y.hora <- convertToDateTime(datos$Fecha.y.hora)
      
      tabla_int <- datos[c(3,9, 10)]
      tabla_int$estacion <- as.character(nombres_est[ce, c])
      names(tabla_int)  <- c("Fecha-Hora", "O3-1h", "O3-8h", "Estacion")
      
      tabla_cont <- rbind(tabla_cont, tabla_int) 
      
      rm(datos)
    }
    
  }
  
}


tabla_O3 <- tabla_cont

rm(tabla_cont, tabla_int)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

## DATOS DE SH2  ####

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

carpetas <- dir(full.names = TRUE)

tabla_cont <- data.frame()


for( c in 1:length(carpetas)){
  
  carpetas_estaciones <- dir(carpetas[c], full.names = TRUE)
  
  for( ce in 1:length(carpetas_estaciones)){
    
    ls <- list.files(carpetas_estaciones[ce], pattern = ".xlsx", full.names = TRUE)
    f <- grep(" SH2 ", ls)
    
    if(length(f) != 0){
      datos <- read.xlsx(ls[f])
      
      datos$Fecha.y.hora <- convertToDateTime(datos$Fecha.y.hora)
      
      tabla_int <- datos[c(3,9)]
      tabla_int$estacion <- as.character(nombres_est[ce, c])
      names(tabla_int) <- c("Fecha-Hora", "SH2-1h", "Estacion")
      
      tabla_cont <- rbind(tabla_cont, tabla_int) 
      
      rm(datos)
    }
    
  }
 
}


tabla_SH2 <- tabla_cont

rm(tabla_cont, tabla_int)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

## DATOS DE Material Particulado  ####

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

carpetas <- dir(full.names = TRUE)

tabla_cont <- data.frame()


for( c in 1:length(carpetas)){
  
  carpetas_estaciones <- dir(carpetas[c], full.names = TRUE)
  
  for( ce in 1:length(carpetas_estaciones)){
    
    ls <- list.files(carpetas_estaciones[ce], pattern = ".xlsx", full.names = TRUE)
    f <- grep(" PM ", ls)
    
    if(length(f) != 0){
      datos <- read.xlsx(ls[f])
      
      datos$Fecha.y.hora <- convertToDateTime(datos$Fecha.y.hora)
      
      tabla_int <- datos[c(3, 9, 10)]
      tabla_int$estacion <- as.character(nombres_est[ce, c])
      names(tabla_int) <- c("Fecha-Hora", "PM10-1h", "PM25-1h", "Estacion")
      
      tabla_cont <- rbind(tabla_cont, tabla_int) 
      
      rm(datos)
    }
    
    f10 <- grep(" PM10 ", ls)
    
    if(length(f10) != 0){
      datos <- read.xlsx(ls[f10])
      
      datos$Fecha.y.hora <- convertToDateTime(datos$Fecha.y.hora)
      
      tabla_int <- datos[c(3, 9, 10)]
      tabla_int$estacion <- as.character(nombres_est[ce, c])
      names(tabla_int) <- c("Fecha-Hora", "PM10-1h", "PM25-1h", "Estacion")
      
      tabla_cont <- rbind(tabla_cont, tabla_int) 
      
      rm(datos)
    }
  }
  
}


tabla_PM <- tabla_cont

rm(tabla_cont, tabla_int)


# # # # # # # # # # # # # # # # # # # # # # # # # # # 

## Crear LISTA ####

# # # # # # # # # # # # # # # # # # # # # # # # # # # 


nombreas_contaminantes <- c("CO", "SO2", "NO",
                   "NOx", "O3", "SH2",
                   "PM")

contaminantes <- list( tabla_CO, tabla_SH2,
                       tabla_NO2, tabla_NOx,
                       tabla_O3, tabla_SH2,
                       tabla_PM)
