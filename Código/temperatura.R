# ______________________________________________________________________________
#
# Proyecto:       Evaluating an Agroforestal Intervention in Rural Mexico: 
#                 Sembrando Vida
# Script:         temperatura.R
# Objetivo:       Generar el algoritmo que asigna observaciones de clima
#                 a cada uno de los municipios.  
#
# Autor:          Daniel Kelly
# Email:          djsanchez@colmex.mx
# 
# Fecha:          25 de Septiembre de 2024
#
# ______________________________________________________________________________

# PREAMBULO --------------------------------------------------------------------

setwd("/Users/danielkelly/Library/Mobile Documents/com~apple~CloudDocs/Tésis")

rm( list=ls() )

# librerias
library(tidyverse)
library(readxl)
library(openxlsx)
library(eventstudyr)
library(fixest)

# utilidades
source('~/Documents/Estadística/R/Utilities/lineplot.R')
source('~/Documents/Estadística/R/Utilities/period.R')
source('~/Documents/Estadística/R/Utilities/myacf.R')
source('~/Documents/Estadística/R/Utilities/mypacf.R')
source('~/Documents/Estadística/R/Utilities/colores.R')
source('~/Documents/Estadística/R/Utilities/grangertest.R') 
source('~/Documents/Estadística/R/Utilities/remove_accents.R')

# CÓDIGO -----------------------------------------------------------------------

## Data ------------------------------------------------------------------------

# Definir años y meses:
years <- 2018:2023
months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")

catalogo <- read_xlsx("Bases de Datos/Finales/Catálogo de Municipios.xlsx")

centroides <- read.csv("Bases de Datos/División Política Municipal/Coordenadas.csv") %>%
  mutate(
    CVEGEO=ifelse(nchar(CVEGEO)==4, paste0("0", CVEGEO), as.character(CVEGEO)),
    CVE_ENT=ifelse(nchar(CVEGEO)==1, paste0("0", CVE_ENT), as.character(CVE_ENT)),
    CVE_MUN=case_when(
      nchar(CVE_MUN)==1 ~ paste0("00", CVE_MUN),
      nchar(CVE_MUN)==2 ~ paste0("0", CVE_MUN),
      nchar(CVE_MUN)==3 ~ as.character(CVE_MUN)
    )
  ) %>%
  rename(
    cve=CVEGEO,
    cve_ent=CVE_ENT,
    cve_mun=CVE_MUN,
    longitud=x,
    latitud=y
  )

# Fechas:
fechas <- seq(as.Date("2018-01-01"), as.Date("2023-12-01"), by="month")

## Script ----------------------------------------------------------------------

for (y in years) {
  for (m in months) {
    file_name <- file.path("Bases de Datos", "Clima", "Temperatura Media", paste(paste(y, m, sep="-"), "csv", sep="."))
    data <- read.csv(file_name)[c(1:2, 6)]
    names(data) <- c("longitud", "latitud", "temperatura")
    assign(paste("temp", y, m, sep="_"), data)
  }
}


# Estructura de la base final:
municipios <- unique(centroides$cve)

base_final <- expand.grid(municipios, fechas) %>%
  as.data.frame() 

names(base_final) <- c("cve", "fecha")

base_final <- arrange(base_final, cve, fecha) %>%
  mutate(temperatura=NA)


# Objeto con nombres de temperatura:
temp <- as.character()
for (y in years) {
  for (m in months) {
    lluvia <- c(temp, paste("lluvia", y, m, sep="_"))
  }
}

# Loop para asignar observaciones de clima
list_distancias <- list()
for (i in municipios) {
  cat('.')
  
  for (y in years) {
    for (m in months) {
      
      fecha <- as.Date(paste(y, m, "01", sep="-"))
      clima <- get(paste("temp", y, m, sep="_"))
      mun <- filter(centroides, cve == i)
      
      x2 <- as.numeric(mun$longitud)
      y2 <- as.numeric(mun$latitud)
      
      distancias <- numeric(nrow(clima))  
      
      for (j in 1:nrow(clima)) {
        x1 <- as.numeric(clima[j, 1])
        y1 <- as.numeric(clima[j, 2])
        
        distancias[j] <- sqrt((x2 - x1)^2 + (y2 - y1)^2)
      }
      
      la.buena <- clima[which.min(distancias), 3]  
      
      base_final[base_final$cve == i & base_final$fecha == fecha, 3] <- la.buena
      
      list_distancias[[paste0(y,m,i)]] <- distancias %>% as.data.frame()
    }
  }
}

# Máxima distancia
df_distancias <- bind_rows(list_distancias) 

names(df_distancias) <- "distancia"

df_distancias <- filter(df_distancias, !is.na(distancia))

max(df_distancias$distancia)
min(df_distancias$distancia)

mean(df_distancias$distancia)

write.xlsx(base_final, "Bases de Datos/Finales/Temperatura.xlsx")
