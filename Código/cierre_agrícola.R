# ______________________________________________________________________________
#
# Proyecto:       Evaluating an Agroforestal Intervention in Rural Mexico: 
#                 Sembrando Vida
# Script:         cierre_agrícola.R
# Objetivo:       Limpiar y unir las bases de datos del cierre agrícola del 
#                 periodo 2018-2023.
#
# Autor:          Daniel Kelly
# Email:          djsanchez@colmex.mx
# 
# Fecha:          7 de Septiembre de 2024
#
# ______________________________________________________________________________

# PREAMBULO --------------------------------------------------------------------

setwd("/Users/danielkelly/Library/Mobile Documents/com~apple~CloudDocs/Tésis")

rm( list=ls() )

# librerias
library(tidyverse)
library(readxl)
library(openxlsx)

# utilidades
source('~/Documents/Estadística/R/Utilities/lineplot.R')
source('~/Documents/Estadística/R/Utilities/period.R')
source('~/Documents/Estadística/R/Utilities/myacf.R')
source('~/Documents/Estadística/R/Utilities/mypacf.R')
source('~/Documents/Estadística/R/Utilities/colores.R')
source('~/Documents/Estadística/R/Utilities/grangertest.R') 
source('~/Documents/Estadística/R/Utilities/remove_accents.R')

# CÓDIGO -----------------------------------------------------------------------

# Definir años y meses:
years <- 2018:2023
months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")

# Importar catálogo de municipios:
catalogo <- read_xlsx("Bases de Datos/Finales/Catálogo de Municipios.xlsx")

# Loop para importar archivos:
for (k in c("Frijol", "Maiz")) {
  for (u in c("Riego", "Temporal")) {
    for (i in years) {
      for (j in months) {
      
        cat("=")
    
        nombre <- paste(j,i,k,u, sep = "_")
    
        assign(nombre, as.data.frame(read.xlsx(file.path("Bases de Datos", "Cierre Agrícola", "Por Cultivo", k, u, paste(paste(i, j, sep="-"), "xlsx", sep=".")))[,2:8]  %>%
             filter( !is.na(X2) & !is.na(X3) & !is.na(X4) & !is.na(X5) & !is.na(X6) & !is.na(X7) & !is.na(X8) ) %>%
             rename(
               Entidad=X2,
               Municipio=X3,
               Sembrada=X4,
               Cosechada=X5,
               Siniestrada=X6,
               Produccion=X7,
               Rendimiento=X8
             ) %>%
             group_by(Municipio, Entidad) %>%
             summarise(
               Municipio=as.character(remove_accents(tolower(first(Municipio)))),
               Entidad=as.character(remove_accents(tolower(first(Entidad)))),
               Sembrada=sum( as.numeric(Sembrada) ),
               Cosechada=sum( as.numeric(Cosechada) ),
               Siniestrada=sum( as.numeric(Siniestrada) ),
               Produccion=sum( as.numeric(Produccion) ),
               Rendimiento=sum( as.numeric(Rendimiento) )
             ) %>%
             rename(
               n_ent=Entidad,
               n_mun=Municipio,
               sembrada=Sembrada,
               cosechada=Cosechada,
               siniestrada=Siniestrada,
               produccion=Produccion,
               rendimiento=Rendimiento
              ) %>%
             right_join(catalogo, by=c("n_ent", "n_mun")) %>%
             mutate(fecha=(as.Date(paste(i, which(months==j), "01", sep="-"), format="%Y-%m-%d"))) %>%
             mutate(
               cultivo=tolower(k),
               regimen=tolower(u)
               ) 
           ))
   }
  }
 }
}

# Incorporar observaciones de Riego y Temporal en la misma base:
nombres <- character()

for (k in c("Frijol", "Maiz")) { 
  for (i in years) { 
    for (j in months) {
      
      nombre <- paste0(j, "_", i, "_", k)
      nombres <- c(nombres, nombre)
      
      riego_df <- get(paste0(j, "_", i, "_", k, "_Riego")) %>%
        select(-regimen)
      
      temporal_df <- get(paste0(j, "_", i, "_", k, "_Temporal")) %>%
        select(-regimen)
      
      joined_df <- full_join(riego_df, temporal_df, 
                             by = c("n_mun", "n_ent", "cve_ent", "cve_mun", "cve", "fecha", "cultivo"), 
                             suffix = c(".riego", ".temporal"))
      
      for (u in c("sembrada", "cosechada", "siniestrada", "produccion", "rendimiento")) { 
        for (o in c("riego", "temporal")) {
          
          var <- paste0(u, ".", o)
          
          joined_df <- joined_df %>% 
            mutate(across(starts_with(var), ~ replace_na(.x, 0)))
          
        }
        
        joined_df <- joined_df %>% 
          mutate(!!paste0(u, ".tot") := 
                   coalesce(.data[[paste0(u, ".riego")]], 0) + coalesce(.data[[paste0(u, ".temporal")]], 0))
        
      }
      
      assign(nombre, joined_df)
    }
  }
}




# Unir bases de datos:
cierre_agrícola <- do.call(rbind, lapply(nombres, get)) %>%
  mutate(
    frijol=ifelse(cultivo=="frijol", 1, 0),
    maiz=ifelse(cultivo=="maiz", 1, 0)
    ) %>%
  dplyr::select(-cultivo)

# Guardar bases de datos:
write.xlsx(cierre_agrícola, "Bases de Datos/Finales/Cierre Agricola.xlsx")

