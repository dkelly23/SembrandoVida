# ______________________________________________________________________________
#
# Proyecto:       Evaluating an Agroforestal Intervention in Rural Mexico: 
#                 Sembrando Vida
# Script:         catálogo_mun.R
# Objetivo:       Generar catálogo de municipios.  
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

source('~/Documents/Estadística/R/Utilities/remove_accents.R')

# CÓDIGO -----------------------------------------------------------------------

# Importar catálogo de municipios y entidades:
municipios <- read_xlsx("Bases de Datos/Catalogo de Municipios/Catálogo de Municipios.xlsx") %>%
  mutate(nombre=remove_accents(tolower(nombre))) %>%
  rename(cve=CVE_MUNICIPIO,
         cve_ent=estado,
         cve_mun=municipio,
         n_mun=nombre
  )

entidades <- read_xlsx("Bases de Datos/Catálogo de Municipios/Catálogo de Entidades.xlsx") %>%
  mutate(nombre_entidad=remove_accents(tolower(nombre_entidad))) %>%
  rename(
    cve_ent=CVE_ENTIDAD,
    n_ent=nombre_entidad
  )

catalogo <- left_join(municipios, entidades, by = "cve_ent") %>%
  mutate(
    cve_ent=as.numeric(cve_ent),
    cve_mun=as.numeric(cve_mun)
  )


# Guardar base:
write.xlsx(catalogo, "Bases de Datos/Finales/Catálogo de Municipios.xlsx")
