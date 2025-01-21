# ______________________________________________________________________________
#
# Proyecto:       Evaluating an Agroforestal Intervention in Rural Mexico: 
#                 Sembrando Vida
# Script:         descriptivas.R
# Objetivo:       Estadísticas descriptivas del programa.  
#
# Autor:          Daniel Kelly
# Email:          djsanchez@colmex.mx
# 
# Fecha:          8 de Septiembre de 2024
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

## Datos -----------------------------------------------------------------------

base_final <- read_xlsx("Bases de Datos/Finales/base_final.xlsx")
ran <- read_xlsx("Bases de Datos/Finales/Registro Agrario Nacional.xlsx")
siap <- read_xlsx("Bases de Datos/Finales/Cierre Agricola.xlsx")
pub <- read_xlsx("Bases de Datos/Finales/Sembrando Vida.xlsx")
catalogo <- read_xlsx("Bases de Datos/Finales/Catálogo de Municipios.xlsx")
siap_filtrado <- read_xlsx("Bases de Datos/Finales/siap_filtrado.xlsx")


## Script ----------------------------------------------------------------------
