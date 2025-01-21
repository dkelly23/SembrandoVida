# ______________________________________________________________________________
#
# Proyecto:       Evaluating an Agroforestal Intervention in Rural Mexico: 
#                 Sembrando Vida
# Script:         estimaciones_preeliminares.R
# Objetivo:       Correr estimaciones preeliminares.
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

## Data ------------------------------------------------------------------------

indice <- as.data.frame(seq(as.Date("2018-01-01"), as.Date("2023-12-01"), by="month")) %>%
  mutate(t=1:n())
names(indice) <- c("fecha", "t")

base <- read.xlsx("Bases de Datos/Finales/base_final.xlsx") %>%
  mutate(fecha=as.Date(fecha, origin = "1899-12-30")) %>%
  left_join(indice, by="fecha")

maiz <- read.xlsx("Bases de Datos/Finales/maiz_final.xlsx") %>%
  mutate(fecha=as.Date(fecha, origin = "1899-12-30")) %>%
  left_join(indice, by="fecha")

frijol <- read.xlsx("Bases de Datos/Finales/frijol_final.xlsx") %>%
  mutate(fecha=as.Date(fecha, origin = "1899-12-30")) %>%
  left_join(indice, by="fecha")

## Script ----------------------------------------------------------------------

# Generamos indicadoras de periodo

for (b in c("frijol", "maiz")) {
  
  assign( paste(substr(b,1,1), "final", sep="_"), get(b) )
  
  for (i in unique(get(b)$cve)) {
    
    data <- get(paste(substr(b,1,1), "final", sep="_")) %>% filter(cve==i)
    
  }
}

