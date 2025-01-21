# ______________________________________________________________________________
#
# Proyecto:       Evaluating an Agroforestal Intervention in Rural Mexico: 
#                 Sembrando Vida
# Script:         sembrando_vida.R
# Objetivo:       Limpiar y unir las bases de datos del Padron de Beneficiarios  
#                 del periodo 2019-2023.
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

## Datos -----------------------------------------------------------------------

# Definir años y meses:
years <- 2019:2023
months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")


# Importar catálogo de municipios:
catalogo <- read_xlsx("Bases de Datos/Finales/Catálogo de Municipios.xlsx")

## Script ----------------------------------------------------------------------

# Crear objeto con los nombres de los DF's:
bases <- character()
for (y in years) {
  for (m in months) {
    bases <- c(bases, paste(y, m, sep="-"))
  }
}

# Objeto con los años que hacen falta en la base de datos:
faltantes <- c("2019-01", "2019-03", "2020-01", "2021-01", "2021-04", "2021-05")
bases <- setdiff(bases, faltantes)

# Loop para importar archivos:
for (k in bases) {
  cat('.')
  año <- substr(k, 1, 4)  
  mes <- substr(k, 6, 7)  
  name <- paste("pub", año, mes, sep="_")  
  
  df <- as.data.frame(read.xlsx(file.path("Bases de Datos", "Sembrando Vida", "Mensual", año, paste(paste(año, mes, sep="-"), "xlsx", sep="."))))

  df <- df[1:(nrow(df)-2), 2:ncol(df)] %>%
    mutate(
      CVE_MUNICIPIO=as.character(CVE_MUNICIPIO),
      cve=ifelse(nchar(CVE_MUNICIPIO)==4, paste("0", CVE_MUNICIPIO, sep=""), CVE_MUNICIPIO),
      cve_mun=as.numeric(substr(cve, 3, 5)),
      sexo=ifelse(SEXO=="MASCULINO", 1, 0),
      alta=as.Date(FECHA.ALTA, origin = "1899-12-30"),
      fecha=as.Date(paste(año, mes, "01", sep="-")),
      nombre=paste(tolower(remove_accents(PRIMER.APELLIDO)), tolower(remove_accents(SEGUNDO.APELLIDO)), tolower(remove_accents(NOMBRE))),
      edad=as.numeric(EDAD)
    ) %>%
    rename(
      cve_ent=CVE_ENTIDAD,
      beneficio=IMPORTE.BENEFICIO
    ) %>%
    dplyr::select(cve_ent, cve_mun, cve, nombre, edad, sexo, alta, fecha, beneficio)

  assign(name, df)
  
  df <- df %>%
    group_by(cve) %>%
    summarise(
      cve_ent=first(cve_ent),
      cve_mun=first(cve_mun),
      edad=mean(edad, na.rm = TRUE),
      sexo=mean(sexo),
      fecha=first(fecha),
      beneficio=mean(beneficio),
      beneficiarios=n()
    ) %>%
    right_join(catalogo, by=c("cve_ent", "cve_mun", "cve"))
  
  assign(paste("sum", name, sep="_"), df)
}

# Crear objeto con los nombres de los DF's modificados:
bases_sum <- character()
for (y in years) {
  for (m in months) {
    bases_sum <- c(bases_sum, paste("sum_pub", y, m, sep="_"))
  }
}

faltantes_sum <- c("sum_pub_2019_01", "sum_pub_2019_03", "sum_pub_2020_01", "sum_pub_2021_01", "sum_pub_2021_04", "sum_pub_2021_05")
bases_sum <- setdiff(bases_sum, faltantes_sum)

# Unir bases de datos:
sembrando_vida <- do.call(rbind, lapply(bases_sum, get)) %>%
  filter(!is.na(beneficiarios))

# Guardar bases de datos:
write.xlsx(sembrando_vida, "Bases de Datos/Finales/Sembrando Vida.xlsx")


