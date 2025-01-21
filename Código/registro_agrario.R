# ______________________________________________________________________________
#
# Proyecto:       Evaluating an Agroforestal Intervention in Rural Mexico: 
#                 Sembrando Vida
# Script:         registro_agrario.R
# Objetivo:       Limpiar la base de datos del Registro Agrario Nacional.  
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

# Importar catálogo de municipios:
catalogo <- read_xlsx("Bases de Datos/Finales/Catálogo de Municipios.xlsx")

entidades <- read_xlsx("Bases de Datos/Catálogo de Municipios/Catálogo de Entidades.xlsx") %>% 
  mutate(CVE_ENTIDAD=as.numeric(CVE_ENTIDAD))

# Importar base de datos del RAN:
ran <- read_xlsx("Bases de Datos/Registro Agrícola Nacional/Catálogo de Núcleos Agrarios.xlsx")[,1:11]

## Script ----------------------------------------------------------------------

# Nombres del archivo
names(ran) <- c("cve", "cve_ent", "x", "cve_mun", "y", "z", "tipo", "o", "r", "i", "superficie")

# Transformaciones
ran <- ran %>%
  dplyr::select(-c(x,y,z,o,r,i)) %>%
  filter(tipo=="EJIDO") %>%
  group_by(cve) %>%
  summarise(
    cve_ent=first(cve_ent),
    cve_mun=first(cve_mun),
    superficie=mean(superficie)
  )

# Resumen por municipio
sum_ran <- ran %>%
  group_by(cve_mun, cve_ent) %>%
  summarise(
    superficie=mean(superficie),
    numero=n()
  ) %>%
  left_join(catalogo, by=c("cve_mun", "cve_ent"))
  

# Resumen por entidad
sum_ran_ent <- ran %>% 
  group_by(cve_ent) %>% 
  summarise(
    superficie=sum(superficie),
    numero=n()
  ) %>% 
  left_join(
    entidades, by=c("cve_ent"="CVE_ENTIDAD")
  )

# Guardar bases de datos:
write.xlsx(sum_ran, "Bases de Datos/Finales/Registro Agrario Nacional.xlsx")


# Gráfica ----------------------------------------------------------------------

states <- c("Chihuahua", "Sonora", "Coahuila", "Durango", "Oaxaca", 
            "Tamaulipas", "Jalisco", "Zacatecas", "Baja California Sur", "Chiapas", 
            "Baja California", "Veracruz", "Nuevo León", "Guerrero", "San Luis Potosí", 
            "Michoacán", "Campeche", "Sinaloa", "Quintana Roo", "Yucatán", "Puebla", 
            "Guanajuato", "Nayarit", "Tabasco", "México", "Hidalgo", "Querétaro", 
            "Colima", "Aguascalientes", "Morelos", "Tlaxcala", "Ciudad de México")
# Area values
areas <- c(247412, 180608, 151595, 123364, 93758, 80249, 78597, 75275, 74608, 
           73311, 73200, 71824, 64156, 63597, 61138, 58599, 57516, 57370, 
           44825, 39851, 34309, 30607, 28095, 24731, 22351, 20821, 11691, 
           5784, 5616, 4879, 3997, 1495) * 100

# Creating a data frame
df <- data.frame(entidad = states, area = areas)

# Proporción ejidal
proporcion <- sum_ran_ent %>% 
  left_join(df, by=c("nombre_entidad"="entidad")) %>% 
  mutate(proporcion=((superficie)/area)*100)


proporcion <- proporcion[order(proporcion$proporcion, decreasing = F), ]

# Crear gráfico de barras horizontal con los datos ordenados
par(family="Times", mar=c(0,8,0,0))
posiciones <- barplot(proporcion$proporcion, 
                      names.arg = proporcion$nombre_entidad, 
                      horiz = TRUE, 
                      col = "#235b4e", 
                      main = NULL,
                      xlab = NULL, 
                      cex.names = 0.9,  # Ajustar tamaño de las etiquetas del eje y
                      las = 1,
                      xlim=c(0,75),
                      axes=F,
                      font=2)

# Agregar etiquetas con valores formateados al lado de las barras
text(proporcion$proporcion + 1, posiciones-0.15, 
     labels = paste0(formattable::comma(round(proporcion$proporcion, 1)), "%")
     , 
     pos = 4, cex = 0.9, col = "black", font=2)

