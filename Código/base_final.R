# ______________________________________________________________________________
#
# Proyecto:       Evaluating an Agroforestal Intervention in Rural Mexico: 
#                 Sembrando Vida
# Script:         base_final.R
# Objetivo:       Limpiar la base de datos del Registro Agrario Nacional.  
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

ran <- read_xlsx("Bases de Datos/Finales/Registro Agrario Nacional.xlsx")
siap <- read_xlsx("Bases de Datos/Finales/Cierre Agricola.xlsx")
pub_original <- read_xlsx("Bases de Datos/Finales/Sembrando Vida.xlsx")
pub_faltantes <- read_xlsx("Bases de Datos/Finales/Sembrando Vida Faltantes.xlsx")
lluvia <- read_xlsx("Bases de Datos/Finales/Lluvia.xlsx")
temperatura <- read_xlsx("Bases de Datos/Finales/Temperatura.xlsx")

extension <- read.csv("Bases de Datos/División Política Municipal/División Política Municipal.csv") %>%
  rename(
    cve_ent=CVE_ENT,
    cve_mun=CVE_MUN,
    area=AREA
         ) %>%
  dplyr::select(cve_ent, cve_mun, area)

catalogo <- read_xlsx("Bases de Datos/Finales/Catálogo de Municipios.xlsx")


## Script ----------------------------------------------------------------------

# Unir PUB y PUB con periodos faltantes:
pub <- bind_rows(pub_original, pub_faltantes) %>%
  arrange(desc(cve))
write.xlsx(pub, "Bases de Datos/Finales/Sembrando Vida Final.xlsx")


# Unir municipios con extensión territorial:
catalogo <- left_join(catalogo, extension, by=c("cve_ent", "cve_mun"))

# Listado de municipios dónde el programa existe al menos en un periodo:
municipios <- unique(catalogo$cve)

# Objeto con fechas:
fechas <- seq(as.Date("2018-01-01"), as.Date("2023-12-01"), by="month")

# Estructura de la base final:
base_final <- expand.grid(municipios, fechas) %>%
  as.data.frame() 

names(base_final) <- c("cve", "fecha")
  
base_final <- arrange(base_final, cve, fecha)


# Unir catalogo y extensión territorial:
base_final <- base_final %>%
  left_join(catalogo, by="cve") %>%
  mutate(area=area*100) 

# Unir catálogo de núcleos agrícolas:
base_final <- base_final %>%
  left_join(ran, by=c("cve", "cve_mun", "cve_ent", "n_ent", "n_mun")) %>%
  mutate(
    superficie=ifelse(is.na(superficie), 0 , superficie),
    numero=ifelse(is.na(numero), 0, numero)
    )

# Unir padron de beneficiarios:
base_final <- base_final %>%
  left_join(pub, by=c("cve", "cve_mun", "cve_ent", "n_ent", "n_mun", "fecha"))%>%
  mutate(
    edad=ifelse(is.na(edad), 0 , edad),
    sexo=ifelse(is.na(sexo), 0 , sexo),
    beneficio=ifelse(is.na(beneficio), 0 , beneficio),
    beneficiarios=ifelse(is.na(beneficiarios), 0 , beneficiarios)
  ) %>%
  mutate(ejido=superficie/area)

# Unir producción agrícola:
siap_filtrado <- siap %>%
  filter(cve %in% unique(pub$cve))

write.xlsx(siap_filtrado, "Bases de Datos/Finales/siap_filtrado.xlsx")

base_final <- siap %>%
  left_join(base_final, by=c("cve", "cve_mun", "cve_ent", "n_ent", "n_mun", "fecha"))


# Unir datos de clima y lluvia:
base_final <- base_final %>%
  left_join(lluvia, by=c("cve", "fecha")) %>%
  left_join(temperatura, by=c("cve", "fecha"))

### Precios --------------------------------------------------------------------
maiz <- read_xlsx("Bases de Datos/INPP/Maiz.xlsx") %>%
  mutate(
    fecha=seq(as.Date("1981-01-01"), as.Date("2024-07-01"), by="month"),
    ipp=Indice,
    frijol=0,
    maiz=1
  ) %>%
  dplyr::select(fecha, ipp)

frijol <- read_xlsx("Bases de Datos/INPP/Frijol.xlsx") %>%
  mutate(
    fecha=seq(as.Date("1981-01-01"), as.Date("2024-07-01"), by="month"),
    ipp=Indice,
    frijol=1,
    maiz=0
  ) %>%
  dplyr::select(fecha, ipp)


# Bases finales:
frijol_final <- base_final %>%
  filter(frijol==1) %>%
  left_join(frijol, by="fecha") %>%
  dplyr::select(-frijol, - maiz)

maiz_final <- base_final %>%
  filter(maiz==1) %>%
  left_join(maiz, by="fecha") %>%
  dplyr::select(-frijol, - maiz)


### Guardar --------------------------------------------------------------------
write.xlsx(base_final, "Bases de Datos/Finales/base_final.xlsx")
write.xlsx(frijol_final, "Bases de Datos/Finales/frijol_final.xlsx")
write.xlsx(maiz_final, "Bases de Datos/Finales/maiz_final.xlsx")

