# ______________________________________________________________________________
#
# Proyecto:       Tesis
#                 
# Script:         mapas.R
# Objetivo:       Generar Visualizaciones Geográficas de Sembrando Vida
#
# Autor:          Daniel Kelly
# Email:          djsanchez@colmex.mx
# 
# Fecha:          
#
# ______________________________________________________________________________

# PREAMBULO --------------------------------------------------------------------

.rs.restartR()
setwd("")
#create_project_folders()

rm( list=ls() )

# librerias
pacman::p_load(tidyverse, readxl, ggmx, showtext, sf)

# idioma
Sys.setlocale("LC_ALL", "es_ES.UTF-8")
Sys.setenv(LANG = "es_MX.UTF-8")

# utilidades
font_add_google("Montserrat")
showtext_auto() 
options(scipen = 999)


# CÓDIGO -----------------------------------------------------------------------

## Data ------------------------------------------------------------------------

municipios <- st_read("Bases de Datos/División Política Municipal/Proyección Cónica/mun21cw.shp")

beneficiarios <- read_xlsx("Bases de Datos/Graficar/benef_mun.xlsx")
ejidos <- read_xlsx("Bases de Datos/Graficar/ejido_mun.xlsx")

## Script ----------------------------------------------------------------------

mun_ej <- left_join(municipios, ejidos, by=c("CVEGEO"="cve")) %>% 
  mutate(
    ejido=ifelse(is.na(ejido), 0, ejido)
  )

mun_benef <- left_join(municipios, beneficiarios, by=c("CVEGEO"="cve")) %>% 
  mutate(
    beneficiarios=ifelse(is.na(beneficiarios), 0, beneficiarios)
  )

mapa <- ggplot() +
  geom_sf(
    data = mun_benef, 
    aes(fill = beneficiarios),  # Fill polygons based on the `beneficiarios` column
    size = 0.5, 
    color = "black"  # Set border color for polygons
  ) +
  scale_fill_gradient(
    low = "white", 
    high = "darkred", 
    name = "Beneficiarios"  # Add a label to the legend
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),  # Remove axis titles
    axis.text = element_blank(),   # Remove axis text
    axis.ticks = element_blank(),  # Remove axis ticks
    panel.grid = element_blank()   # Remove grid lines
  )

ggplot() +
  geom_sf(data = mun_ej, color = "black", aes(fill = beneficiarios)) +
  scale_fill_gradient(
    low = "white", 
    high = "darkred", 
    name = "Beneficiarios"  # Add a label to the legend
  ) +
  theme_minimal() 

#+
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    plot.title = element_blank(),
    legend.position = "none"
  )
  
  
hist(beneficiarios$beneficiarios, plot=F)
