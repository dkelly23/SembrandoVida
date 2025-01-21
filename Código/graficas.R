# ______________________________________________________________________________
#
# Proyecto:       Evaluating an Agroforestal Intervention in Rural Mexico: 
#                 Sembrando Vida
# Script:         graficas.R
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

## Datos -----------------------------------------------------------------------

base_final <- read_xlsx("Bases de Datos/Finales/base_final.xlsx")
ran <- read_xlsx("Bases de Datos/Finales/Registro Agrario Nacional.xlsx")
siap <- read_xlsx("Bases de Datos/Finales/Cierre Agricola.xlsx")
pub <- read_xlsx("Bases de Datos/Finales/Sembrando Vida.xlsx")
pub_faltante <- read_xlsx("Bases de Datos/Finales/Sembrando Vida Faltantes.xlsx")
catalogo <- read_xlsx("Bases de Datos/Finales/Catálogo de Municipios.xlsx")
siap_filtrado <- read_xlsx("Bases de Datos/Finales/siap_filtrado.xlsx")


## Script ----------------------------------------------------------------------

fechas <- as.data.frame(seq(as.Date("2019-01-01"), as.Date("2023-12-01"), by="month"))
names(fechas) <- "fecha"

pub_año <- pub %>%
  group_by(fecha) %>%
  summarise(beneficiarios=sum(beneficiarios)) %>%
  right_join(fechas, by="fecha") %>%
  arrange(fecha) %>%
  mutate(fecha=as.Date(fecha))

pub_f_año <- pub_faltante %>%
  group_by(fecha) %>%
  summarise(beneficiarios=sum(beneficiarios)) %>%
  right_join(fechas, by="fecha") %>%
  arrange(fecha) %>%
  mutate(fecha=as.Date(fecha))

# Grafica de evolución:
par( mar=c(2,3,1,1), mfrow=c(1, 1), family="Times")
cxlims <- c(min(pub_año$fecha), max(pub_año$fecha))
plot(pub_año$fecha , pub_año$beneficiarios , t="l", col="black" , lwd=1.5 , axes=F, xlim=cxlims, ylim=c(0, 500000) , xaxs="r", main="", cex.main=1, ylab="")
points(pub_f_año$fecha, pub_f_año$beneficiarios, type="p", lwd=1.5, pch=1, cex=1)
title(ylab="Number of Beneficiaries", line=2, cex.lab=1.1)
xticks <- axis.Date(1, x=pub_año$fecha , at=seq(pub_año$fecha[1], as.Date("2024-01-01"), by="year") , lwd=0, lwd.tick=1, tck=0.02)
yticks <- axis(2 , lwd=0, lwd.tick=1, tck=0.02, at=seq(0, 500000, by=100000), labels=c("0", "100,000", "200,000", "300,000", "400,000", "500,000"))
axis.Date(3, x=fechas, at=seq(pub_año$fecha[1], as.Date("2024-01-01"), by="year"), lwd=0, lwd.tick=1, tck=0.02, lab=F)
axis(4, lwd=0, lwd.tick=1, tck=0.02, lab=F, at=seq(100000, 500000, by=100000))
abline( h=yticks , lty=3 , lw=0.7)
abline( v=xticks , lty=3 , lw=0.7)
box()

# Histograma:
par(mar=c(2,2,0.1,0.1), mfrow=c(1, 1), family="Times")
hist(pub$edad, freq=FALSE, breaks=50, col="firebrick", main=NULL)
lines(density(pub$edad), lwd=2.5, col="black")
box()
grid(col = "gray80")
legend("topleft", "Kernel Density Estimate", lwd=3, col="black", bty="n")

# Data para mapas:

## Beneficiarios:
benef_mun <- pub %>%
  filter(fecha==as.Date("2023-12-01")) 

write.xlsx(benef_mun, "Bases de Datos/Graficar/benef_mun.xlsx")


## Propiedad de la tierra:
ejido_mun <- base_final %>%
  group_by(cve) %>%
  summarise(ejido=first(ejido))

write.xlsx(ejido_mun, "Bases de Datos/Graficar/ejido_mun.xlsx")
