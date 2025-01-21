# ______________________________________________________________________________
#
# Proyecto:       Evaluating an Agroforestry Intervention in Rural Mexico:
#                 Sembrando Vida
# Script:         graficas_mun.R
# Objetivo:       Visualizar el efecto del programa en los municipios tratados.
#
# Autor:          Daniel Kelly
# Email:          djsanchez@colmex.mx
# 
# Fecha:          06 de October de 2024
#
# ______________________________________________________________________________

# PREAMBULO --------------------------------------------------------------------

setwd("/Users/danielkelly/Library/Mobile Documents/com~apple~CloudDocs/Tésis")

rm( list=ls() )

# librerias
library(tidyverse)
library(readxl)
library(openxlsx)
library(stringr)

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

base <- read.xlsx("Bases de Datos/Finales/base_final.xlsx") %>%
  mutate(fecha=as.Date(fecha, origin = "1899-12-30"))

maiz <- read.xlsx("Bases de Datos/Finales/maiz_final.xlsx") %>%
  mutate(fecha=as.Date(fecha, origin = "1899-12-30"))

frijol <- read.xlsx("Bases de Datos/Finales/frijol_final.xlsx") %>%
  mutate(fecha=as.Date(fecha, origin = "1899-12-30")) 

municipios <- read_excel("Bases de Datos/Finales/Catálogo de Municipios.xlsx")

fechas <- seq(as.Date("2018-01-01"), as.Date("2023-12-01"), by="month")

## Script ----------------------------------------------------------------------

cultivos <- c("maiz", "frijol")

tipos <- c("sembrada.tot", "cosechada.tot")

colores <- c(
  "firebrick",
  "forestgreen",
  "dodgerblue",
  "goldenrod"
)

for (m in municipios$cve) {
  
  cat('yacasi')
  
  pdf(paste0("/Users/danielkelly/Library/Mobile Documents/com~apple~CloudDocs/Tésis/Gráficas/Municipios/", m, ".pdf"), width = 6, height = 4) 
  df <- filter(base, cve==m)
  lim <- c(0,(max(df$sembrada.tot, df$cosechada.tot)+0.2*max(df$sembrada.tot, df$cosechada.tot)))
  title <- paste(
    paste(str_to_title(municipios$n_mun[which(municipios$cve==m)]), 
          str_to_title(municipios$n_ent[which(municipios$cve==m)]), sep=", "), 
    paste0("(", m, ")"), 
           sep=" ")
  entrada <- mutate(df, tratado=ifelse(beneficiarios!=0, 1, 0))[,34]
  
  if (any(entrada!=0)) {
    sembrado <- as.Date(fechas[min(which(entrada==1))])
  }
  
  lineplot(seq(as.Date("2018-01-01"), as.Date("2024-01-01"), by="month"), rep(-100000, 73), "black", title, "year", ylim=lim)
  
  for (s in 1:length(cultivos)) {
    df2 <- df %>% filter(frijol==(s-1))

      lines( df2$fecha, df2$sembrada.tot, lwd=2, col=colores[s], type="l" )
      lines( df2$fecha, df2$sembrada.tot, lwd=2, col=colores[s], type="p", pch=16 )
      
      lines( df2$fecha, df2$cosechada.tot, lwd=2, col=colores[s+2], type="l" )
      lines( df2$fecha, df2$cosechada.tot, lwd=2, col=colores[s+2], type="p", pch=16 )

  }
  
  if (any(entrada!=0)) {
    abline(v=sembrado, lwd=2, lty=2, col="red")
  }
  
  legend("topleft", cex=0.7,
         legend=c("Maiz-Sembrada", "Maiz-Cosechada", "Frijol-Sembrada", "Frijol-Cosechada"), 
         col=c("firebrick", "dodgerblue", "forestgreen", "goldenrod"),
         lty=1,
         lwd=3)
  dev.off()
}
