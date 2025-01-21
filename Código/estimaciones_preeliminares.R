# ______________________________________________________________________________
#
# Proyecto:       Evaluating an Agroforestal Intervention in Rural Mexico:
#                 Sembrando Vida.
#                 
# Script:         estimaciones_preeliminares.R
# Objetivo:       Estimaciones Preeliminares del Efecto de Sembrando Vida.
#
# Autor:          Daniel Kelly
# Email:          djsanchez@colmex.mx
# 
# Fecha:          6 de Octubre de 2024
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
library(tseries)
library(stargazer)

remotes::install_github("dkelly23/ggmx")
library(ggmx)

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

# Dummys por Entidad Federativa
entidades <- data.frame(factor(unique(base$cve_ent)))
names(entidades) <- "cve_ent"

matriz.entidades <- model.matrix(~cve_ent - 1, data=entidades) %>%
  bind_cols(data.frame(unique(base$cve_ent))) %>%
  rename(cve_ent=unique.base.cve_ent.)

n <- character()
for (i in entidades) {
  n <- c(n, paste("cve_ent", i , sep=""))
}

# Dummys por Mes
meses <- as.data.frame(factor(1:12))
names(meses) <- "mes"

matriz.meses <- model.matrix(~mes - 1, data=meses) %>%
  bind_cols(as.data.frame(1:12)) %>%
  rename(mes=`1:12`)

m <- character()
for (i in 1:12) {
  m <- c(m, paste("mes", i , sep=""))
}

### Detrending -----------------------------------------------------------------

cultivos <- c("maiz", "frijol")

for (c in cultivos) {
assign(paste("df", c, sep="_"), 
  get(c) %>%
  arrange(cve, fecha) %>% 
  group_by(cve) %>%
  mutate(
    tratamiento=ifelse(sum(beneficiarios)==0, 0, 1)
  ) %>%
  ungroup() %>%
  filter(tratamiento==1) %>%
  mutate(
    tratamiento=ifelse(beneficiarios!=0, 1, 0),
    lbenef=ifelse(beneficiarios==0, 0, log(beneficiarios))
  )
)

total <- get(paste("df", c, sep="_"))$rendimiento.tot
riego <-  get(paste("df", c, sep="_"))$rendimiento.riego
temporal <-  get(paste("df", c, sep="_"))$rendimiento.temporal
fechas <-  get(paste("df", c, sep="_"))$fecha
cve <-  get(paste("df", c, sep="_"))$cve

assign(paste("df_detrend", c, sep="_"), bind_cols(total=total, riego=riego, temporal=temporal, fecha=fechas,
                        cve=cve) %>%
  left_join(indice, by="fecha") %>%
  mutate(mes=month(fecha),
         año=year(fecha)) %>%
  left_join(matriz.meses, by="mes")
)

formula1 <- as.formula(paste("total ~ ", paste(m, collapse="+"), " | cve + año"))
formula2 <- as.formula(paste("riego ~ ", paste(m, collapse="+"), " | cve + año"))
formula3 <- as.formula(paste("temporal ~ ", paste(m, collapse="+"), " | cve + año"))

tipos <- c("total", "riego", "temporal")
for (i in 1:3) {
  detrend <- feols(
    data=get(paste("df_detrend", c, sep="_")),
    fml=get(paste0("formula", i)),
    se="cluster",
    panel.id="cve"
  )
  assign(paste("res", tipos[i], sep="_"), detrend$residuals)
  assign(paste("model", tipos[i], c, sep="_"), detrend)
}

assign(paste("residuos", c, sep="_"),
       bind_cols(total=res_total, riego=res_riego, temporal=res_temporal))

assign(paste("df_est", c, sep="_"), bind_cols(get(paste("df", c, sep="_")), get(paste("residuos", c, sep="_"))))
}


# Análisis de Efecto Fijo -----

fe_frijol <- data.frame(fe=fixef(model_total_frijol)$cve, cve=names(fixef(model_total_frijol)$cve)) %>% 
  arrange(desc(fe)) %>% 
  mutate(id=1:1139)

fe_maiz <- data.frame(fe=fixef(model_total_maiz)$cve, cve=names(fixef(model_total_maiz)$cve)) %>% 
  arrange(desc(fe)) %>% 
  mutate(id=1:1139)


# Unir con ejido 
maiz_ejido <- df_est_maiz %>% 
  distinct(
    cve, ejido, area
  ) %>% 
  mutate(
    cve=as.numeric(cve)
  ) %>% 
  left_join(
    mutate(fe_maiz, cve=as.numeric(cve)), by="cve"
  )

frijol_ejido <- df_est_frijol %>% 
  distinct(
    cve, ejido, area
  ) %>% 
  mutate(
    cve=as.numeric(cve)
  ) %>% 
  left_join(
    mutate(fe_frijol, cve=as.numeric(cve)), by="cve"
  )


# Regresiones de FE
reg_maiz <- lm(log(fe) ~ ejido + area, data=maiz_ejido)
summary(reg_maiz)

reg_frijol <- lm(log(fe) ~ ejido + area, data=frijol_ejido)
summary(reg_frijol)

stargazer(reg_maiz, reg_frijol, style="aer")


# Gráfica FE
par(mar=c(1,2.5,1,1), family="Times")
plot(1:1139, rep(NA, 1139), type = "n", ylim=c(0,15), axes=F, ylab = "", xlab = "")  # Replace 'your_data' with your actual data frame for the plot range

lines(
  fe_frijol$id,
  fe_frijol$fe,
  t="p",
  pch=16,
  col="#9f2241"
)

lines(
  fe_maiz$id,
  fe_maiz$fe,
  t="p",
  pch=16,
  col="#235b4e"
)

axis(2, font=2, lwd=3, cex.axis=1.4)
axis(1, labels=F, tck=0, at=c(-1000, 100000), lwd=3)
axis(2, labels=F, tck=0, at=c(-1000, 1000), lwd=3)

legend(
  "topright",                   
  legend = c("Maize", "Beans"),
  fill = c("#235b4e", "#9f2241"),
  cex = 1.4,
  bty = "n"                   
)

dev.print(png, "Gráficas/fe.png", width = 3024, height = 1964, res = 400)
dev.off()


# Histograma
par(mfrow=c(1,2), family="Times")

par(mar=c(2,2,0.8,0.8))
plot(density(fe_frijol$fe), axes=F, main="", xlab="", ylab="", lwd=3, col="#9f2241")
axis(1, lwd=2, font=2, cex.axis=1.2)
axis(2, lwd=2, font=2, cex.axis=1.2)
axis(2, lwd=2, tck=0, at=c(-1000,1000))
axis(1, lwd=2, tck=0, at=c(-1000,1000))

par(mar=c(2,2,0.8,0.8))
plot(density(fe_maiz$fe), axes=F, main="", xlab="", ylab="", lwd=3, col="#235b4e")
axis(1, lwd=2, font=2, cex.axis=1.2)
axis(2, lwd=2, font=2, cex.axis=1.2)
axis(2, lwd=2, tck=0, at=c(-1000,1000))
axis(1, lwd=2, tck=0, at=c(-1000,1000))

legend(
  "topright",                   
  legend = c("Maize", "Beans"),
  col = c("#235b4e", "#9f2241"),
  lwd=3,
  cex = 1.4,
  bty = "n"                   
)

dev.print(png, "Gráficas/fe_hist.png", width = 3024, height = 1964, res = 400)
dev.off()

### Estudios de Evento ---------------------------------------------------------

#### Frijol --------------------------------------------------------------------

# Rendimiento Total
frijol_tot <- EventStudy(
  estimator="OLS",
  data=df_est_frijol,
  outcomevar="total",
  policyvar="beneficiarios",
  idvar="cve",
  timevar="t",
  post=12,
  pre=0,
  normalize = -1,
  FE=FALSE,
  TFE=TRUE,
  controls=c("lluvia", "temp")
)
EventStudyPlot(frijol_tot, sup=0.1)


# Rendimiento Temporal
frijol_temp <- EventStudy(
  estimator="OLS",
  data=df_est_frijol,
  outcomevar="temporal",
  policyvar="beneficiarios",
  idvar="cve",
  timevar="t",
  post=12,
  pre=0,
  normalize = -1,
  FE=FALSE,
  TFE=TRUE,
  controls=c("lluvia", "temp")
)
EventStudyPlot(frijol_temp)


# Rendimiento Riego
frijol_riego <- EventStudy(
  estimator="OLS",
  data=df_est_frijol,
  outcomevar="riego",
  policyvar="beneficiarios",
  idvar="cve",
  timevar="t",
  post=12,
  pre=0,
  normalize = -1,
  FE=FALSE,
  TFE=TRUE,
  controls=c("lluvia", "temp")
)
EventStudyPlot(frijol_riego)


#### Maiz ----------------------------------------------------------------------

# Rendimiento Total
maiz_tot <- EventStudy(
  estimator="OLS",
  data=df_est_maiz,
  outcomevar="total",
  policyvar="beneficiarios",
  idvar="cve",
  timevar="t",
  post=12,
  pre=0,
  normalize = -1,
  FE=TRUE,
  TFE=TRUE,
  controls=c("lluvia", "temp")
)
EventStudyPlot(maiz_tot, sup=0.1)


# Rendimiento Temporal
maiz_temp <- EventStudy(
  estimator="OLS",
  data=df_est_maiz,
  outcomevar="temporal",
  policyvar="beneficiarios",
  idvar="cve",
  timevar="t",
  post=12,
  pre=0,
  normalize = -1,
  FE=FALSE,
  TFE=TRUE,
  controls=c("lluvia", "temp")
)
EventStudyPlot(maiz_temp)


# Rendimiento Riego
maiz_riego <- EventStudy(
  estimator="OLS",
  data=df_est_maiz,
  outcomevar="riego",
  policyvar="beneficiarios",
  idvar="cve",
  timevar="t",
  post=12,
  pre=0,
  normalize = -1,
  FE=FALSE,
  TFE=TRUE,
  controls=c("lluvia", "temp")
)
EventStudyPlot(maiz_riego)


#save(frijol_tot, maiz_tot, file="Resultados/modelos_productividad.RData")

## Graficas --------------------------------------------------------------------

es_plot <- function(x, main, ylim, labs, at, family) {
  coefs <- c(as.numeric(x$output$coefficients[1:13]),0,
             as.numeric(x$output$coefficients[14:26]))
  lower <- c(as.numeric(x$output$conf.low[1:13]),0,
             as.numeric(x$output$conf.low[14:26]))
  upper <- c(as.numeric(x$output$conf.high[1:13]),0,
             as.numeric(x$output$conf.high[14:26]))
  
  par( mar=c(2,2,1.5,0.1), mfrow=c(1, 1), family=family)
  plot(-13:13, coefs, t="p",  ylim=ylim, pch=16, axes=F, ylab = '', xlab='', main=main)
  abline(h=0, lwd=1, lty=2, col="red")
  arrows(-13:13, coefs, y1=upper, angle = 90, code = 2, length = 0.025, col = "black", lwd=2, lty=1)
  arrows(-13:13, coefs, y1=lower, angle = 90, code = 2, length = 0.025, col = "black", lwd=2, lty=1)
  xticks <- axis(1, at=-13:13, labels=-13:13, lwd=0, lwd.tick=2, tck=0.02, cex.axis=0.7, font=2)
  yticks <- axis(2 , lwd=0, lwd.tick=2, tck=0.02, cex.axis=1, at=at,
                 labels=labs, font=2)
  axis(1,at=c(-20,20), lwd=3)
  axis(2,at=c(-20,20), lwd=3)
  #box() 
}

es_plot(
  frijol_tot,
  "",
  c(-0.0002,0.0002),
  c("-0.0002", "-0.0001", "0", "0.0001", "0.0002"),
  c(-0.0002, -0.0001, 0, 0.0001, 0.0002),
  "serif"
)
dev.print(png, "Gráficas/productividad_frijol.png", width = 3024, height = 1964, res = 400)
dev.off()


es_plot(
  frijol_riego,
  "Productividad del Frijol - Riego",
  c(-0.0002,0.0002),
  c("-0.0002", "-0.0001", "0", "0.0001", "0.0002"),
  c(-0.0002, -0.0001, 0, 0.0001, 0.0002),
  "Times"
)

es_plot(
  frijol_temp,
  "Productividad del Frijol - Temporal",
  c(-0.0002,0.0002),
  c("-0.0002", "-0.0001", "0", "0.0001", "0.0002"),
  c(-0.0002, -0.0001, 0, 0.0001, 0.0002),
  "Times"
)

es_plot(
  maiz_tot,
  "",
  c(-0.001,0.001),
  c("-0.0005", "-0.001", "0", "0.0005", "0.001"),
  c(-0.0005, -0.001, 0, 0.0005, 0.001),
  "serif"
)
dev.print(png, "Gráficas/productividad_maiz.png", width = 3024, height = 1964, res = 400)
dev.off()

es_plot(
  maiz_riego,
  "Productividad del Maiz - Riego",
  c(-0.001,0.001),
  c("-0.0005", "-0.001", "0", "0.0005", "0.001"),
  c(-0.0005, -0.001, 0, 0.0005, 0.001),
  "Times"
)


es_plot(
  maiz_temp,
  "Productividad del Maiz - Temporal",
  c(-0.001,0.001),
  c("-0.0005", "-0.001", "0", "0.0005", "0.001"),
  c(-0.0005, -0.001, 0, 0.0005, 0.001),
  "Times"
)


## Exportar DF -----------------------------------------------------------------
write.xlsx(df_est_maiz, "Bases de Datos/maiz_est.xlsx")
write.xlsx(df_est_frijol, "Bases de Datos/frijol_est.xlsx")