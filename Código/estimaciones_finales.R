# ______________________________________________________________________________
#
# Proyecto:       Evaluating an Agroforestal Intervention in Rural Mexico:
#                 Sembrando Vida.
#                 
# Script:         estimaciones_finales.R
# Objetivo:       Estimaciones finales.
#
# Autor:          Daniel Kelly
# Email:          djsanchez@colmex.mx
# 
# Fecha:          16 de Noviembre de 2024.
#
# ______________________________________________________________________________

# PREAMBULO ____________________________________________________________________

#setwd("")

#rm( list=ls() )
#.rs.restartR()

# librerias
pacman::p_load(tidyverse, readxl, showtext, readxl, openxlsx, eventstudyr,
               fixest, tseries, stargazer)

# idioma
Sys.setlocale("LC_ALL", "es_ES.UTF-8")
Sys.setenv(LANG = "es_MX.UTF-8")

# utilidades
font_add_google("PT Serif")
font_add_google("EB Garamond")
showtext_auto() 
options(scipen = 999)

# funciones
#source("/Users/danielkelly/Documents/Estadística/R/Utilities/mex_gob_gg/ggmx/R/graph_tsline.R")
#source("/Users/danielkelly/Documents/Estadística/R/Utilities/mex_gob_gg/ggmx/R/graph_tsbar.R")

# opciones
#create_project_folders()
#file.edit("~/.Rprofile")
#file.edit("/Users/danielkelly/.config/rstudio/templates/default.R")


# CÓDIGO _______________________________________________________________________

# Data -------------------------------------------------------------------------

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

# Script -----------------------------------------------------------------------

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

## Detrending ------------------------------------------------------------------

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
  
  composicion <- case_when(
    get(paste("df", c, sep="_"))$produccion.temporal+get(paste("df", c, sep="_"))$produccion.riego != 0 ~ get(paste("df", c, sep="_"))$produccion.riego / (get(paste("df", c, sep="_"))$produccion.temporal+get(paste("df", c, sep="_"))$produccion.riego),
    get(paste("df", c, sep="_"))$produccion.temporal+get(paste("df", c, sep="_"))$produccion.riego == 0 ~ 0
  )
  
  fechas <-  get(paste("df", c, sep="_"))$fecha
  cve <-  get(paste("df", c, sep="_"))$cve
  
  assign(paste("df_detrend", c, sep="_"), bind_cols(composicion=composicion, cve=cve, fecha=fechas) %>%
           left_join(indice, by="fecha") %>%
           mutate(mes=month(fecha),
                  año=year(fecha)) %>%
           left_join(matriz.meses, by="mes")
  )
  
  formula1 <- as.formula(paste("composicion ~ ", paste(m, collapse="+"), " | cve + año"))
  
  detrend <- feols(
    data=get(paste("df_detrend", c, sep="_")),
    fml=get(paste0("formula1")),
    se="cluster",
    panel.id="cve"
   )
   assign(paste("res", c, sep="_"), detrend$residuals)
   assign(paste("model", c, sep="_"), detrend)
  
  
  assign(paste("residuos", c, sep="_"), bind_cols(residuos=get(paste("res", c, sep="_"))))
  
  assign(paste("df_est", c, sep="_"), bind_cols(get(paste("df", c, sep="_")), get(paste("residuos", c, sep="_"))) %>% mutate(lbenef=log(beneficiarios)))
}

# Event Studies ----------------------------------------------------------------
frijol_comp <- EventStudy(
  estimator="OLS",
  data=df_est_frijol,
  outcomevar="residuos",
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
EventStudyPlot(frijol_comp, sup=0.1)


maiz_comp <- EventStudy(
  estimator="OLS",
  data=df_est_maiz,
  outcomevar="residuos",
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
EventStudyPlot(maiz_comp, sup=0.1)


save(maiz_comp, frijol_comp, file="Resultados/modelos_composicion.RData")

# Gráficas ---------------------------------------------------------------------

es_plot <- function(x, main, ylim, labs, at, conf_level=0.95, family) {
  
  lm_robust_object <- x$output

  summary_obj <- summary(lm_robust_object)
  coef_est <- summary_obj$coefficients[, "Estimate"]
  std_err <- summary_obj$coefficients[, "Std. Error"]
  df <- lm_robust_object$df.residual

  alpha <- 1 - conf_level
  t_crit <- qt(1 - alpha / 2, df = df)
  
  lower_bound <- coef_est - t_crit * std_err
  upper_bound <- coef_est + t_crit * std_err
  
  coefs <- c(as.numeric(x$output$coefficients[1:13]),0,
             as.numeric(x$output$coefficients[14:26]))
  lower <- c(as.numeric(lower_bound[1:13]),0,
             as.numeric(lower_bound[14:26]))
  upper <- c(as.numeric(upper_bound[1:13]),0,
             as.numeric(upper_bound[14:26]))
  
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

# Composición del Frijol
es_plot(
  frijol_comp,
  "",
  c(-0.0002,0.0002),
  c("-0.0002", "-0.0001", "0", "0.0001", "0.0002"),
  c(-0.0002, -0.0001, 0, 0.0001, 0.0002),
  conf_level=0.999,
  family="serif"
)
dev.print(png, "Gráficas/composicion_frijol.png", width = 3024, height = 1964, res = 400)
dev.off()

es_plot(
  maiz_comp,
  "",
  c(-0.0002,0.0002),
  c("-0.0002", "-0.0001", "0", "0.0001", "0.0002"),
  c(-0.0002, -0.0001, 0, 0.0001, 0.0002),
  "serif",
  conf_level=0.999
)
dev.print(png, "Gráficas/composicion_maiz.png", width = 3024, height = 1964, res = 400)
dev.off()

