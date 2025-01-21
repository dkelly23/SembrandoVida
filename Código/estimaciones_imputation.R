# ______________________________________________________________________________
#
# Proyecto:       Evaluating an Agroforestal Intervention in Rural Mexico:
#                 Sembrando Vida.
#                 
# Script:         estimaciones_imputation.R
# Objetivo:       Estimaciones con el imputation estimator de Borusyack.
#
# Autor:          Daniel Kelly
# Email:          djsanchez@colmex.mx
# 
# Fecha:          28 de Noviembre de 2024.
#
# ______________________________________________________________________________

# PREAMBULO ____________________________________________________________________

#setwd("")

# rm( list=ls() )
#.rs.restartR()

# librerias
pacman::p_load(tidyverse, readxl, showtext, readxl, openxlsx, eventstudyr,
               fixest, tseries, stargazer, didimputation, did2s)

# idioma
Sys.setlocale("LC_ALL", "es_ES.UTF-8")
Sys.setenv(LANG = "es_MX.UTF-8")

# utilidades
font_add_google("Montserrat")
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

maiz <- read_xlsx("Bases de Datos/maiz_est.xlsx")
frijol <- read_xlsx("Bases de Datos/frijol_est.xlsx")

# Script -----------------------------------------------------------------------

# Creación de Indicadoras de Tratamiento
maiz_dummy <- maiz %>% 
  arrange(cve, fecha) %>% 
  group_by(cve) %>% 
  mutate(
    id_tratamiento = min(which(beneficiarios!=0)),
    id_fecha = fecha[id_tratamiento]
  ) %>% 
  mutate(
    posicion=ifelse(id_tratamiento==t, 0, NA)
  ) %>%
  group_by(cve) %>%
  mutate(
    pos = which(posicion == 0), 
    posicion = case_when(
      row_number() < pos ~ -(pos - row_number()),
      row_number() == pos ~ 0,
      row_number() > pos ~ row_number() - pos
    )
  ) %>%
  ungroup() %>%
  select(-pos) 

frijol_dummy <- frijol %>% 
  arrange(cve, fecha) %>% 
  group_by(cve) %>% 
  mutate(
    id_tratamiento = min(which(beneficiarios!=0)),
    id_fecha = fecha[id_tratamiento]
  ) %>% 
  mutate(
    posicion=ifelse(id_tratamiento==t, 0, NA)
  ) %>%
  group_by(cve) %>%
  mutate(
    pos = which(posicion == 0), 
    posicion = case_when(
      row_number() < pos ~ -(pos - row_number()),
      row_number() == pos ~ 0,
      row_number() > pos ~ row_number() - pos
    )
  ) %>%
  ungroup() %>%
  select(-pos) 


# Imputation Estimator =========================================================

## Stage 1 ---------------------------------------------------------------------

df_0 <- maiz_dummy %>% filter(posicion < -13)

modelo1 <- feols(
  fml= total ~ lluvia + temp | fecha + cve,
  data=df_0,
  cluster=c("cve")
)

summary(modelo1)

fe_unidad <- data.frame(unit_fe=fixef(modelo1)$cve, cve=names(fixef(modelo1)$cv))
fe_tiempo <- data.frame(time_fe=fixef(modelo1)$fecha, fecha=names(fixef(modelo1)$fecha)) %>% 
  mutate(fecha=as.Date(fecha))

coef_lluvia <- modelo$coefficients[["lluvia"]]
coef_temp <- modelo$coefficients[["temp"]]

df_1 <- maiz_dummy %>% 
  filter(posicion >= -13) %>% 
  left_join(fe_unidad, by="cve") %>% 
  left_join(fe_tiempo, by="fecha") %>% 
  mutate(
    unit_fe=ifelse(is.na(unit_fe), 0, unit_fe),
    time_fe=ifelse(is.na(time_fe), 0, time_fe)
    ) %>% 
  mutate(
    y_0=
      unit_fe + time_fe
      + coef_lluvia * lluvia
      + coef_temp * temp
  ) %>% 
  mutate(
    tau_t = total - y_0
  )

coeficientes <- df_1 %>% 
  mutate(weights=ifelse(beneficiarios==0, 0, 1/beneficiarios)) %>% 
  filter(posicion %in% -13:13) %>% 
  mutate(posicion=as.character(posicion)) %>% 
  group_by(posicion) %>% 
  summarise(
    efecto=sum(tau_t * weights)
  )

plot(-13:13, coeficientes$efecto*-1, t="p")


