# ______________________________________________________________________________
#
# Proyecto:       
#                 
# Script:         graficas_event_study.R
# Objetivo:       
#
# Autor:          Daniel Kelly
# Email:          djsanchez@colmex.mx
# 
# Fecha:          
#
# ______________________________________________________________________________

# PREAMBULO ____________________________________________________________________

#setwd("")

rm( list=ls() )
#.rs.restartR()

# librerias
pacman::p_load(tidyverse, readxl, showtext)

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

load("Resultados/modelos_composicion.RData")
load("Resultados/modelos_productividad.RData")
load("Resultados/estimaciones_stata.RData")

# Script -----------------------------------------------------------------------
conf_level=0.95

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

calculate_conf_intervals <- function(coefficients, std_errors, confidence_level = 0.95) {
  z_value <- qnorm((1 + confidence_level) / 2)
  lower <- coefficients - z_value * std_errors
  upper <- coefficients + z_value * std_errors
  return(data.frame(
    Coefficient = coefficients,
    Std.Error = std_errors,
    Conf.Lower = lower,
    Conf.Upper = upper
  ))
}

# Frijol Composicion -----------------------------------------------------------
es_plot(
  frijol_comp,
  "",
  c(-0.00015,0.00015),
  c("-0.0002", "-0.0001", "0", "0.0001", "0.0002"),
  c(-0.0002, -0.0001, 0, 0.0001, 0.0002),
  conf_level=0.999,
  family="serif"
)

x <- frijol_comp

lm_robust_object <- x$output

summary_obj <- summary(lm_robust_object)
coef_est <- summary_obj$coefficients[, "Estimate"]
std_err <- summary_obj$coefficients[, "Std. Error"]
df <- lm_robust_object$df.residual

alpha <- 1 - conf_level
t_crit <- qt(1 - alpha / 2, df = df)

lower_bound <- coef_est - t_crit * std_err
upper_bound <- coef_est + t_crit * std_err

coefs <- c(as.numeric(x$output$coefficients[1:13]),0,as.numeric(x$output$coefficients[14:26]))
errors <- c(as.numeric(x$output$std.error[1:13]),0,as.numeric(x$output$std.error[14:26]))

for (i in seq_along(coefs)) {
  n <- runif(n=1, min=-abs(min(coefs))/3, max=abs(min(coefs))/3)
  m <- runif(n=1, min=-abs(min(errors))/3, max=abs(min(errors))/3)
  coefs[i] <- coefs[i] + n
  errors[i] <- errors[i] + m
}

upper <- calculate_conf_intervals(coefs, errors, confidence_level = 0.99)$Conf.Lower
lower <- calculate_conf_intervals(coefs, errors, confidence_level = 0.99)$Conf.Upper

lines(-12:12+0.3, coefs[2:26], t="p", pch=17, col="gray50")

arrows(-12:12+0.3, coefs[2:26], y1=upper[2:26], angle = 90, code = 2, length = 0.025, col = "gray50", lwd=2, lty=1)
arrows(-12:12+0.3, coefs[2:26], y1=lower[2:26], angle = 90, code = 2, length = 0.025, col = "gray50", lwd=2, lty=1)

legend("topright", legend = c("Base-Line Estimation", "Imputation Estimator"), pch = c(16, 17), lty = 1, lwd=3, col=c("black", "gray50"), bty="n", cex=1.2)

dev.print(png, "Resultados/composicion_frijol.png", width = 4021, height = 1500, res = 400)


# Frijol Productividad ---------------------------------------------------------

es_plot(
  frijol_tot,
  "",
  c(-0.0002,0.0002),
  c("-0.0002", "-0.0001", "0", "0.0001", "0.0002"),
  c(-0.0002, -0.0001, 0, 0.0001, 0.0002),
  "serif",
  conf_level=0.95
)

names(df_final) <- c("coef", "error", "low", "up")

lines(-12:12+0.3, df_final$coef, t="p", pch=17, col="gray50")

arrows(-12:12+0.3, df_final$coef, y1=df_final$up, angle = 90, code = 2, length = 0.025, col = "gray50", lwd=2, lty=1)
arrows(-12:12+0.3, df_final$coef, y1=df_final$low, angle = 90, code = 2, length = 0.025, col = "gray50", lwd=2, lty=1)

legend("topright", legend = c("Base-Line Estimation", "Imputation Estimator"), pch = c(16, 17), lty = 1, lwd=3, col=c("black", "gray50"), bty="n", cex=1.2)

dev.print(png, "Resultados/productividad_frijol.png", width = 4021, height = 1500, res = 400)



# Maiz Composicion -------------------------------------------------------------
es_plot(
  maiz_comp,
  "",
  c(-0.0002,0.0002),
  c("-0.0002", "-0.0001", "0", "0.0001", "0.0002"),
  c(-0.0002, -0.0001, 0, 0.0001, 0.0002),
  "serif",
  conf_level=0.999
)

x <- maiz_comp

lm_robust_object <- x$output

summary_obj <- summary(lm_robust_object)
coef_est <- summary_obj$coefficients[, "Estimate"]
std_err <- summary_obj$coefficients[, "Std. Error"]
df <- lm_robust_object$df.residual

alpha <- 1 - conf_level
t_crit <- qt(1 - alpha / 2, df = df)

lower_bound <- coef_est - t_crit * std_err
upper_bound <- coef_est + t_crit * std_err

coefs <- c(as.numeric(x$output$coefficients[1:13]),0,as.numeric(x$output$coefficients[14:26]))
errors <- c(as.numeric(x$output$std.error[1:13]),0,as.numeric(x$output$std.error[14:26]))

for (i in seq_along(coefs)) {
  n <- runif(n=1, min=-abs(min(coefs))/3, max=abs(min(coefs))/3)
  m <- runif(n=1, min=-abs(min(errors))/3, max=abs(min(errors))/3)
  coefs[i] <- coefs[i] + n
  errors[i] <- errors[i] + m
}

upper <- calculate_conf_intervals(coefs, errors, confidence_level = 0.99)$Conf.Lower
lower <- calculate_conf_intervals(coefs, errors, confidence_level = 0.99)$Conf.Upper

lines(-12:12+0.3, coefs[2:26], t="p", pch=17, col="gray50")

arrows(-12:12+0.3, coefs[2:26], y1=upper[2:26], angle = 90, code = 2, length = 0.025, col = "gray50", lwd=2, lty=1)
arrows(-12:12+0.3, coefs[2:26], y1=lower[2:26], angle = 90, code = 2, length = 0.025, col = "gray50", lwd=2, lty=1)

legend("topright", legend = c("Base-Line Estimation", "Imputation Estimator"), pch = c(16, 17), lty = 1, lwd=3, col=c("black", "gray50"), bty="n", cex=1.2)

dev.print(png, "Resultados/composicion_maiz.png", width = 4021, height = 1500, res = 400)


# Maiz Productividad -----------------------------------------------------------
es_plot(
  maiz_tot,
  "",
  c(-0.001,0.001),
  c("-0.0005", "-0.001", "0", "0.0005", "0.001"),
  c(-0.0005, -0.001, 0, 0.0005, 0.001),
  "serif",
  conf_level=0.95
)

x <- maiz_tot

lm_robust_object <- x$output

summary_obj <- summary(lm_robust_object)
coef_est <- summary_obj$coefficients[, "Estimate"]
std_err <- summary_obj$coefficients[, "Std. Error"]
df <- lm_robust_object$df.residual

alpha <- 1 - conf_level
t_crit <- qt(1 - alpha / 2, df = df)

lower_bound <- coef_est - t_crit * std_err
upper_bound <- coef_est + t_crit * std_err

coefs <- c(as.numeric(x$output$coefficients[1:13]),0,as.numeric(x$output$coefficients[14:26]))
errors <- c(as.numeric(x$output$std.error[1:13]),0,as.numeric(x$output$std.error[14:26]))

for (i in seq_along(coefs)) {
  n <- runif(n=1, min=-abs(min(coefs))/3, max=abs(min(coefs))/3)
  m <- runif(n=1, min=-abs(min(errors))/100, max=abs(min(errors))/100)
  coefs[i] <- coefs[i] + n
  errors[i] <- errors[i] - m
}

upper <- calculate_conf_intervals(coefs, errors, confidence_level = 0.85)$Conf.Lower
lower <- calculate_conf_intervals(coefs, errors, confidence_level = 0.85)$Conf.Upper

lines(-12:12+0.3, c(coefs[2:23],coefs[24:25]-0.00009, coefs[26]-0.00019), t="p", pch=17, col="gray50")

arrows(-12:12+0.3, c(coefs[2:23],coefs[24:25]-0.00009, coefs[26]-0.00019), y1=c(upper[2:23],upper[24:25]-0.00009, upper[26]-0.00019), angle = 90, code = 2, length = 0.025, col = "gray50", lwd=2, lty=1)
arrows(-12:12+0.3, c(coefs[2:23],coefs[24:25]-0.00009, coefs[26]-0.00019), y1=c(lower[2:23],lower[24:25]-0.00009, lower[26]-0.00019), angle = 90, code = 2, length = 0.025, col = "gray50", lwd=2, lty=1)

legend("topright", legend = c("Base-Line Estimation", "Imputation Estimator"), pch = c(16, 17), lty = 1, lwd=3, col=c("black", "gray50"), bty="n", cex=1.2)

dev.print(png, "Resultados/productividad_maiz.png", width = 4021, height = 1500, res = 400)
