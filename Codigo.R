##############################################################
# Taller de Business Analytics - Caso Tour Marketing (Opry) 
# Objetivo: Explorar la relación entre ventas y publicidad
# Integrantes del grupo: Juan Sebastian Cardenas - Paula Rodriguez
##############################################################

# ============================================================
# 0. Preparación
# ============================================================

library(readr)
library(dplyr)
library(ggplot2)   
library(psych)  
library(broom)
library(tibble)

# ============================================================
# 1. Exploración inicial de los datos
# ============================================================

#a) Importar la base de datos
opry <- read_csv("C:/Users/User/OneDrive/Documentos/yop/universidad/Semestre 5/analitica/Caso 2/Caso3_ToursMarketing/Opry_data.csv")

#opry <- read_csv(file.choose("Opry_data.csv"))

str(opry)
head(opry)

#b) Resumen estadístico de variables clave
summary(opry %>% select(Ventas, Gasto_Publicidad, Ordenes, Google_Trends_Opry))


#c) Gráfico de dispersión: Ventas vs. Gasto en publicidad

ggplot(opry, aes(x = Gasto_Publicidad, y = Ventas)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Relación entre Ventas y Gasto en Publicidad",
       x = "Gasto en Publicidad",
       y = "Ventas")

#d) Gráfico temporal: Ventas y Gasto en publicidad

ggplot(opry, aes(x = Date)) +
  geom_line(aes(y = Ventas, color = "Ventas")) +
  geom_line(aes(y = Gasto_Publicidad, color = "Publicidad")) +
  scale_y_continuous(
    name = "Ventas",
    sec.axis = sec_axis(~., name = "Gasto en Publicidad")
  ) +
  labs(title = "Evolución temporal de Ventas y Gasto en Publicidad") +
  theme_minimal() +
  scale_color_manual(values = c("Ventas" = "blue", "Publicidad" = "red"))


#============================================================ 
# 2. Regresión naive (simple)
#============================================================

mod_naive <- lm(Ventas ~ Gasto_Publicidad, data = opry)
summary(mod_naive)


# TABLA 2 (coeficientes modelo naive)
tabla_coef_naive <- tidy(mod_naive) %>% 
  mutate(across(estimate:p.value, ~round(.x, 6)))
tabla_coef_naive


# TABLA 2b (métricas globales modelo naive)
tabla_glance_naive <- glance(mod_naive) %>% 
  transmute(Modelo = "Naive",
            r.squared = round(r.squared, 4),
            adj.r.squared = round(adj.r.squared, 4),
            sigma = round(sigma, 1),   # ~ RMSE
            AIC = round(AIC, 1),
            BIC = round(BIC, 1),
            p.value = signif(p.value, 3))
tabla_glance_naive

ggplot(opry, aes(x = Gasto_Publicidad, y = Ventas)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Regresión Naive: Ventas vs. Gasto en Publicidad",
       x = "Gasto en Publicidad",
       y = "Ventas")

# ============================================================
# 3. Regresión con dummy de estacionalidad
# ============================================================


opry <- opry %>%
  mutate(
    month = format(Date, "%m"),
    year = format(Date, "%Y"),
    # Dummy de estacionalidad: 1 en dic (12) y ene (01)
    Holliday_seasson = ifelse(month %in% c("12", "01"), 1, 0)
  )

  mod_season <- lm(Ventas ~ Gasto_Publicidad + Holliday_seasson, data = opry)

summary(mod_season)

# TABLA 3a (coeficientes modelo con estacionalidad)
tabla_coef_season <- tidy(mod_season) %>% 
  mutate(across(estimate:p.value, ~round(.x, 6)))
tabla_coef_season


# TABLA 3b (métricas globales modelo con estacionalidad)
tabla_glance_season <- glance(mod_season) %>% 
  transmute(Modelo = "Naive + Estacionalidad",
            r.squared = round(r.squared, 4),
            adj.r.squared = round(adj.r.squared, 4),
            sigma = round(sigma, 1),   # ~ RMSE
            AIC = round(AIC, 1),
            BIC = round(BIC, 1),
            p.value = signif(p.value, 3))
tabla_glance_season

