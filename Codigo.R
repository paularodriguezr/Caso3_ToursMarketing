##############################################################
# Taller de Business Analytics - Caso Tour Marketing (Opry) 
# Objetivo: Explorar la relación entre ventas y publicidad
# Integrantes del grupo: Juan Sebastian Cardenas - Paula Rodriguez
##############################################################

# ============================================================
# 0. Preparación
# ============================================================
install.packages("readr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("psych")
install.packages("broom")
install.packages("tibble")
install.packages("officer")
install.packages("flextable")
install.packages("gridExtra")

library(gridExtra)
library(readr)
library(dplyr)
library(ggplot2)   
library(psych)  
library(broom)
library(tibble)
library(officer)
library(flextable)

# ============================================================
# 1. Exploración inicial de los datos
# ============================================================

#a) Importar la base de datos
opry <- read_csv("Opry_data.csv")

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

# ============================================================
# 4. Transformación logarítmica
# ============================================================

# a) Crear la variable logarítmica
opry <- opry %>%
  mutate(Log_Ventas = log(Ventas))

# b) Estimar el modelo con la variable logarítmica como dependiente
mod_log <- lm(Log_Ventas ~ Gasto_Publicidad + Holliday_seasson, data = opry)

# c) Resumen del modelo
summary(mod_log)

# d) Tablas de coeficientes y métricas globales
tabla_coef_log <- broom::tidy(mod_log) %>%
  mutate(across(estimate:p.value, ~round(.x, 6)))
tabla_coef_log

tabla_glance_log <- broom::glance(mod_log) %>%
  transmute(Modelo = "Log(Ventas) ~ Gasto_Publicidad + Holliday_seasson",
            r.squared = round(r.squared, 4),
            adj.r.squared = round(adj.r.squared, 4),
            sigma = round(sigma, 4),   
            AIC = round(AIC, 1),
            BIC = round(BIC, 1),
            p.value = signif(p.value, 3))
tabla_glance_log

# ============================================================
# 5. Modelo con Retail_sales__total y CPI
# ============================================================

# Modelo con Retail_sales_total como variable adicional
mod_final <- lm(Log_Ventas ~ Gasto_Publicidad + Holliday_seasson + 
                	Retail_sales__total + cpi, data = opry)

# Resumen del modelo
summary(mod_final)

# Tabla de coeficientes
tabla_coef_final <- tidy(mod_final) %>%
  mutate(across(estimate:p.value, ~round(.x, 6)))
tabla_coef_final

# Tabla de métricas globales del modelo final
tabla_glance_final <- glance(mod_final) %>% 
  transmute(Modelo = "Modelo Final (Log + Retail + CPI)",
            r.squared = round(r.squared, 4),
            adj.r.squared = round(adj.r.squared, 4),
            sigma = round(sigma, 4),   # RMSE (en log)
            AIC = round(AIC, 1),
            BIC = round(BIC, 1),
            p.value = signif(p.value, 3))
tabla_glance_final

# Gráfico del modelo final con múltiples paneles
# Crear gráficos individuales
p1 <- ggplot(opry, aes(x = Gasto_Publicidad, y = Log_Ventas)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Log(Ventas) vs. Gasto Publicidad",
       x = "Gasto en Publicidad",
       y = "Log(Ventas)") +
  theme_minimal()

p2 <- ggplot(opry, aes(x = as.factor(Holliday_seasson), y = Log_Ventas)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Log(Ventas) vs. Temporada",
       x = "Temporada Alta (1) vs. Normal (0)",
       y = "Log(Ventas)") +
  theme_minimal()

p3 <- ggplot(opry, aes(x = Retail_sales__total, y = Log_Ventas)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Log(Ventas) vs. Retail Sales",
       x = "Ventas Minoristas Totales",
       y = "Log(Ventas)") +
  theme_minimal()

p4 <- ggplot(opry, aes(x = cpi, y = Log_Ventas)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Log(Ventas) vs. CPI",
       x = "Índice de Precios al Consumidor",
       y = "Log(Ventas)") +
  theme_minimal()

# Combinar los gráficos en un panel
panel_final <- grid.arrange(p1, p2, p3, p4, ncol = 2,
                          top = "Relaciones entre Log(Ventas) y Variables Predictoras")

# Guardar el panel de gráficos
ggsave("modelo_final_panel.png", 
       plot = panel_final,
       width = 7, 
       height = 6)

# ============================================================
# 6. Exportación de resultados
# ============================================================

# Crear tabla de coeficientes con formato
tabla_resultados <- tidy(mod_final) %>%
  mutate(
    significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ ""
    ),
    estimate = round(estimate, 4),
    std.error = round(std.error, 4),
    statistic = round(statistic, 4),
    p.value = round(p.value, 4)
  ) %>%
  rename(
    "Variable" = term,
    "Coeficiente" = estimate,
    "Error Est." = std.error,
    "Estadístico t" = statistic,
    "P-valor" = p.value,
    "Significancia" = significance
  )

# Crear tabla de métricas globales
metricas_globales <- glance(mod_final) %>%
  select(r.squared, adj.r.squared, sigma, AIC) %>%
  round(4)

# Crear documento Word
doc <- read_docx()

# Agregar título
doc <- doc %>%
  body_add_par("Resultados del Modelo de Regresión con Retail Sales y CPI", style = "heading 1") %>%
  body_add_par("", style = "Normal")

# Agregar tabla de coeficientes
doc <- doc %>%
  body_add_par("Tabla 1: Coeficientes del modelo", style = "heading 2") %>%
  body_add_flextable(
    flextable(tabla_resultados) %>%
      theme_vanilla() %>%
      autofit()
  )

# Agregar nota al pie
doc <- doc %>%
  body_add_par("Niveles de significancia: *** p<0.001, ** p<0.01, * p<0.05", style = "Normal") %>%
  body_add_par("", style = "Normal")

# Agregar métricas globales
doc <- doc %>%
  body_add_par("Tabla 2: Métricas globales del modelo", style = "heading 2") %>%
  body_add_flextable(
    flextable(metricas_globales) %>%
      theme_vanilla() %>%
      autofit()
  )

# Agregar gráfico al documento Word
doc <- doc %>%
  body_add_par("Gráfico 1: Relaciones del Modelo Final", style = "heading 2") %>%
  body_add_img("modelo_final_panel.png", 
               width = 7, 
               height = 6)

# Guardar el gráfico
ggsave("modelo_grafico.png", 
       plot = last_plot(), 
       width = 7, 
       height = 6)

# Guardar documento
print(doc, target = "Resultados_Modelo_Retail_Sales_CPI.docx")
