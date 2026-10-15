
# Cargar librerías necesarias
install.packages("ggplot2") # si no está instalada
library(ggplot2)

# Leer el dataset
datos <- read.csv("survey lung cancer.csv", sep = ";")
# Reparar nombres para que R los pueda usar sin errores
names(datos) <- make.names(names(datos))
names(datos)
lineas <- readLines("survey lung cancer.csv")
length(lineas)
lineas[1]
# Verificar nombres corregidos
names(datos)

# Explorar estructura
head(datos)
str(datos)
summary(datos)
colSums(is.na(datos))

# Gráficos básicos
# 1 histograma de edades
hist(datos$AGE,
     main = "Histograma de edad",
     xlab = "Edad",
     col = "lightblue")
# 2 gráfico de barras de género
barplot(table(datos$GENDER),
        main = "Distribución por género",
        col = c("pink", "lightblue"))
# 3 consumo de alcohol vs cáncer
barplot(table(datos$ALCOHOL.CONSUMING, datos$LUNG_CANCER),
        beside = TRUE,
        main = "Alcohol vs Cáncer",
        col = c("gray", "red"))
# 4 tos vs cáncer
barplot(table(datos$COUGHING, datos$LUNG_CANCER),
        beside = TRUE,
        main = "Tos vs Cáncer",
        col = c("blue", "orange"),
        legend.text = c("No tose", "Tose"))
    


# Modelo de regresión logística
# primero convierto el 0 y 1
datos <- read.csv("survey lung cancer.csv", sep = ";")
datos$cancer <- ifelse(datos$LUNG_CANCER == "YES", 1, 0)
modelo <- glm(cancer ~ ALCOHOL.CONSUMING + COUGHING, data = datos, family = binomial)
summary(modelo)

# Cargar paquetes necesarios
library(dplyr)

# Asegurar que las variables son factores
datos <- datos %>%
  mutate(
    ALCOHOL.CONSUMING = factor(ALCOHOL.CONSUMING, levels = c(1,2), labels = c("No", "Sí")),
    COUGHING = factor(COUGHING, levels = c(1,2), labels = c("No", "Sí")),
    LUNG_CANCER = factor(LUNG_CANCER, levels = c("NO","YES"), labels = c("No","Sí"))
  )

# Modelo logístico simple según tu hipótesis
modelo_logit <- glm(
  LUNG_CANCER ~ ALCOHOL.CONSUMING + COUGHING,
  data = datos,
  family = binomial
)

# Ver el resumen del modelo
summary(modelo_logit)

# Obtener los Odds Ratios (interpretación real)
exp(coef(modelo_logit))

# Intervalos de confianza de los OR (opcional pero profesional)
exp(confint(modelo_logit))



library(ggplot2)

# Datos antes y después de SMOTE
smote_df <- data.frame(
  Estado = rep(c("Antes (crudo)", "Después (SMOTE)"), each = 2),
  Clase = rep(c("Yes", "No"), times = 2),
  Frecuencia = c(270, 39, 270, 270)
)

# Gráfico
ggplot(smote_df, aes(x = Estado, y = Frecuencia, fill = Clase)) +
  geom_col(position = "dodge", width = 0.6) +
  scale_fill_manual(values = c("Yes" = "#FF80A6",   # rosa claro
                               "No"  = "#A6CEE3")) +  # azul suave
  labs(
    title = "Balanceo de Clases Antes y Después de SMOTE",
    x = "",
    y = "Frecuencia"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.title = element_blank(),
    legend.position = "bottom"
  )

