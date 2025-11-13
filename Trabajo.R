# PaulaBelaza_Trabajo2.R
# Trabajo final Bioinformática - Curso 25/26
# Análisis de parámetros biomédicos por tratamiento

# 1. Cargar librerías (si necesarias) y datos del archivo "datos_biomed.csv". (0.5 pts)

datos<- read.csv("datos_biomed (1).csv")
# 2. Exploración inicial con las funciones head(), summary(), dim() y str(). ¿Cuántas variables hay? ¿Cuántos tratamientos? (0.5 pts)
print("===EXPLORACIÖN INICIAL===")
print(head(datos)) #esto me muestra las primeras 6 filas
print(summary(datos)) #muestra las estadísticas básicas de todas las columnas
print(paste("Filas:", nrow(datos))) #me dice cuántas filas hay
print(paste("Columnas:", ncol(datos))) #cuenta las columnas
print(paste("Tratamientos:", toString(unique(datos$Tratamiento)))) #muestra los tipos de tratamientos y grupos experimentales


# 3. Una gráfica que incluya todos los boxplots por tratamiento. (1 pt)
par(mfrow = c(1,3)) #hace 3 gráficos en una fila, visualización múltiple
boxplot(Glucosa ~ Tratamiento, data = datos, main = "Glucosa") #boxplot de glucosa
boxplot(Presion ~ Tratamiento, data = datos, main = "Presión") #boxplot de presión
boxplot(Colesterol ~ Tratamiento, data = datos, main = "Colesterol") #boxplot de colesterol

# 4. Realiza un violin plot (investiga qué es). (1 pt)
#un violin plot es similar a los boxplots pero muestran densidad
# primero instalo y cargo el paquete vioplot 
if(!require(vioplot)) {
  install.packages("vioplot")
  library(vioplot)
}
  
library(vioplot)

vioplot(Glucosa ~ Tratamiento, data = datos, main = "Violin Plot - Glucosa", col = "lightblue")
vioplot(Presion ~ Tratamiento, data = datos, main = "Violin Plot - Presión", col = "lightgreen") 
vioplot(Colesterol ~ Tratamiento, data = datos, main = "Violin Plot - Colesterol", col = "lightcoral")

# 5. Realiza un gráfico de dispersión "Glucosa vs Presión". Emplea legend() para incluir una leyenda en la parte inferior derecha. (1 pt)
colores <- character(nrow(datos))
colores[datos$Tratamiento == "Placebo"] <- "red"
colores[datos$Tratamiento == "FarmacoA"] <- "blue"
colores[datos$Tratamiento == "FarmacoB"] <- "green"
plot(datos$Glucosa, datos$Presion, col = colores, pch = 16,
     xlab = "Glucosa", ylab = "Presión", main = "Glucosa vs Presión")
legend("bottomright", legend = c("Placebo", "FarmacoA", "FarmacoB"),
       col = c("red", "blue", "green"), pch = 16
      


# 6. Realiza un facet Grid (investiga qué es): Colesterol vs Presión por tratamiento. (1 pt)
#un facet grid es una forma de dividir un gráfico en varios subgráficos según una o más variables. Sirve para comparar grupos dentro de un mismo conjunto de datos de form visual.
par(mfrow = c(1, 3))  # 3 gráficos en fila

plot(subset(datos, Tratamiento == "Placebo")$Presion, 
     subset(datos, Tratamiento == "Placebo")$Colesterol,
     main = "Placebo", xlab = "Presión", ylab = "Colesterol", col = "red", pch = 16)

plot(subset(datos, Tratamiento == "FarmacoA")$Presion, 
     subset(datos, Tratamiento == "FarmacoA")$Colesterol,
     main = "FarmacoA", xlab = "Presión", ylab = "Colesterol", col = "blue", pch = 16)

plot(subset(datos, Tratamiento == "FarmacoB")$Presion, 
     subset(datos, Tratamiento == "FarmacoB")$Colesterol,
     main = "FarmacoB", xlab = "Presión", ylab = "Colesterol", col = "green", pch = 16)

par(mfrow = c(1, 1))
# 7. Realiza un histogramas para cada variable. (0.5 pts)
par(mfrow = c(2, 2))  # 4 espacios para gráficos (2 filas x 2 columnas)

hist(datos$Glucosa, main = "Distribución Glucosa", xlab = "Glucosa", col = "lightblue")
hist(datos$Presion, main = "Distribución Presión", xlab = "Presión", col = "lightgreen")  
hist(datos$Colesterol, main = "Distribución Colesterol", xlab = "Colesterol", col = "lightpink")

# 8. Crea un factor a partir del tratamiento. Investifa factor(). (1 pt)
#esto lo hago para poder hacer un test estadístico
datos$Tratamiento <- as.factor(datos$Tratamiento)
print("Niveles del factor Tratamiento:")
print(levels(datos$Tratamiento))
print("Resumen:")
print(summary(datos$Tratamiento))
# 9. Obtén la media y desviación estándar de los niveles de glucosa por tratamiento. Emplea aggregate() o apply(). (0.5 pts)
#voy a calcularlo para cada tratamiento
media_placebo <- mean(datos$Glucosa[datos$Tratamiento == "Placebo"])
sd_placebo <-sd(datos$Glucosa[datos$Tratamiento == "PLacebo"])

media_farmacoA <- mean(datos$Glucosa[datos$Tratamiento == "FarmacoA"])
sd_farmacoA <- sd(datos$Glucosa[datos$Tratamiento == "FarmacoA"])

media_farmacoB <- mean(datos$Glucosa[datos$Tratamiento == "FarmacoB"])
sd_farmacoB <- sd(datos$Glucosa[datos$Tratamiento == "FarmacoB"])

print(paste("Placebo - Media:", round(media_placebo, 2), "Desviación:", round(sd_placebo, 2)))
print(paste("FarmacoA - Media:", round(media_farmacoA, 2), "Desviación:", round(sd_farmacoA, 2)))
print(paste("FarmacoB - Media:", round(media_farmacoB, 2), "Desviación:", round(sd_farmacoB, 2)))

# 10. Extrae los datos para cada tratamiento y almacenalos en una variable. Ejemplo todos los datos de Placebo en una variable llamada placebo. (1 pt)
placebo <- subset(datos, Tratamiento == "Placebo")
farmacoA <- subset(datos, Tratamiento == "FarmacoA")
farmacoB <- subset(datos, Tratamiento == "FarmacoB")

print(paste("Placebo:", nrow(placebo), "observaciones"))
print(paste("FarmacoA:", nrow(farmacoA), "observaciones"))
print(paste("FarmacoB:", nrow(farmacoB), "observaciones"))

# 11. Evalúa si los datos siguen una distribución normal y realiza una comparativa de medias acorde. (1 pt)
#para evaluar si los datos son normales realizaré un test de shapiro. Un resultado p>0,05 singifica que los datos son normales, mientras que un resultado p<0,05 significa que no son normales. este test me permitirá saber que test usar luego,si anova o krustal-wallis
#test de shapiro-wilk para glucosa en cada grupo
shapiro_placebo <- shapiro.test(placebo$Glucosa)
shapiro_farmacoA <- shapiro.test(farmacoA$Glucosa)
shapiro_farmacoB <- shapiro.test(farmacoB$Glucosa)

print("Resultados Shapiro-Wilk (p > 0.05 = normal):")
print(paste("Placebo: p =", round(shapiro_placebo$p.value, 4)))
print(paste("FarmacoA: p =", round(shapiro_farmacoA$p.value, 4)))
print(paste("FarmacoB: p =", round(shapiro_farmacoB$p.value, 4)))
#interpretación test
print("Interpretación:")
if(shapiro_placebo$p.value > 0.05) print("Placebo: Distribución normal") else print("Placebo: No normal")
if(shapiro_farmacoA$p.value > 0.05) print("FarmacoA: Distribución normal") else print("FarmacoA: No normal")
if(shapiro_farmacoB$p.value > 0.05) print("FarmacoB: Distribución normal") else print("FarmacoB: No normal")
#verificar si todos los grupos son normales
todos_normales <- (shapiro_placebo$p.value > 0.05) & 
  (shapiro_farmacoA$p.value > 0.05) & 
  (shapiro_farmacoB$p.value > 0.05)

if(todos_normales) {
  print("Todos los grupos son normales -> Se puede usar ANOVA")
  # podemos hacer ANOVA 
} else {
  print("Al menos un grupo NO es normal -> Se debería usar Kruskal-Wallis")
  # Hacemos Kruskal-Wallis como comparativa de medias no paramétrica
  print("Realizando Kruskal-Wallis como comparativa de medias:")
  kruskal_test <- kruskal.test(Glucosa ~ Tratamiento, data = datos)
  print(kruskal_test)
  
  if(kruskal_test$p.value < 0.05) {
    print("Kruskal-Wallis: Hay diferencias significativas entre tratamientos")
  } else {
    print("Kruskal-Wallis: No hay diferencias significativas")
  }
}
# 12. Realiza un ANOVA sobre la glucosa para cada tratamiento. (1 pt)
anova_resultado <- aov(Glucosa ~ Tratamiento, data = datos)
print("Resumen del ANOVA:")
print(summary(anova_resultado))
#para obtener el p-value
p_valor <- summary(anova_resultado)[[1]][1,5]
print(paste("P-valor del ANOVA:", round(p_valor, 4)))
#interpretacin--> si p-value < 0,05 = hay diferencias significativas entre al menos 2 grupos / p-value > 0,05 = NO hay diferencia significativa
#si el p-value es significativo hago el tkey HSD, que mide exactamente entre qué grupos hay diferencias
if(p_valor < 0.05) {
  print("RESULTADO: Existen diferencias significativas entre los tratamientos")
  print("Realizando test post-hoc de Tukey...")
  tukey_resultado <- TukeyHSD(anova_resultado)
  print(tukey_resultado)
} else {
  print("RESULTADO: No existen diferencias significativas entre los tratamientos")
}

