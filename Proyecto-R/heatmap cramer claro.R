datos <- read.csv("survey lung cancer.csv", sep = ";")

#HEATMAP
# Paquetes necesarios
install.packages("vcd")
install.packages("reshape2")
library(ggplot2)
library(dplyr)
library(vcd)

# Función para calcular Cramer's V entre dos variables categóricas
cramersV <- function(x, y) {
  tbl <- table(x, y)
  v <- suppressWarnings(vcd::assocstats(tbl)$cramer)
  return(v)
}

# Seleccionamos solo las variables categóricas importantes para tu póster
vars <- c("ALCOHOL.CONSUMING", "COUGHING", "FATIGUE", "LUNG_CANCER", "WHEEZING")
df <- datos[vars]

# Calculamos la matriz de Cramer's V (sin diagonal)
n <- length(vars)
mat <- matrix(NA, n, n)
colnames(mat) <- vars
rownames(mat) <- vars

for (i in 1:n) {
  for (j in 1:n) {
    if (i != j) {
      mat[i, j] <- cramersV(df[[i]], df[[j]])
    }
  }
}

# Convertimos a dataframe largo para ggplot
library(reshape2)
df_mat <- melt(mat, na.rm = TRUE)

# Heatmap rosita
ggplot(df_mat, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = sprintf("%.2f", value)), color = "black", size = 4) +
  scale_fill_gradientn(
    colours = c("#FFE6EC", "#FFB3C6", "#FF80A6", "#FF4D88"),
    limits = c(0, max(df_mat$value, na.rm = TRUE)),
    name = "Cramer's V"
  ) +
  labs(
    title = "Matriz de Asociación (Cramer's V) – Variables Categóricas",
    x = "", y = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  )
