datos <- read.csv("survey lung cancer.csv")
#1.evaluación general
#estructura general 
str(datos)
#nombres columna
names(datos)
#nombre filas y columnas
dim(datos)
#2.estadistica descriptiva
summary(datos)
#tabla de frecuencias para variables categóricas
table(datos$genero)
prop.table(table(datos$genero))
#3.visualización datos
library(ggplot2)
### ================================
### 1. CARGA DEL DATASET
### ================================
df <- read.csv("survey lung cancer.csv", sep=";", header=TRUE)

### ================================
### 2. CONVERTIR VARIABLES
### ================================
# Variables binarias (1=No, 2=Sí)
binarias <- c("SMOKING","YELLOW_FINGERS","ANXIETY","PEER_PRESSURE",
              "CHRONIC.DISEASE","FATIGUE","ALLERGY","WHEEZING",
              "ALCOHOL.CONSUMING","COUGHING","SHORTNESS.OF.BREATH",
              "SWALLOWING.DIFFICULTY","CHEST.PAIN")

# Cambiar 1/2 → No/Sí
for (col in binarias) {
  df[[col]] <- factor(df[[col]], levels=c(1,2), labels=c("No","Sí"))
}

# Género
df$GENDER <- factor(df$GENDER)

# Diagnóstico
df$LUNG_CANCER <- factor(df$LUNG_CANCER)

### ================================
### 3. HISTOGRAMA DE EDAD (ROSA)
### ================================
hist(df$AGE,
     breaks=15,
     col="pink",
     border="black",
     main="Histograma de Edad",
     xlab="Edad")

### ================================
### 4. GRÁFICOS DE BARRAS (BINARIAS)
### ================================
par(mfrow=c(3,5))   # para mostrar varias gráficas a la vez

for (col in binarias) {
  barplot(table(df[[col]]),
          col="pink",
          border="black",
          main=col,
          ylab="Frecuencia")
}

par(mfrow=c(1,1))  # reset

### ================================
### 5. BOXPLOT EDAD vs CÁNCER
### ================================
boxplot(AGE ~ LUNG_CANCER,
        data=df,
        col=c("lightblue","pink"),
        main="Edad según diagnóstico",
        xlab="Cáncer de pulmón",
        ylab="Edad")

### ================================
### 6. NA Y OUTLIERS
### ================================
print("Valores NA por columna:")
print(colSums(is.na(df)))

boxplot(df$AGE, col="pink",
        main="Detección de outliers en EDAD")


#PARA ANALIZAR MI HIPÓTESIS 
# ========================================================
# 1. Cargar librerías
# ========================================================
library(tidyverse)
library(janitor)

# ========================================================
# 2. Cargar dataset
# ========================================================
df <- read.csv("survey lung cancer.csv", sep=";") |> clean_names()

# ========================================================
# 3. Convertir variables binarias a factores
# ========================================================
binarias <- c("smoking","yellow_fingers","anxiety","peer_pressure",
              "chronic_disease","fatigue","allergy","wheezing",
              "alcohol_consuming","coughing","shortness_of_breath",
              "swallowing_difficulty","chest_pain")

for (col in binarias) {
  df[[col]] <- factor(df[[col]], levels=c(1,2), labels=c("No","Sí"))
}

df$lung_cancer <- factor(df$lung_cancer)
df$gender <- factor(df$gender)

# ========================================================
# 4. GRÁFICO 1 — HISTOGRAMA EDAD (SIEMPRE SE VE)
# ========================================================
print(
  ggplot(df, aes(x = age)) +
    geom_histogram(binwidth = 2, fill="pink", color="black") +
    theme_minimal() +
    labs(title="Histograma de Edad", x="Edad", y="Frecuencia")
)

# ========================================================
# 5. GRÁFICO 2 — ALCOHOL vs CÁNCER
# ========================================================
print(
  ggplot(df, aes(x = alcohol_consuming, fill = lung_cancer)) +
    geom_bar(position="fill") +
    scale_fill_manual(values=c("lightblue","pink")) +
    theme_minimal() +
    labs(title="Consumo de alcohol vs Cáncer",
         y="Proporción", x="¿Consume alcohol?")
)

# ========================================================
# 6. GRÁFICO 3 — TOS vs CÁNCER
# ========================================================
print(
  ggplot(df, aes(x = coughing, fill = lung_cancer)) +
    geom_bar(position="fill") +
    scale_fill_manual(values=c("lightblue","pink")) +
    theme_minimal() +
    labs(title="Tos frecuente vs Cáncer",
         y="Proporción", x="¿Tos frecuente?")
)

# ========================================================
# 7. GRÁFICO 4 — Combinación Alcohol + Tos
# ========================================================
print(
  ggplot(df, aes(x = alcohol_consuming, fill = coughing)) +
    geom_bar(position="dodge") +
    theme_minimal() +
    labs(title="Relación Alcohol + Tos",
         x="Consumo de alcohol", y="Conteo")
)





#estudio eda con mi hipotesis

df <- read.csv("survey lung cancer.csv", sep = ";") |> clean_names()
print(names(df))
print(dim(df))
print(colSums(is.na(df)))
#4. convertir binarias de 1/2 a NO/SI
binarias <- c(
  "smoking","yellow_fingers","anxiety","peer_pressure",
  "chronic_disease","fatigue","allergy","wheezing",
  "alcohol_consuming","coughing","shortness_of_breath",
  "swallowing_difficulty","chest_pain"
)
for (col in binarias) {
  if (col %in% names(df)) {
    df[[col]] <- factor(df[[col]], levels = c(1,2), labels = c("No","Sí"))
  } else {
    cat("COLUMNA NO ENCONTRADA:", col, "\n")
  }
}

df$gender <- factor(df$gender)
df$lung_cancer <- factor(df$lung_cancer)
# 5. HISTOGRAMA EDAD (DEBE MOSTRARSE)
print(
  ggplot(df, aes(x = age)) +
    geom_histogram(binwidth = 3, fill="pink", color="black") +
    theme_minimal() +
    labs(title="Histograma de Edad", x="Edad", y="Frecuencia")
)

#############################################################
# 6. TOS vs CÁNCER
#############################################################
print(
  ggplot(df, aes(x = coughing, fill = lung_cancer)) +
    geom_bar(position="fill") +
    theme_minimal() +
    labs(title="Tos vs Cáncer", x="¿Tos?", y="Proporción")
)

#############################################################
# 7. ALCOHOL vs CÁNCER
#############################################################
print(
  ggplot(df, aes(x = alcohol_consuming, fill = lung_cancer)) +
    geom_bar(position="fill") +
    theme_minimal() +
    labs(title="Alcohol vs Cáncer", x="¿Alcohol?", y="Proporción")
)









#etoy hasta el nabo
#############################################################
# 1. LIBRERÍAS
#############################################################
library(tidyverse)
library(janitor)
library(smotefamily)
library(ggplot2)

#############################################################
# 2. CARGAR DATASET Y LIMPIAR NOMBRES
#############################################################
df <- read.csv("survey lung cancer.csv", sep=";") |> clean_names()

#############################################################
# 3. CREAR DATASET NUMÉRICO PARA SMOTE
#############################################################

df_smote <- df

# lung_cancer → convertir a 0/1
df_smote$lung_cancer <- ifelse(df_smote$lung_cancer == "YES", 1, 0)

# ELIMINAR VARIABLES NO NUMÉRICAS
# gender es NO numérica → QUITAR
df_smote$gender <- NULL

# CONVERTIR TODO LO DEMÁS A NUMÉRICO
df_smote <- df_smote |> mutate(across(everything(), as.numeric))

#############################################################
# 4. APLICAR SMOTE SOLO A LUNG_CANCER (OBJETIVO)
#############################################################

X <- df_smote[, setdiff(names(df_smote), "lung_cancer")]
y <- df_smote$lung_cancer

smote_out <- SMOTE(X, y, K=5)

df_bal <- smote_out$data

# Convertir de nuevo la clase a factor
df_bal$lung_cancer <- factor(df_bal$class, labels=c("NO", "YES"))
df_bal$class <- NULL

# Comprobar balanceo
print(table(df_bal$lung_cancer))
print(prop.table(table(df_bal$lung_cancer)))












#############################################################
# 1. LIBRERÍAS
#############################################################
library(tidyverse)
library(ggplot2)
library(janitor)
library(smotefamily)

#############################################################
# 2. CARGAR Y LIMPIAR
#############################################################
df <- read.csv("survey lung cancer.csv", sep=";") |> clean_names()

#############################################################
# 3. CONVERTIR VARIABLES PARA EDA
#############################################################

binarias <- c(
  "smoking","yellow_fingers","anxiety","peer_pressure",
  "chronic_disease","fatigue","allergy","wheezing",
  "alcohol_consuming","coughing","shortness_of_breath",
  "swallowing_difficulty","chest_pain"
)

df_plot <- df

for(col in binarias){
  df_plot[[col]] <- factor(df_plot[[col]], levels=c(1,2), labels=c("No","Sí"))
}

df_plot$gender <- factor(df_plot$gender)
df_plot$lung_cancer <- factor(df_plot$lung_cancer)

#############################################################
# 4. TABLAS DESCRIPTIVAS
#############################################################

# Frecuencias de cancer
print(table(df_plot$lung_cancer))

# Alcohol vs cancer
print(table(df_plot$alcohol_consuming, df_plot$lung_cancer))

# Tos vs cancer
print(table(df_plot$coughing, df_plot$lung_cancer))

# Combinado
print(table(df_plot$alcohol_consuming, df_plot$coughing, df_plot$lung_cancer))

#############################################################
# 5. VISUALIZACIONES ROSAS
#############################################################

# HISTOGRAMA EDAD
print(
  ggplot(df_plot, aes(age)) +
    geom_histogram(binwidth=3, fill="pink", color="black") +
    theme_minimal() +
    labs(title="Histograma de Edad")
)

# ALCOHOL vs CANCER
print(
  ggplot(df_plot, aes(x=alcohol_consuming, fill=lung_cancer)) +
    geom_bar(position="fill") +
    scale_fill_manual(values=c("lightblue","pink")) +
    theme_minimal() +
    labs(title="Alcohol y Diagnóstico")
)

# TOS vs CANCER
print(
  ggplot(df_plot, aes(x=coughing, fill=lung_cancer)) +
    geom_bar(position="fill") +
    scale_fill_manual(values=c("lightblue","pink")) +
    theme_minimal() +
    labs(title="Tos y Diagnóstico")
)

# COMBINACIÓN
print(
  ggplot(df_plot, aes(x=alcohol_consuming, fill=coughing)) +
    geom_bar(position="dodge") +
    scale_fill_manual(values=c("lightblue","pink")) +
    theme_minimal() +
    labs(title="Alcohol + Tos")
)
#############################################################
# 6. PREPARAR SMOTE (SOLO NUMÉRICO)
#############################################################

df_num <- df
df_num$gender <- NULL
df_num$lung_cancer <- ifelse(df_num$lung_cancer=="YES", 1, 0)
df_num <- df_num |> mutate(across(everything(), as.numeric))

X <- df_num[, setdiff(names(df_num), "lung_cancer")]
y <- df_num$lung_cancer

smote_out <- SMOTE(X, y, K=5)
df_bal <- smote_out$data
df_bal$lung_cancer <- factor(df_bal$class, labels=c("NO","YES"))
df_bal$class <- NULL

#############################################################
# 7. VISUALIZACIONES POST-SMOTE (ROSAS)
#############################################################
df_bal$alcohol_consuming <- factor(df_bal$alcohol_consuming, levels=c(1,2), labels=c("No","Sí"))
df_bal$coughing <- factor(df_bal$coughing, levels=c(1,2), labels=c("No","Sí"))

print(
  ggplot(df_bal, aes(x=alcohol_consuming, fill=lung_cancer)) +
    geom_bar(position="fill") +
    scale_fill_manual(values=c("lightblue","pink")) +
    theme_minimal() +
    labs(title="Alcohol vs Cáncer (Balanceado)")
)

print(
  ggplot(df_bal, aes(x=coughing, fill=lung_cancer)) +
    geom_bar(position="fill") +
    scale_fill_manual(values=c("lightblue","pink")) +
    theme_minimal() +
    labs(title="Tos vs Cáncer (Balanceado)")
)

#############################################################
# 8. MODELO LOGÍSTICO (HIPÓTESIS)
#############################################################

# Lo hacemos sobre el dataset balanceado
df_mod <- df_bal
df_mod$alcohol_consuming <- as.numeric(df_mod$alcohol_consuming=="Sí")
df_mod$coughing <- as.numeric(df_mod$coughing=="Sí")

modelo <- glm(lung_cancer ~ alcohol_consuming + coughing + alcohol_consuming*coughing,
              data=df_mod, family="binomial")

#TABLAS CON SMOTE
print(summary(modelo))

table(df_bal$alcohol_consuming, df_bal$lung_cancer)
table(df_bal$coughing, df_bal$lung_cancer)
table(df_bal$alcohol_consuming, df_bal$coughing, df_bal$lung_cancer)
