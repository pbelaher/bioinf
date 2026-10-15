install.packages("smotefamily")
install.packages("dplyr")
install.packages("ggplot2")

library(dplyr)
library(ggplot2)
library(smotefamily)

library(smotefamily)
library(dplyr)

# 1. Crear copia del dataset
df_smote <- datos

# 2. Convertir variable objetivo a 0/1
df_smote$LUNG_CANCER <- ifelse(df_smote$LUNG_CANCER == "YES", 1, 0)

# 3. Eliminar la única columna NO numérica (GENDER)
df_smote <- df_smote %>% select(-GENDER)

# 4. Convertir todo lo demás a numérico SIN warnings
df_smote <- df_smote %>% mutate(across(everything(), as.numeric))

# 5. Aplicar SMOTE con smotefamily
set.seed(123)
resultado_smote <- SMOTE(
  df_smote[ , !(names(df_smote) %in% "LUNG_CANCER")],
  df_smote$LUNG_CANCER,
  K = 5
)

df_balanceado <- resultado_smote$data

# 6. Renombrar la clase
colnames(df_balanceado)[ncol(df_balanceado)] <- "LUNG_CANCER"

df_balanceado <- df_balanceado %>%
  mutate(
    ALCOHOL.CONSUMING = factor(ALCOHOL.CONSUMING, levels = c(1, 2),
                               labels = c("No Alcohol", "Sí Alcohol")),
    COUGHING = factor(COUGHING, levels = c(1, 2),
                      labels = c("No Tos", "Sí Tos")),
    LUNG_CANCER = factor(LUNG_CANCER, levels = c(0, 1),
                         labels = c("No Cáncer", "Cáncer"))
  )

table(df_balanceado$LUNG_CANCER)

saveRDS(df_balanceado, "df_balanceado.rds")

