
df_balanceado <- readRDS("df_balanceado.rds")

#limpio filas defectosas creadas por SMOTE
df_balanceado_clean <- na.omit(df_balanceado)
nrow(df_balanceado) #filas antes de limpiar= 504
nrow(df_balanceado_clean)#filas tras limpiar = 399

#modelo logístico
modelo_logit_balanceado <- glm(
  LUNG_CANCER ~ ALCOHOL.CONSUMING + COUGHING,
  data = df_balanceado_clean,
  family = binomial
)

summary(modelo_logit_balanceado)

#curva ROC rosa
library(pROC)
library(ggplot2)

# Probabilidades
df_balanceado_clean$prob <- predict(modelo_logit_balanceado, type = "response")

# ROC
roc_obj <- roc(df_balanceado_clean$LUNG_CANCER, df_balanceado_clean$prob)

# AUC
auc_value <- auc(roc_obj)
print(paste("AUC =", round(auc_value, 3)))

roc_df <- data.frame(
  tpr = roc_obj$sensitivities,
  fpr = 1 - roc_obj$specificities
)

ggplot(roc_df, aes(x = fpr, y = tpr)) +
  geom_line(color = "#FF69B4", size = 1.2) +           
  geom_abline(linetype = "dashed", color = "#F8BBD0") +
  labs(
    title = "Curva ROC - Modelo Logístico (Datos Balanceados y Limpios)",
    subtitle = paste("AUC =", round(auc_value, 3)),
    x = "1 - Especificidad (FPR)",
    y = "Sensibilidad (TPR)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", color = "#AD1457"),
    plot.subtitle = element_text(color = "#C2185B")
  
  )


print(auc_value)

