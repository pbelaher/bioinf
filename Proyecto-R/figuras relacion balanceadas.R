df_balanceado <- readRDS("df_balanceado.rds")

#TODO ESTO ES PARA LA FIGURA QUE RELACIONA MIS TRES VARIABLES
library(dplyr)
library(ggplot2)

# 1. Balancear datos mediante oversampling (sin errores)
yes <- datos %>% filter(LUNG_CANCER == "YES")
no  <- datos %>% filter(LUNG_CANCER == "NO")

set.seed(123)
no_over <- no %>% sample_n(nrow(yes), replace = TRUE)

df_balanceado <- bind_rows(yes, no_over)

# 2. Convertir variables a factores claros
df_balanceado <- df_balanceado %>%
  mutate(
    ALCOHOL.CONSUMING = factor(ALCOHOL.CONSUMING,
                               levels = c(1, 2),
                               labels = c("No Alcohol", "Sí Alcohol")),
    COUGHING = factor(COUGHING,
                      levels = c(1, 2),
                      labels = c("No Tos", "Sí Tos")),
    LUNG_CANCER = factor(LUNG_CANCER,
                         levels = c("NO", "YES"),
                         labels = c("No Cáncer", "Cáncer"))
  )

# 3. Crear la combinación Alcohol + Tos (para el gráfico)
df_balanceado$grupo <- interaction(
  df_balanceado$ALCOHOL.CONSUMING,
  df_balanceado$COUGHING,
  sep = " & "
)

levels(df_balanceado$grupo) <- c(
  "No Alcohol & No Tos",
  "No Alcohol & Sí Tos",
  "Sí Alcohol & No Tos",
  "Sí Alcohol & Sí Tos"
)

# 4. Gráfico final para el póster
ggplot(df_balanceado, aes(x = grupo, fill = LUNG_CANCER)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("#A6CEE3", "lightpink")) +
  labs(
    title = "Relación entre Alcohol, Tos y Cáncer (Datos Balanceados)",
    x = "Combinación de Factores",
    y = "Proporción"
  ) +
  theme_minimal(base_size = 15) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

#ALCOHOL VS CANCER BALANCEADO
ggplot(df_balanceado, aes(x = ALCOHOL.CONSUMING, fill = LUNG_CANCER)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("#A6CEE3", "lightpink")) +
  labs(
    title = "Relación entre Alcohol y Cáncer (Datos Balanceados)",
    x = "Consumo de Alcohol",
    y = "Proporción"
  ) +
  theme_minimal(base_size = 15)

#TOS VS CANCER BALANCEADO
ggplot(df_balanceado, aes(x = COUGHING, fill = LUNG_CANCER)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("#A6CEE3", "lightpink")) +
  labs(
    title = "Relación entre Tos y Cáncer (Datos Balanceados)",
    x = "Tos Frecuente",
    y = "Proporción"
  ) +
  theme_minimal(base_size = 15)



