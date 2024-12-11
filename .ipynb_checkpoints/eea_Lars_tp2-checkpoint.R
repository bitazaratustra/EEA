---
title: "Trabajo Práctico Nro.2"
subtitle: "Enfoque Estadístico del Aprendizaje"
author: "Baldaseroni,Esteban; Conde, M. Cecilia, Lopez, Juan Jose"
date: "10/12/2024"
output:
  html_document:
    toc: true
    code_folding: show
    toc_float: true
    df_print: paged
    theme: flatly
    code_download: true
  pdf_document:
    toc: true
editor_options: 
  markdown: 
    wrap: 72
---

# 1. Configuraciones Generales de R

```{r Configuracion General}
require("knitr")
knitr::opts_chunk$set(echo = TRUE)
# indica desde dónde instalar paquetes
options(repos = c(CRAN = "http://cran.rstudio.com")) 
```

```{r Configuracion General2, message=FALSE, warning=FALSE}
# Seteo de directorio de trabajo
setwd("C:/Users/mconde/Documents/EEA-Tps/LARS")

```

```{r Librerias, message=FALSE, warning=FALSE}
#librerías
library(tidyverse)
library(tidymodels)
library(dplyr)
library(kableExtra)
library(readxl)
library(stats)
library(BSDA)
library(ggplot2)
library(reshape2) #correlacion calor
library(GGally)
library(robustbase)
library(knitr)
library(lars) #especifica de lars
library(caret) # ver para qu esirve
library(glmnet) #modelo lasso


```

# 2. Lectura de Datos y armado de dataset de análisis.

El objetivo del presente documento es armar un modelo de estimación de
total de gases efecto invernadero para los países de sudamérica. Se
utilizar la técnica LARS.

Se utilizará un dataset disponible públicamente de Kaggle “[Agri-food
CO2 emission dataset -
Forecast](https://www.kaggle.com/datasets/alessandrolobello/agri-food-co2-emission-dataset-forecasting-ml)”
que contiene múltiples variables relevantes para el análisis
provenientes de la fusión de datos de Food and Agriculture Organization
(FAO) y el Intergovernmental Panel of Climate Change (IPCC), desde el
año 1990 hasta el 2020, para diferentes países del mundo.

El dataset se encuentra limpio y preprocesado, por lo que se filtrarán
los países relevantes para nuestro estudio. A continuación se procederá
con un breve análisis exploratorio y además, se normalizarán los datos
para asegurar que todas las variables estén en la misma escala.

**Estructura del dataset orginal**: El dataset consta de 6965 filas y 31
columnas. Incluye diversas variables que permiten realizar análisis
detallados sobre las emisiones de CO2 y su relación con el sector
agroalimentario. Algunas de las variables incluyen

```{r Lectura de datos}
#Leer datos
df<-read.csv("Agrofood_co2_emission.csv")
view(df)

```

```{r Paises de sudamerica}
# Crear un vector con los países de Sudamérica
paises_sudamerica <- c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", 
                       "Ecuador", "Guyana", "Paraguay", "Peru", "Suriname", 
                       "Uruguay", "Venezuela")

# Ver la lista de países
print(paises_sudamerica)
```

```{r Dataset de analisis}
# Filtrar el data frame por los países de Sudamérica
df_sudamerica <- df %>% 
  filter(Area %in% paises_sudamerica)
head(df_sudamerica)
# Guardar el dataframe filtrado en un archivo CSV
#write.csv(df_sudamerica, "df_sudamerica.csv", row.names = FALSE)

```

# 3. EDA

Estructura de dataset del estudio: Se incluye los paises de sudamerica
reducionedo la cantidad de registros a 310.

```{r}
# Resumen inicial de los datos
cat("Resumen de la estructura del dataset:\n")
str(df_sudamerica)

cat("\nResumen estadístico de las variables numéricas:\n")
summary(df_sudamerica)

cat("\nDimensiones del dataset (filas y columnas):\n")
dim(df_sudamerica)
```

```{r}
# Verificar datos faltantes
cat("\nConteo de valores faltantes por columna:\n")
missing_data <- sapply(df, function(x) sum(is.na(x)))
head(missing_data)
```

```{r}
# Gráfico de barras para visualizar el total de emision
ggplot(df_sudamerica, aes(x = Area, y = total_emission, fill = Area)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Emision de Gases Efecto invernadero", 
       x = "Paises", 
       y = "Emision Total") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

```{r}
# Análisis de variables numéricas
num_vars <- df_sudamerica %>% select(where(is.numeric))
cat("\nCorrelación entre variables numéricas:\n")
cor_matrix <- cor(num_vars, use = "complete.obs")

```

```{r}
# Convertir la matriz de correlación a formato largo
cor_data <- melt(cor_matrix)
# Crear el mapa de calor
ggplot(cor_data, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name = "Correlación") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 8), # Ajustar tamaño de texto
    axis.text.y = element_text(size = 5), # Ajustar tamaño de etiquetas en Y
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10) # Controlar los márgenes
  )
  labs(title = "Mapa de Calor de Correlación",
       x = "", y = "")
```

# Modelo Lineal

Vamos a probar un modelo lineal que incluya todas las variables (excepto
Fires.in.organic.soils y Area) y obtener las estimaciones de los
parámetros junto a su p-valor e intervalo de confianza.

```{r}
data <- df_sudamerica %>% 
  select(where(is.numeric))%>% 
  select(-Fires.in.organic.soils) #La saque porque son todos ceros
head(data)
```

```{r}
modelo_lineal = data %>% lm(formula = total_emission~., data = .)
#Coeficientes
lineal_coef = modelo_lineal %>% tidy(conf.int=TRUE)
print(lineal_coef)

```

```{r}
lineal_coef %>% filter(!is.na(estimate)) %>% 
  ggplot(., aes(term, estimate))+
  geom_point(color = "forestgreen")+
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), color = "forestgreen")+
  geom_hline(yintercept = 0, lty = 2, color = "black") +
  labs(title = "Coeficientes de la regresión lineal", x="", y="Estimación e Int. Confianza") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90))

```

```{r}
#Graficamos los p-valores de mayor a menor para evaluar la significatividad individual de los coeficientes estimados.

lineal_coef %>% filter(!is.na(estimate)) %>% 
  ggplot(., aes(reorder(term, -p.value), p.value, fill=p.value))+
  geom_bar(stat = 'identity', aes(fill=p.value))+
  geom_hline(yintercept = 0.05) +
  labs(title = "P-valor de los regresores", x="", y="P-valor") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90)) + 
  scale_fill_gradient2(high='firebrick', low = 'forestgreen', mid='yellow2',midpoint = 0.5 )
```

#Rescalado Variables

```{r}
# Reescalamos las variables numericas, menos la variable predictora
data_scaled = data %>% mutate_at(vars(-total_emission), scale)
head(data_scaled)
```

Dado que las variables están escaladas, los coeficientes no están en la
misma escala que los datos originales. Esto puede dificultar la
interpretación directa, pero sigue siendo útil para evaluar la
importancia relativa de los predictores.

```{r}
# Modelo lineal
# Ajustar modelo lineal con datos escalados de entrenamiento
modelo_lineal_scaled <- lm(total_emission ~ ., data = data_scaled)

# Extraer coeficientes con intervalos de confianza
lineal_coef_scaled <- modelo_lineal_scaled %>% tidy(conf.int = TRUE)

# Imprimir coeficientes
print(lineal_coef_scaled)

```

```{r}
summary(modelo_lineal_scaled)
```

```{r}
lineal_coef_scaled %>% filter(!is.na(estimate)) %>% 
  ggplot(., aes(term, estimate))+
  geom_point(color = "forestgreen")+
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), color = "forestgreen")+
  geom_hline(yintercept = 0, lty = 2, color = "black") +
  labs(title = "Coeficientes de la regresión lineal", x="", y="Estimación e Int. Confianza") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90))
```

```{r}
#Graficamos los p-valores de mayor a menor para evaluar la significatividad individual de los coeficientes estimados.

lineal_coef_scaled %>% filter(!is.na(estimate)) %>% 
  ggplot(., aes(reorder(term, -p.value), p.value, fill=p.value))+
  geom_bar(stat = 'identity', aes(fill=p.value))+
  geom_hline(yintercept = 0.05) +
  labs(title = "P-valor de los regresores", x="", y="P-valor") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90)) + 
  scale_fill_gradient2(high='firebrick', low = 'forestgreen', mid='yellow2',midpoint = 0.5 )
```

```{r}
modelos_lineales = list(lineal = modelo_lineal, lineal_escalado = modelo_lineal_scaled)
map_dfr(.x = modelos_lineales, .f = glance, .id="modelo") %>% 
  select(modelo, r.squared, adj.r.squared, p.value)
```

```{r}
# Particion del dataset en train y test, #data_scaled es el data escalado
# Suponiendo que `data` es el conjunto de datos ya escalado
train_test <- data %>% initial_split(prop = 0.7)

# Extraer los datos de entrenamiento y prueba
train <- training(train_test) #70% train
test <- testing(train_test) #30% para test

# Visualizar los tamaños de los conjuntos
cat("Tamaño del conjunto de entrenamiento:", nrow(train), "\n")
cat("Tamaño del conjunto de prueba:", nrow(test), "\n")
```

```{r}
# Elimina filas con NA en `train`
train <- na.omit(train)
```

```{r}
# Vector de total emision
data_emision <- train$total_emission
# Matriz con los regresores
data_mtx = model.matrix(total_emission~., data = train)

# Dimensiones del vector de emisión
length(data_emision)

# Dimensiones de la matriz de regresores
dim(data_mtx)
```

```{r}
# Modelo Lasso
library(glmnet)
lasso.mod = glmnet(x = data_mtx, # Matriz de regresores
                   y = data_emision, #Vector de la variable a predecir
                   alpha = 1,
                   # Indicador del tipo de regularizacion
                   nlambda = 100,
                   standardize = F) 
# aplicamos la función tidy para obtener los coeficientes del modelo                 
lasso_coef = lasso.mod %>% tidy() %>% arrange(step)
lasso_coef
```

```         
```

```{r}
plot(lasso.mod, 'lambda')
```

```{r}
plot(lasso.mod)
```

```{r}
library(cowplot)

# Gráfico de coeficientes en función del lambda con intercepto
g1 = lasso_coef  %>% 
  ggplot(., aes(log(lambda), estimate, group=term, color=term)) +
  geom_line() +
  theme_bw()  +
  theme(legend.position = 'none') +
  labs(title="Lasso con Intercepto",  y="Coeficientes")
# Gráfico de coeficientes en función del lambda sin intercepto
g2 = lasso_coef %>% 
  filter(term!='(Intercept)') %>% 
  ggplot(., aes(log(lambda), estimate, group=term, color=term)) +
  geom_line() +
  theme_bw()  +
  theme(legend.position = 'none') +
  labs(title="Lasso sin Intercepto", y="Coeficientes")
# armamos la grilla con ambos gráficos
plot_grid(g1,g2)
```

```{r}
# Seleccionamos los terminos que sobreviven para valores altos de lambda
terminos_sobrevientes = lasso_coef %>% 
  filter(log(lambda)>17, term != "(Intercept)") %>%
  select(term) %>% 
  distinct() %>% 
  pull()
# Graficamos
lasso_coef %>% filter(term %in% terminos_sobrevientes) %>% 
  ggplot(., aes(log(lambda), estimate, group=term, color=term)) +
  geom_line(size=1) +
  geom_hline(yintercept = 0, linetype='dashed') +
  theme_bw() +
  labs(title="Lasso sin Intercepto", y="Coeficientes", subtitle= "\"Mejores\" variables") +
  scale_color_brewer(palette = 'Set1')
```

```{r}
# Modelo Lasso
library(glmnet)
lasso.mod_1 = glmnet(x = data_mtx, # Matriz de regresores
                   y = data_emision, #Vector de la variable a predecir
                   alpha = 1, # Indicador del tipo de regularizacion
                   standardize = TRUE) # Que esta haciendo este parametro ?
# aplicamos la función tidy para obtener los coeficientes del modelo                 
lasso_coef_1 = lasso.mod_1 %>% tidy() %>% arrange(step)
lasso_coef_1
```

```{r}
cv_lasso_1 <- cv.glmnet(x = data_mtx, y = data_emision, alpha = 1)
plot(cv_lasso_1)  # Gráfico de validación cruzada
best_lambda <- cv_lasso_1$lambda.min  # Mejor lambda
```

```{r}
plot(lasso.mod_1, 'lambda')
```

```{r}
plot(lasso.mod_1)
```

```{r}
# Gráfico de coeficientes en función del lambda con intercepto
g1_1 = lasso_coef_1 %>% 
  ggplot(., aes(log(lambda), estimate, group=term, color=term)) +
  geom_line() +
  theme_bw()  +
  theme(legend.position = 'none') +
  labs(title="Lasso con Intercepto",  y="Coeficientes")
# Gráfico de coeficientes en función del lambda sin intercepto
g2_1=lasso_coef_1 %>% 
  filter(term!='(Intercept)') %>% 
  ggplot(., aes(log(lambda), estimate, group=term, color=term)) +
  geom_line() +
  theme_bw()  +
  theme(legend.position = 'none') +
  labs(title="Lasso sin Intercepto", y ="Coeficientes")
# armamos la grilla con ambos gráficos
plot_grid(g1_1,g2_1)
```

```{r}
# Cargar el paquete
library(RColorBrewer)
# Seleccionamos los terminos que sobreviven para valores altos de lambda
terminos_sobrevientes = lasso_coef_1 %>% 
  filter(log(lambda)>13.1, term != "(Intercept)") %>%
  select(term) %>% 
  distinct() %>% 
  pull()
# Graficamos
lasso_coef_1 %>% 
  filter(term %in% terminos_sobrevientes) %>% 
  ggplot(., aes(log(lambda), estimate, group=term, color=term)) +
  geom_line(size=1)  +
  scale_colour_manual(values=rep(brewer.pal(8,"Set1"),times=2))+
#   scale_color_brewer(palette = mycolors) +
  geom_hline(yintercept = 0, linetype='dashed') +
  theme_bw() +
  labs(title="Lasso sin Intercepto", y="Coeficientes", subtitle= "\"Mejores\" variables")
```

# Modelos LARS

```{r}
# Definir y como la columna de la variable objetivo
y <- data_scaled[["total_emission"]]

# Definir X como una matriz de las variables predictoras
X <- as.matrix(data_scaled[, -which(names(data_scaled) == "total_emission")])


```

```{r}
# Dividir los datos en entrenamiento y prueba (70% entrenamiento, 30% prueba)
set.seed(42)  # Para reproducibilidad
trainIndex <- createDataPartition(y, p = 0.7, list = FALSE)
X_train <- X[trainIndex, ]
y_train <- y[trainIndex]
X_test <- X[-trainIndex, ]
y_test <- y[-trainIndex]
```

```{r}
# Comprobar el tipo de X_train y y_train
str(X_train)
```

```{r}
str(y_train)
```

```{r}
X_train <- as.matrix(X_train)
y_train <- as.numeric(y_train)

```

```{r}
dim(X_train)
length(y_train)

```

```{r}
# Usando cv.lars sin el argumento index
cv.lars(X_train,y_train, K = 10, trace = FALSE, plot.it = TRUE, se = TRUE, type = "lar", mode = "step")

```

```         
```

```{r Modelo Lars}

# Ahora ajustamos el modelo LARS sin valores faltantes
modelo_lars <- lars(X_train, y_train, type = "lar") # "lar" selecciona el tipo de regularización
summary(modelo_lars)

```

```{r}
plot.lars

```

```{r}

# Realizar predicciones en el conjunto de prueba
y_pred <- predict(modelo_lars, X_test, type = c("fit","coefficientes"), mode = "step")
y_pred <- predict(modelo_lars, X_test) 
```

```{r}
summary(modelo_lars, sigma2 = NULL)
```

```{r}
# Evaluación del modelo
mse <- mean((y_test - y_pred)^2)
cat("Mean Squared Error (MSE):", mse, "\n")

# Coeficientes de las variables seleccionadas
print(coef(modelo_lars))



```

```{r}
mse <- mean((y_test - y_pred)^2)
r_squared <- 1 - sum((y_test - y_pred)^2) / sum((y_test - mean(y_test))^2)
```

```{r}
plot(y_test, y_pred)
abline(0, 1, col = "red") # Línea diagonal para referencia
```

```{r}
summary(modelo_lars)
```

Este resumen de tu modelo LARS/LASSO muestra una serie de métricas clave
para cada etapa de ajuste del modelo, representando cómo se va
construyendo el modelo a medida que se agregan variables. Aquí tienes
una explicación detallada de cada columna:

Columnas en el Resumen: Df (Degrees of Freedom): Indica el número de
variables o parámetros en el modelo en esa etapa. A medida que avanzas
en las etapas, el modelo incluye más variables. Rss (Residual Sum of
Squares): Es la suma de los residuos al cuadrado. A medida que se
agregan variables, este valor generalmente disminuye, ya que el modelo
ajusta mejor los datos. Sin embargo, si comienza a estabilizarse o
disminuir muy lentamente, puede ser una señal de que las variables
adicionales están contribuyendo marginalmente. Cp (Mallow's Cp
Statistic): Es una métrica utilizada para evaluar la bondad de ajuste y
la complejidad del modelo. Un valor de Cp cercano al número de variables
en el modelo indica un buen equilibrio entre ajuste y parsimonia (evitar
sobreajuste). Cuando el valor de Cp es bajo, es una señal de que el
modelo es adecuado; cuando es alto, puede sugerir que el modelo es
demasiado complejo o sobreajustado. Observaciones: Al inicio, el Rss
disminuye rápidamente a medida que se agregan variables, lo cual es
esperado en los primeros pasos de ajuste. En etapas avanzadas, el Cp se
vuelve negativo, lo cual puede ser un indicio de sobreajuste,
especialmente cuando todos los residuos han sido explicados
completamente y el Rss es prácticamente cero (0.0000e+00). Esto indica
que el modelo ha llegado a un punto en el que ha ajustado perfectamente
los datos de entrenamiento, lo cual puede no ser deseable para datos
nuevos o de prueba. Las etapas finales con Cp negativos son puntos donde
el modelo está posiblemente sobreajustado. Generalmente, elegir una
etapa anterior a estas, donde el Cp se estabiliza o se acerca al número
de variables, podría representar un buen modelo con buen ajuste sin
sobreajuste. Selección de un Modelo Óptimo Para seleccionar el modelo
óptimo, podrías considerar la etapa en la que el valor de Cp es bajo y
cercano al número de variables agregadas en esa etapa. Esto te ofrece un
buen compromiso entre ajuste y simplicidad.

Si necesitas identificar las variables seleccionadas y sus coeficientes
en esa etapa específica, puedes usar el índice de esa etapa para extraer
los coeficientes correspondientes.

Sí, basándote en el resumen, la etapa 27 parece una buena elección para
el modelo óptimo. En esta etapa, el Residual Sum of Squares (Rss) es
bastante bajo y el Cp se encuentra en un rango aceptable (aún positivo y
cercano a cero). Además, en esta etapa se han agregado 24 variables, lo
que sugiere que el modelo es lo suficientemente complejo para capturar
la variabilidad de los datos sin llegar al sobreajuste que se observa en
las etapas posteriores (donde el Cp se vuelve negativo).

Extraer los Coeficientes en la Etapa 27 Para obtener los coeficientes
correspondientes a la etapa 27, puedes hacer lo siguiente:

```{r}
# Extraer los coeficientes en la etapa 27
coef_etapa_27 <- coef(modelo_lars)[27, ]

# Filtrar solo los coeficientes diferentes de cero y asociarlos con los nombres de variables
coef_no_cero_etapa_27 <- coef_etapa_27[coef_etapa_27 != 0]
variable_no_cero_etapa_27 <- variable_names[coef_etapa_27 != 0]

# Crear un data frame con los nombres de las variables y sus coeficientes en la etapa 27
coef_data_etapa_27 <- data.frame(Variable = variable_no_cero_etapa_27, Coeficiente = coef_no_cero_etapa_27)

# Ordenar los coeficientes en orden descendente por valor absoluto
coef_data_etapa_27 <- coef_data_etapa_27[order(abs(coef_data_etapa_27$Coeficiente), decreasing = TRUE), ]

# Mostrar los coeficientes ordenados con sus variables asociadas en la etapa 27
print(coef_data_etapa_27)

```

El coef_data_etapa_27 te dará un data frame con las variables
seleccionadas en la etapa 27 y sus respectivos coeficientes, ordenados
en función de su influencia. Este conjunto de variables y coeficientes
es tu modelo final, que debería ofrecer un buen equilibrio entre ajuste
y simplicidad.

```{r}
# Realizar validación cruzada
cv_resultado <- cv.lars(X_train, y_train, K = 10, type = "lasso")

# Graficar el error de validación para cada etapa
#plot(cv_resultado, main = "Error de Validación Cruzada en Diferentes Etapas")

# Identificar la etapa óptima según la validación cruzada
mejor_etapa_cv <- which.min(cv_resultado$cv)
cat("La mejor etapa según validación cruzada es:", mejor_etapa_cv, "\n")

```

```{r}
# Calcular R² ajustado para cada etapa
R2_ajustado_etapas <- sapply(1:ncol(coef(modelo_lars)), function(s) {
  y_pred <- predict(modelo_lars, X_train, s = s, type = "fit")$fit
  SSE <- sum((y_train - y_pred)^2)
  SST <- sum((y_train - mean(y_train))^2)
  R2 <- 1 - (SSE / SST)
  R2_ajustado <- 1 - ((1 - R2) * (nrow(X_train) - 1) / (nrow(X_train) - s - 1))
  return(R2_ajustado)
})

# Seleccionar la etapa con el R² ajustado más alto
mejor_etapa_R2 <- which.max(R2_ajustado_etapas)
cat("La mejor etapa según R² ajustado es:", mejor_etapa_R2, "\n")

```

```{r}
# Graficar trayectorias de coeficientes
plot(modelo_lars, breaks = FALSE)
title("Trayectoria de Coeficientes en LARS/LASSO")

```
