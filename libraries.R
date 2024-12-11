# libraries.R
suppressWarnings({
    # Instalar las librerías si no están instaladas
     install.packages('tidyverse')
#     install.packages('tidymodels')
#     install.packages('kableExtra')
#     install.packages('readxl')
#     install.packages('BSDA')
#     install.packages('reshape2')
#     install.packages('robustbase')
#     install.packages('knitr')
#     install.packages('lars')
#     install.packages('caret')
#     install.packages('glmnet')
#     install.packages('factoextra')
    
    # Cargar las librerías
    library(tidyverse)
    library(tidymodels)
    library(dplyr)
    library(kableExtra)
    library(readxl)
    library(stats)
    library(BSDA)
    library(ggplot2)
    library(reshape2)  # Correlación calor
    library(GGally)
    library(robustbase)
    library(knitr)
    library(lars)  # Específico de lars
    library(caret)  # Ver para qué sirve
    library(glmnet)  # Modelo lasso
})