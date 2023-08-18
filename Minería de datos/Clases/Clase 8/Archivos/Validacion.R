library(caret)

setwd("C:/Users/Sebastian/Documents/R")

datos <- read.csv("DatosPaises.csv", sep = ";", header = TRUE)
datos <- datos[,2:30]


## k-fold cross-validation ##
train.control1 <- trainControl(method = "cv",
                              number = 10)

# Entrenamiento del modelo de regresion
modelo1 <- train(PIB ~ .,
                data = datos,
                method = "lm",
                trControl = train.control1)
print(modelo1)
summary(modelo1)


## k-fold cross-validation repetido ##
train.control2 <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 5)

# Entrenamiento del modelo de regresion
modelo2 <- train(PIB ~.,
                 data = datos,
                 method = "lm",
                 trControl = train.control2)
print(modelo2)
summary(modelo2)


## Leave one out cross validation ##
train.control3 <- trainControl(method = "LOOCV")

# Entrenamiento del modelo de regresion
modelo3 <- train(PIB ~.,
                 data = datos,
                 method = "lm",
                 trControl = train.control3)
print(modelo3)
summary(modelo3)