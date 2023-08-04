# Instalamos los paquetes necesarios, en caso que no los tengamos instaladas
#install.packages("class")

# Cargamos las librerias que utilizaremos
library(class) # knn no optimizado para grandes datasets. Para esta caso está OK

# Indicamos el directorio de trabajo
setwd(paste0(
  '/home/nestorprr/Documentos/Diplomado_Big_Data_Data_Science/',
  'Minería de datos/Clases/Clase 5'
))


# Cargamos la base de datos
datos <- read.csv(
  file = "../../Evaluaciones/Evaluación 2/SouthGermanCredit.csv",
  sep = ";",
  header=TRUE
)
head(datos)


# En este ejemplo solo utilizaremos cuatro variables explicativas, por simplicidad
# La cantidad de variables a utilizar es algo que usted debe analizar en su evaluacion

# Elegiremos como variables exogenas:
# age: variable continua, la podemos usar tal cual esta en la base de datos
# housing: variable categorica con 3 niveles, debe ser transformada
# job: variable ordinal numerica, la podemos usar tal cual esta en la base de datos
# foreign: variable binaria, la podemos usar tal cual esta en la base de datos

# Ademas usaremos obviamente la variable endógena a clasificar:
# credit: variable binaria, la podemos usar tal cual esta en la base de datos

datos_small <- as.data.frame(cbind(
  age = datos$age,
  housing_free = (datos$housing == 1), # Se transforma housing a variable binaria
  housing_rent = (datos$housing == 2), # housing = 3 sería (housing_free == 0 & housing_rent == 0)
  job = datos$job,
  foreign = datos$foreign,
  credit = datos$credit
))


# Se crea una función para normalizar los datos
normalize <- function(x) { return ((x - min(x)) / (max(x) - min(x))) }

# Se normalizan todos los datos ejecutando nomalize con lapply
datos_norm <- as.data.frame(lapply(datos_small, normalize))


# Dividimos los datos en entrenamiento (75%) y validacion (25%)
# En caso del modelo KNN los datos de entrenamiento corresponden a los registros
#con valor conocido de la variable endógena y los datos de validación son los
#registros cuyo valor de variable endógena hay que predecir.
set.seed(123)
subset <- sample(1:nrow(datos), size = 0.75 * nrow(datos), replace = FALSE)

# Creamos las bases de datos a utilizar
datos_train <- datos_norm[subset,1:5]
label_train <- datos_norm[subset,6]
datos_test <- datos_norm[-subset,1:5]
label_test <- datos_norm[-subset,6]


# Modelo KNN con dos K distintos
# La función class::knn trabaja con distancia Euclidiana, en caso de empate en 
#la clasificación se decide al azar y, en caso de distancias similares para dos
#o más datos en la posición K-ésica, se incluyen todos estos valores
modelo_k3 <- knn(
  train = datos_train, # Datos de entrenamiento
  test = datos_test, # Registros a clasificar
  cl = label_train, # Valores de la variable endógena para los datos de entrenamiento
  k = 3 # Cantidad de vecinos cercanos
)

modelo_k5 <- knn(
  train = datos_train,
  test = datos_test,
  cl = label_train,
  k = 5
)


# Matriz de confusion
table(modelo_k3, label_test)
table(modelo_k5, label_test)

# Accuracy
sum(label_test == modelo_k3)/length(label_test)
sum(label_test == modelo_k5)/length(label_test)
# Una estrategia para definir el valor de K puede ser al tener mayor precisión


# Paquetes para matrices de confusion
library(caret)
library(e1071)

# Matriz de confusion
confusionMatrix(modelo_k3,as.factor(label_test))
confusionMatrix(modelo_k5,as.factor(label_test))


# Accuracy para distintos K

i = 1
k.optm = 1
for (i in 1:25){
  knn.mod <- knn(
    datos_train,
    datos_test,
    label_train,
    k = i
  )
  k.optm[i] <- sum(label_test == knn.mod)/length(label_test)
  cat(i, '=', k.optm[i],'\n') 
}

plot(k.optm)


# Corramos otra vez el modelo con K = 4
modelo_k_4 <- knn(
  train = datos_train,
  test = datos_test,
  cl = label_train,
  k = 4)

sum(label_test == modelo_k_4)/length(label_test)
# Es posible que las predicciones del modelo cambien con cada intento porque
#existan instancias de empate que se decidan al azar, por lo que repetir muchas
#el cálculo de precisión por cada valor de K y promediarlos podría ser bueno.


# Comparacion con un arbol de clasificacion
library(rpart)
library(rpart.plot)

datos_tree <- cbind(
  datos_train,
  credit = label_train
)

# Se genera un árbol con las mismas variables exógenas que se usaron con KNN
modelo_tree <- rpart(
  credit ~ age + housing_free + housing_rent + job + foreign,
  data = datos_tree,
  method = "class",
  parms = list(split = "information")
)

tree_predict <- predict(
  modelo_tree,
  datos_test,
  type = "class"
)

sum(label_test == tree_predict)/length(label_test)
# En ocasiones quedarse sólo con la precisión como mejor parámetro puede no ser
#la mejor opción. En este ejemplo, a pesar de tener la mejor precisión el árbol
#simplemente no hace nada.

rpart.plot(modelo_tree)
table(label_test, tree_predict)
