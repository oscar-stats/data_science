
load('datos.RData')
datos <- dengue_rio_clima
rm(dengue_rio_clima)
#======================================================================================================
# Exploratory data analysis
# Summary of data set
head(datos)
glimpse(datos)
datos$Serotipo <- as.factor(datos$Serotipo)
levels(datos$Serotipo)

# we remove unimportant and incomplete variables
datos$`Dengue_Grave_ (d)` <- NULL
datos$`DG/D_x100_(e)` <- NULL
datos$Confirmados_Laboratorio <- NULL
datos$Letalidad_f <- NULL
datos$Muertes <- NULL
datos$Poblacion_X1000 <-NULL
datos$RiopyPilar <- NULL
datos$RiopyBahia <- NULL
datos$Temp_Aire_max <- NULL
datos$Temp_Aire_min <- NULL
datos$`Tasa _Incidencia _(c)` <- NULL

#======================================================================================================

#create variable cases per week
datos$casos <- c(20, diff(datos$casos_n, lag = 1))
datos$casos_n <- NULL

#se detecta un valor atípico negativo
datos$casos[datos$casos == -287] <- 11
datos$casos[datos$casos ==-936] <- 896

#======================================================================================================
#Summary
summary(datos)
plot(datos$casos)
abline(h=60, col='red')
plot(quantile(datos$casos, probs = c(seq(0.1:1, by=0.01))))
abline(v=60, col='red')

#Create variable Infection level with high and low categories.
datos$infection <- as.factor(ifelse(datos$casos > 60, 'high', 'low'))

#Remove this variable from where the infection variable was created
datos$casos <- NULL

datos <- datos[, c(12, 1,3, 2, 4:11)]

# Number of observations and missing values
nrow(datos)

# Detection if there is an incomplete row
any(!complete.cases(datos))

# Number of missing data per variable
map_dbl(datos, .f = function(x){sum(is.na(x))})

# We proceed to identify which variables contain values "". This in case there is any empty category
datos %>% map_lgl(.f = function(x){any(!is.na(x) & x == "")})
#======================================================================================================
#Conclusion: There are no missing values, nor empty categories, all the variables already correspond
#a its type assigned a priori.
#======================================================================================================
# Distribution of response variables
#When creating a model, it is very important to study the distribution of the response variable, since
#after all, it is what we are interested in predicting.
ggplot(data = datos, aes(x = infection, y = ..count.., fill = infection)) +
  geom_bar() +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  labs(title = "Level of infection") +
  theme_bw() +
  theme(legend.position = "bottom")+
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

# Frequency table
table(datos$infection)
prop.table(table(datos$infection)) %>% round(digits = 2)

#======================================================================================================
#Here we observe the relationships of the variables to each other.
if(!require(car)) install.packages("car", repos = "http://cran.us.r-project.org")
library(car)
scatterplotMatrix(~RiopyAsu + Precipitacion  + Temp_Aire_med + Presion_Atm_med+
                    Insolacion+Evaporacion, 
                  data = datos,  reg.line=lm, smooth=FALSE,
                  spread=FALSE, span=0.5, diagonal = 'density')
#======================================================================================================
# Distribution of continuous variables
p1 <- ggplot(data = datos, aes(x = RiopyAsu, fill = infection)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  geom_rug(aes(color = infection), alpha = 0.5) +
  scale_color_manual(values = c("gray50", "orangered2")) +
  theme_bw()

# Distribution of continuous variables
p2 <- ggplot(data = datos, aes(x = Temp_Rocio_med, fill = infection)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  geom_rug(aes(color = infection), alpha = 0.5) +
  scale_color_manual(values = c("gray50", "orangered2")) +
  theme_bw()

# Distribution of continuous variables
p3 <- ggplot(data = datos, aes(x = Humedad_med, fill = infection)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  geom_rug(aes(color = infection), alpha = 0.5) +
  scale_color_manual(values = c("gray50", "orangered2")) +
  theme_bw()

# Distribution of continuous variables
p4 <- ggplot(data = datos, aes(x = Precipitacion, fill = infection)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  geom_rug(aes(color = infection), alpha = 0.5) +
  scale_color_manual(values = c("gray50", "orangered2")) +
  theme_bw()
if(!require(ggpubr)) install.packages("ggpubr", repos = "http://cran.us.r-project.org")
library(ggpubr)
final_plot <- ggarrange(p1, p2, p3, p4,  legend = "top")
final_plot
#======================================================================================================
# here we remove correlated variables or those that do not contribute anything to the model.
datos$Temp_Aire_med <- NULL
datos$Presion_Atm_med <- NULL
datos$Insolacion <-NULL
datos$Evaporacion <- NULL
datos$Anho <- NULL
datos$semana <- NULL
#======================================================================================================
# We divide our data into two parts of training and testing

if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
library(caret)
set.seed(2020)
# Indices of training observations are created
train <- createDataPartition(y = datos$infection, p = 0.7, list = FALSE, times = 1)
datos_train <- datos[train, ]
datos_test  <- datos[-train, ]

prop.table(table(datos_train$infection))
prop.table(table(datos_test$infection))
#======================================================================================================
#Variables with variance close to zero
datos %>% nearZeroVar(saveMetrics = TRUE)
#======================================================================================================

#Preprocessed data
if(!require(recipes)) install.packages("recipes", repos = "http://cran.us.r-project.org")

# A recipe () object is created with the response variable and the predictors.
objeto_recipe <- recipe(formula = infection ~ . ,  data =  datos_train)
objeto_recipe

#======================================================================================================
# Standardization and scaling
#For this analysis, all numerical variables are normalized.
objeto_recipe <- objeto_recipe %>% step_center(all_numeric())
objeto_recipe <- objeto_recipe %>% step_scale(all_numeric())
#======================================================================================================
# Binarization of qualitative variables
objeto_recipe <- objeto_recipe %>% step_dummy(all_nominal(), -all_outcomes())

#======================================================================================================
## The recipe object is trained
trained_recipe <- prep(objeto_recipe, training = datos_train)
trained_recipe
#======================================================================================================
# Transformations are applied to the training and test set
datos_train_prep <- bake(trained_recipe, new_data = datos_train)
class(datos_train_prep)
datos_test_prep  <- bake(trained_recipe, new_data = datos_test)
class(datos_test_prep)

#Remove objects
rm("datos_test",    "datos_train" ,
    "final_plot",   "n_observaciones",  "objeto_recipe",    "p1", "p2",              
 "p3",   "p4", "predicciones" ,    "train" , "trained_recipe")
#======================================================================================================
#Importance of variables
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")

datos_rf <- datos
datos_rf <- map_if(.x = datos_rf, .p = is.character, .f = as.factor) %>%
  as.data.frame()

modelo_randforest <- randomForest(formula = infection ~ . ,
                                  data = na.omit(datos_rf),
                                  mtry = 5,
                                  importance = TRUE, 
                                  ntree = 1000) 

importancia <- as.data.frame(modelo_randforest$importance)
importancia <- rownames_to_column(importancia,var = "variable")

p1 <- ggplot(data = importancia, aes(x = reorder(variable, MeanDecreaseAccuracy),
                                     y = MeanDecreaseAccuracy,
                                     fill = MeanDecreaseAccuracy)) +
  labs(x = "variable", title = "Reducción de Accuracy") +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom")

p2 <- ggplot(data = importancia, aes(x = reorder(variable, MeanDecreaseGini),
                                     y = MeanDecreaseGini,
                                     fill = MeanDecreaseGini)) +
  labs(x = "variable", title = "Reducción de pureza (Gini)") +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom")
ggarrange(p1, p2)

rm(datos_rf, modelo_randforest, importancia, p1, p2, datos)

#======================================================================================================
#Model training - K-Nearest Neighbor (kNN)
#======================================================================================================

#K-Nearest Neighbor (kNN)

# HYPERPARAMETERS, NUMBER OF REPETITIONS AND SEEDS FOR EACH REPETITION

particiones  <- 10
repeticiones <- 5

# Hiperparámetros
hiperparametros <- data.frame(k = c(1, 2, 5, 10, 15, 20, 30, 50))

set.seed(123)
seeds <- vector(mode = "list", length = (particiones * repeticiones) + 1)
for (i in 1:(particiones * repeticiones)) {
  seeds[[i]] <- sample.int(1000, nrow(hiperparametros)) 
}
seeds[[(particiones * repeticiones) + 1]] <- sample.int(1000, 1)

# DEFINITION OF TRAINING

control_train <- trainControl(method = "repeatedcv", number = particiones,
                              repeats = repeticiones, seeds = seeds,
                              returnResamp = "final", verboseIter = FALSE,
                              allowParallel = TRUE)

# MODEL ADJUSTMENT

set.seed(342)
modelo_knn <- train(infection ~ ., data = datos_train_prep,
                    method = "knn",
                    tuneGrid = hiperparametros,
                    metric = "Accuracy",
                    trControl = control_train)
modelo_knn

# GRAPHIC REPRESENTATION
ggplot(modelo_knn, highlight = TRUE) +
  scale_x_continuous(breaks = hiperparametros$k) +
  labs(title = "Evolution of the accuracy of the KNN model", x = "K") +
  theme_bw()

#======================================================================================================
#Model training - Logistic regression
#======================================================================================================
# HYPERPARAMETERS, NUMBER OF REPETITIONS AND SEEDS FOR EACH REPETITION
particiones  <- 10
repeticiones <- 5

# HYPERPARAMETERS
hiperparametros <- data.frame(parameter = "none")

set.seed(123)
seeds <- vector(mode = "list", length = (particiones * repeticiones) + 1)
for (i in 1:(particiones * repeticiones)) {
  seeds[[i]] <- sample.int(1000, nrow(hiperparametros))
}
seeds[[(particiones * repeticiones) + 1]] <- sample.int(1000, 1)

# DEFINITION OF TRAINING

control_train <- trainControl(method = "repeatedcv", number = particiones,
                              repeats = repeticiones, seeds = seeds,
                              returnResamp = "final", verboseIter = FALSE,
                              allowParallel = TRUE)

# MODEL ADJUSTMENT

set.seed(342)
modelo_logistic <- train(infection ~ ., data = datos_train_prep,
                         method = "glm",
                         tuneGrid = hiperparametros,
                         metric = "Accuracy",
                         trControl = control_train,
                         family = "binomial")
modelo_logistic

#======================================================================================================
#Model training - Simple classification tree
#======================================================================================================

particiones  <- 10
repeticiones <- 5

# HYPERPARAMETERS
hiperparametros <- data.frame(parameter = "none")

set.seed(123)
seeds <- vector(mode = "list", length = (particiones * repeticiones) + 1)
for (i in 1:(particiones * repeticiones)) {
  seeds[[i]] <- sample.int(1000, nrow(hiperparametros))
}
seeds[[(particiones * repeticiones) + 1]] <- sample.int(1000, 1)

# DEFINITION OF TRAINING
#===============================================================================
control_train <- trainControl(method = "repeatedcv", number = particiones,
                              repeats = repeticiones, seeds = seeds,
                              returnResamp = "final", verboseIter = FALSE,
                              allowParallel = TRUE)

# MODEL ADJUSTMENT
# ==============================================================================
set.seed(342)
modelo_C50Tree <- train(infection ~ ., data = datos_train_prep,
                        method = "C5.0Tree",
                        tuneGrid = hiperparametros,
                        metric = "Accuracy",
                        trControl = control_train)
modelo_C50Tree

#======================================================================================================
#Model training - Random Forest
#======================================================================================================
#RandomForest
particiones  <- 10
repeticiones <- 5

# HYPERPARAMETERS
hiperparametros <- expand.grid(mtry = c(3, 4, 5),
                               min.node.size = c(2, 3, 4, 5, 10, 15, 20, 30),
                               splitrule = "gini")

set.seed(123)
seeds <- vector(mode = "list", length = (particiones * repeticiones) + 1)
for (i in 1:(particiones * repeticiones)) {
  seeds[[i]] <- sample.int(1000, nrow(hiperparametros))
}
seeds[[(particiones * repeticiones) + 1]] <- sample.int(1000, 1)

# DEFINITION OF TRAINING
#===============================================================================
control_train <- trainControl(method = "repeatedcv", number = particiones,
                              repeats = repeticiones, seeds = seeds,
                              returnResamp = "final", verboseIter = FALSE,
                              allowParallel = TRUE)

# fit model
# ==============================================================================
set.seed(342)
modelo_rf <- train(infection ~ ., data = datos_train_prep,
                   method = "ranger",
                   tuneGrid = hiperparametros,
                   metric = "Accuracy",
                   trControl = control_train,
                   # Número de árboles ajustados
                   num.trees = 500)
modelo_rf

ggplot(modelo_rf, highlight = TRUE) +
  scale_x_continuous(breaks = 1:30) +
  labs(title = "Evolution of the accuracy of the Random Forest model") +
  guides(color = guide_legend(title = "mtry"),
         shape = guide_legend(title = "mtry")) +
  theme_bw()

#======================================================================================================
#Model training - SVM
#======================================================================================================
#SVM
particiones  <- 10
repeticiones <- 5

# HYPERPARAMETERS
hiperparametros <- expand.grid(sigma = c(0.001, 0.01, 0.1, 0.5, 1),
                               C = c(1 , 20, 50, 100, 200, 500, 700))

set.seed(123)
seeds <- vector(mode = "list", length = (particiones * repeticiones) + 1)
for (i in 1:(particiones * repeticiones)) {
  seeds[[i]] <- sample.int(1000, nrow(hiperparametros))
}
seeds[[(particiones * repeticiones) + 1]] <- sample.int(1000, 1)

# DEFINITION OF TRAINING
#===============================================================================
control_train <- trainControl(method = "repeatedcv", number = particiones,
                              repeats = repeticiones, seeds = seeds,
                              returnResamp = "final", verboseIter = FALSE,
                              allowParallel = TRUE)

# fit model
# ==============================================================================
set.seed(342)
modelo_svmrad <- train(infection ~ ., data = datos_train_prep,
                       method = "svmRadial",
                       tuneGrid = hiperparametros,
                       metric = "Accuracy",
                       trControl = control_train)
modelo_svmrad


ggplot(modelo_svmrad, highlight = TRUE) +
  labs(title = "Evolution of the accuracy of the  SVM Radial model") +
  theme_bw()

#======================================================================================================
#Model training - #Neural networks (NNET)
#======================================================================================================
#Neural networks (NNET)

particiones  <- 10
repeticiones <- 5

# Hyperparámeter
hiperparametros <- expand.grid(size = c(10, 20, 50, 80, 100, 120),
                               decay = c(0.0001, 0.1, 0.5))

set.seed(123)
seeds <- vector(mode = "list", length = (particiones * repeticiones) + 1)
for (i in 1:(particiones * repeticiones)) {
  seeds[[i]] <- sample.int(1000, nrow(hiperparametros))
}
seeds[[(particiones * repeticiones) + 1]] <- sample.int(1000, 1)

# DEFINITION OF TRAINING
#===============================================================================
control_train <- trainControl(method = "repeatedcv", number = particiones,
                              repeats = repeticiones, seeds = seeds,
                              returnResamp = "final", verboseIter = FALSE,
                              allowParallel = TRUE)

# fit model
# ==============================================================================
set.seed(342)
modelo_nnet <- train(infection ~ ., data = datos_train_prep,
                     method = "nnet",
                     tuneGrid = hiperparametros,
                     metric = "Accuracy",
                     trControl = control_train,
                     # Rango de inicialización de los pesos
                     rang = c(-0.7, 0.7),
                     # Número máximo de pesos
                     # se aumenta para poder incluir más meuronas
                     MaxNWts = 2000,
                     # Para que no se muestre cada iteración por pantalla
                     trace = FALSE)
modelo_nnet

ggplot(modelo_nnet, highlight = TRUE) +
  labs(title = "Evolution of the accuracy of the NNET model") +
  theme_bw()

#======================================================================================================
#List of trained models
#======================================================================================================

modelos <- list(KNN = modelo_knn, logistic = modelo_logistic,
                arbol = modelo_C50Tree, rf = modelo_rf,
                SVMradial = modelo_svmrad, 
                NNET = modelo_nnet)
#======================================================================================================
#Results of trained models
resultados_resamples <- resamples(modelos)
resultados_resamples$values %>% head(10)
#======================================================================================================

# The data frame returned by resamples () is transformed to separate the name of the
# model and metrics in different columns.
metricas_resamples <- resultados_resamples$values %>%
  gather(key = "modelo", value = "valor", -Resample) %>%
  separate(col = "modelo", into = c("modelo", "metrica"),
           sep = "~", remove = TRUE)
metricas_resamples %>% head()
#======================================================================================================

#Accuracy and Kappa average of each model

metricas_resamples %>% 
  group_by(modelo, metrica) %>% 
  summarise(media = mean(valor)) %>%
  spread(key = metrica, value = media) %>%
  arrange(desc(Accuracy))
#======================================================================================================
# Model comparison chart
metricas_resamples %>%
  filter(metrica == "Accuracy") %>%
  group_by(modelo) %>% 
  summarise(media = mean(valor)) %>%
  ggplot(aes(x = reorder(modelo, media), y = media, label = round(media, 2))) +
  geom_segment(aes(x = reorder(modelo, media), y = 0,
                   xend = modelo, yend = media),
               color = "grey50") +
  geom_point(size = 7, color = "firebrick") +
  geom_text(color = "white", size = 2.5) +
  scale_y_continuous(limits = c(0, 1)) +
  # Accuracy basal
  geom_hline(yintercept = 0.43, linetype = "dashed") +
  annotate(geom = "text", y = 0.48, x =6.5, label = "") +
  labs(title = "Validación: Accuracy medio repeated-CV",
       subtitle = "Modelos ordenados por media",
       x = "modelo") +
  coord_flip() +
  theme_bw()
#======================================================================================================
#Test error
#======================================================================================================
#predictions
predicciones <- extractPrediction(
  models = modelos,
  testX = datos_test_prep[, -5],
  testY = datos_test_prep$infection
)
predicciones %>% head()
#======================================================================================================
#compare the prediction results between models and the differences between training set and test.

metricas_predicciones <- predicciones %>%
  mutate(acierto = ifelse(obs == pred, TRUE, FALSE)) %>%
  group_by(object, dataType) %>%
  summarise(accuracy = mean(acierto))

metricas_predicciones %>%
  spread(key = dataType, value = accuracy) %>%
  arrange(desc(Test))

#======================================================================================================
# Graphically we have.
ggplot(data = metricas_predicciones,
       aes(x = reorder(object, accuracy), y = accuracy,
           color = dataType, label = round(accuracy, 2))) +
  geom_point(size = 8) +
  scale_color_manual(values = c("orangered2", "gray50")) +
  geom_text(color = "white", size = 3) +
  scale_y_continuous(limits = c(0, 1)) +
  # Accuracy basal
  geom_hline(yintercept = 0.43, linetype = "dashed") +
  annotate(geom = "text", y = 0.48, x = 6.5, label = "Accuracy basal") +
  coord_flip() +
  labs(title = "Accuracy de entrenamiento y test", 
       x = "modelo") +
  theme_bw() + 
  theme(legend.position = "bottom")
#======================================================================================================
#The Random Forest model is the one that obtains the best results taking into account the accuracy 
#metric both in the test set and in the validation (repeated CV). The remaining models achieve very similar 
#test values.
#======================================================================================================
