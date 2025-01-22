library(plotly)
library(dplyr)
library(visdat)
library(pROC)
library(caret)
data<- read.csv("/Users/Alessiospl/Il mio Drive/Università/Statistical Learning/Diabetes.csv",stringsAsFactors=TRUE)
View(data)
#attach(data)
head(data)



#Visualizzo eventuali dati mancanti
vis_miss(data) 
data <-na.omit(data)
View(data)
head(data,width=400)

#ANALISI ESPLORATIVA 
#matrice di correlazione
library(corrplot)
matrice<-cor(data)
print(matrice, width = 300)#allargando la console ho la matrice tutta sulla riga
corrplot(matrice, type = "lower",tl.col = "black",method = "color")


#Grafico a torta
conteggi <- table(data$Outcome)

# Crea il grafico a torta
grafico <- plot_ly(labels = c("Non Diabetico", "Diabetico"), values = conteggi, type = "pie",
                   hole = 0.33, opacity = 0.85, textposition = "outside",
                   textinfo = "percent+label",
                   marker = list(colors = c("orange", "gray")))

# Aggiungo il titolo al grafico
grafico <-grafico %>% layout(title = "Diabete")

# Visualizza il grafico
grafico


#Faccio i boxplot
boxplot(data$Pregnancies, main = "Esplorativa Dataset", xlab="Pregnancies")
summary(data$Pregnancies)

boxplot(data$Glucose, main = "Esplorativa Dataset", xlab="Glucose")
summary(data$Glucose)

boxplot(data$BloodPressure, main = "Esplorativa Dataset", xlab="BloodPressure")
summary(data$BloodPressure)

boxplot(data$SkinThickness, main = "Esplorativa Dataset", xlab="SkinThickness")
summary(data$SkinThickness)

boxplot(data$Insulin, main = "Esplorativa Dataset", xlab="Insulin")
summary(data$Insulin)

boxplot(data$BMI, main = "Esplorativa Dataset", xlab="BMI")
summary(data$BMI)

boxplot(data$DiabetesPedigreeFunction, main = "Esplorativa Dataset", xlab="DiabetesPedigreeFunction")
summary(data$DiabetesPedigreeFunction)

boxplot(data$Age, main = "Esplorativa Dataset", xlab="Age")
summary(data$Age)


# Conversione della variabile "Diabete" 
data$Outcome <-as.factor(data$Outcome)
set.seed(321)
#Divido il mio dataset in training e test set
training.samples = createDataPartition(data$Outcome, p = .75, list = FALSE)
train <- data[training.samples, ]
test <- data[-training.samples, ]

#Metodo di cross validation
control <- trainControl(method="repeatedcv", number=10, repeats=3)

# ADDESTRO I DIVERSI MODELLI 
# Regressione logistica
fit_logistica <- train(Outcome ~ ., data = train, method = "glm", 
                       family = "binomial", trControl = control)

# LDA
fit_lda <- train(Outcome ~ ., data = train, method = "lda", trControl = control)

# SVM Lineare
fit_svm_linear <- train(Outcome ~ ., data = train, method = "svmLinear", 
                        trControl = control,prob.model=TRUE)

# SVM Radiale
fit_svm_radial <- train(Outcome ~ ., data = train, method = "svmRadial", 
                        trControl = control,prob.model = TRUE)

# KNN
fit_knn <- train(Outcome ~ ., data = train, method = "knn", 
                 trControl = control)


# Previsioni sul set di training
pred_logistica_train <- predict(fit_logistica, train)
pred_lda_train <- predict(fit_lda, train)
pred_svm_radial_train <- predict(fit_svm_radial, train)
pred_svm_linear_train <- predict(fit_svm_linear, train)
pred_knn_train <- predict(fit_knn, train)

# Ottengo la matrice di confusione e calcolo le metriche per ciascun modello sul training set
cm_logistica_train <- confusionMatrix(pred_logistica_train, train$Outcome)
cm_lda_train <- confusionMatrix(pred_lda_train, train$Outcome)
cm_svm_radial_train <- confusionMatrix(pred_svm_radial_train, train$Outcome)
cm_svm_linear_train <- confusionMatrix(pred_svm_linear_train, train$Outcome)
cm_knn_train <- confusionMatrix(pred_knn_train, train$Outcome)

# Estraggo l'accuratezza e la sensibilità per il training set
results_train <- data.frame(
  Model = c("Logistica", "LDA", "SVM Radial", "SVM Lineare", "KNN"),
  Accuracy = c(cm_logistica_train$overall["Accuracy"],
               cm_lda_train$overall["Accuracy"],
               cm_svm_radial_train$overall["Accuracy"],
               cm_svm_linear_train$overall["Accuracy"],
               cm_knn_train$overall["Accuracy"])
)
print(results_train)



#Dopo aver addestrato i modelli procedo con la previsione sul test set
pred_logistica <- predict(fit_logistica, test)

#lda
pred_lda <- predict(fit_lda,test)

#svm
pred_svm_radial <- predict(fit_svm_radial,test)
pred_svm_linear <- predict(fit_svm_linear,test)

#KNN
pred_knn <- predict(fit_knn, test)


#ora valuto le previsioni del mio modello con la matrice di confusione
cm_logistica <- confusionMatrix(pred_logistica, test$Outcome)
cm_lda <- confusionMatrix(pred_lda, test$Outcome)
cm_svm_radial <- confusionMatrix(pred_svm_radial, test$Outcome)
cm_svm_linear <- confusionMatrix(pred_svm_linear, test$Outcome)
cm_knn <- confusionMatrix(pred_knn, test$Outcome)

# Estraggo l'accuratezza e la sensibilità da ciascuna matrice di confusione
results_test <- data.frame(
  Model = c("Logistica", "LDA", "SVM Radial", "SVM Lineare", "KNN"),
  Accuracy = c(cm_logistica$overall["Accuracy"],
               cm_lda$overall["Accuracy"],
               cm_svm_radial$overall["Accuracy"],
               cm_svm_linear$overall["Accuracy"],
               cm_knn$overall["Accuracy"]),
  Sensitivity = c(cm_logistica$byClass["Sensitivity"],
                  cm_lda$byClass["Sensitivity"],
                  cm_svm_radial$byClass["Sensitivity"],
                  cm_svm_linear$byClass["Sensitivity"],
                  cm_knn$byClass["Sensitivity"])
)
print(results_test)

#Vado Infine a rappresentare le curve di ROC

# Per prima cosa dobbiamo calcolare le probabilità predette per ogni modello,
#l'estrazione della seconda colonna serve per ottenere le probabilità per la
#classe positiva (diabetico)
prob_logistica <- predict(fit_logistica, test, type = "prob")[,2]
prob_lda <- predict(fit_lda, test, type = "prob")[,2]
prob_svm_radial <- predict(fit_svm_radial, test, type = "prob")[,2]
prob_svm_linear <- predict(fit_svm_linear, test, type = "prob")[,2]
prob_knn <- predict(fit_knn, test, type = "prob")[,2]

# Calcolo le curve ROC 
roc_logistica <- roc(test$Outcome, prob_logistica)
roc_lda <- roc(test$Outcome, prob_lda)
roc_svm_radial <- roc(test$Outcome, prob_svm_radial)
roc_svm_linear <- roc(test$Outcome, prob_svm_linear)
roc_knn <- roc(test$Outcome, prob_knn)

# Calcolo l'AUC per ogni curva ROC
auc_logistica <- auc(roc_logistica)
auc_lda <- auc(roc_lda)
auc_svm_radial <- auc(roc_svm_radial)
auc_svm_linear <- auc(roc_svm_linear)
auc_knn <- auc(roc_knn)

# Legenda per i valori di AUC
legend_logistica <- paste("Logistica (AUC = ", round(auc_logistica, 2), ")", sep = "")
legend_lda <- paste("LDA (AUC = ", round(auc_lda, 2), ")", sep = "")
legend_svm_radial <- paste("SVM Radiale (AUC = ", round(auc_svm_radial, 2), ")", sep = "")
legend_svm_linear <- paste("SVM Lineare (AUC = ", round(auc_svm_linear, 2), ")", sep = "")
legend_knn <- paste("KNN (AUC = ", round(auc_knn, 2), ")", sep = "")

#plot curva ROC per la Regressione Logistica
plot(roc_logistica, col = "blue", main = "ROC - Logistica")
legend("bottomright", legend = legend_logistica, col = "blue", lwd = 2)

# plot curva ROC per LDA
plot(roc_lda, col = "red", main = "ROC - LDA")
legend("bottomright", legend = legend_lda, col = "red", lwd = 2)

# plot curva ROC per SVM Radiale
plot(roc_svm_radial, col = "green", main = "ROC - SVM Radiale")
legend("bottomright", legend = legend_svm_radial, col = "green", lwd = 2)

# plot curva ROC per SVM Lineare
plot(roc_svm_linear, col = "purple", main = "ROC - SVM Lineare")
legend("bottomright", legend = legend_svm_linear, col = "purple", lwd = 2)

# plot curva ROC per KNN
plot(roc_knn, col = "orange", main = "ROC - KNN")
legend("bottomright", legend = legend_knn, col = "orange", lwd = 2)

