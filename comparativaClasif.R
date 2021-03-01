#-----------------------------Comparativa de clasificadores 
#install.packages("ggvis")

#----Preprocesamiento
lentes<-read.csv("RFiles/ContactLens.csv",header = TRUE)
View(lentes)

set.seed(122) #Semilla
id <- sample(2, nrow(lentes), prob = c(0.6, 0.4), replace = T) #Todos las columnas a tratar deben ser factores
lenTrain <- lentes[id ==1,]
lenTest <- lentes[id ==2,]

View(lenTrain)


#----Arboles de desición  - Aprendizaje inductivo
library(rpart)
library(rattle)
library(rpart.plot)

#Modelado del arbol de decisión
arbolModeloLentes <- rpart(                     
  formula = Astigmatism~.,
  data = lenTrain,
  method = 'class',
  cp = -1
)

#Ploteamiento
arbolModeloLentes
rpart.plot(arbolModeloLentes,extra = 4)
fancyRpartPlot(arbolModeloLentes)

printcp(arbolModeloLentes)  #estadisticas de resultados
plotcp(arbolModeloLentes)   #evolución del error a medida que crece el arbol

#Predicción
pred_arbol_Len <- predict(arbolModeloLentes,lenTest,type = 'class')
pred_arbol_Len

#Precisión del modelo
table(pred_arbol_Len,lenTest$Astigmatism)
sum(pred_arbol_Len == lenTest$Astigmatism) / length(lenTest$Astigmatism)*100


#----Naive Bayes - Suposición ingenua
library(e1071)
naiveModeloLentes <- naiveBayes(Astigmatism~., data = lenTrain)
print(naiveModeloLentes)

#Predicción
pred_naive_len <- predict(naiveModeloLentes, newdata = lenTrain)
print(pred_naive_len)

library(caret)
#Precisión del modelo
confusionMatrix(pred_naive_len, lenTrain$Astigmatism)
#Si quiero la info de test debo crear predict y luego confusion con test


#----KNN - Aprendizaje perezoso
library(class)
library(ggvis)
library(FNN)
library(gmodels)

lentes.trainLabel <- lentes[ind==1,3]   #Astigmatism es la variable objetivo
lentes.testLabel <- lentes[ind==2,3]

#Predicción
pred_knn_lentes <-knn(train=lenTrain, test=lenTest, cl=lentes.trainLabel,k=3)
pred_knn_lentes

summary(pred_knn_lentes)  #Visualización del resultado

#Precisión del modelo
CrossTable(x=pred_knn_lentes,y=estudiantes.testLabel,prob.chisq=FALSE)
