setwd('/home/michal/R/Projects/isolationForest')
source('R/isolationForest.R')
library(rmatio)
library(DMwR)
library(pROC)
library("e1071")
library(caret)
library(class)

################################PRE-PROCESSING################################
data = read.mat('data/mammography.mat')
attributesOfData = data[["X"]]
classOfData = data.frame(data[["y"]])
data = cbind(attributesOfData, classOfData)
data[is.na(data)] = 0
ndata = data[ ,1:6]

# Normalizacja
ndata = (ndata - min(ndata))/(max(ndata) - min(ndata))

# Podzielenie danych
dataSize = nrow(ndata)
trainSetSize = floor(0.7 * dataSize)
testSetSize = dataSize - trainSetSize

set.seed(123)

trainIds = sample(seq_len(dataSize), size = trainSetSize)
trainData = ndata[trainIds, ]
trainClass = data[trainIds, 7]

## szum
contTrainData = trainData
contTrainData[ ,1] = (trainData[ ,1] + runif(nrow(trainData))/1e6)

testData= ndata[-trainIds, ]
testClass = data[-trainIds, 7]

## Ciekawostka
# Gdy dajemy do knn za duza populacje, on zwraca blad too many ties in knn, co znaczy, ze obiekty
# zachodza na siebie. Rozwiazanie: wprowadzenie szumu.
################################KNN################################
set.seed(123)
nn3 <- knn(contTrainData, testData, trainClass, k=1)
nn3 = as.numeric(nn3)
nn3[nn3 == 1] = 0
nn3[nn3 ==2 ] = 1

confMatrix = table(nn3, testClass)
confMatrix

t_obj = roc(testClass, nn3)
auc(t_obj)

n = sum(confMatrix) # number of instances
diag = diag(confMatrix) # number of correctly classified instances per class

confMatrix.accuracy = sum(diag) / n 
confMatrix.sensitivity = sensitivity(confMatrix)
confMatrix.specificity = specificity(confMatrix)
confMatrix.accuracy
confMatrix.sensitivity
confMatrix.specificity

################################SVM################################
set.seed(123)
svm.model = svm(trainData,y = trainClass,
                type='one-classification',
                nu= 0.2,
                gamma = 0.5,
                kernel="radial")
svm.predTest = as.integer(!predict(svm.model, testData))


# SVM miary jakości
confMatrix = table(svm.predTest, testClass)
t_obj = roc(testClass, svm.predTest)
auc(t_obj)

n = sum(confMatrix) # number of instances
diag = diag(confMatrix) # number of correctly classified instances per class

confMatrix.accuracy = sum(diag) / n 
confMatrix.sensitivity = sensitivity(confMatrix)
confMatrix.specificity = specificity(confMatrix)
confMatrix.accuracy
confMatrix.sensitivity
confMatrix.specificity

################################IF################################
forest = buildIsolationForest(trainData, 75, 190)
pred = predict(forest, testData, 190)

confMatrix = table(pred, testClass)
t_obj = roc(testClass, pred)
print(auc(t_obj))

n = sum(confMatrix) # number of instances
diag = diag(confMatrix) # number of correctly classified instances per class

confMatrix.accuracy = sum(diag) / n 
confMatrix.sensitivity = sensitivity(confMatrix)
confMatrix.specificity = specificity(confMatrix)
confMatrix.accuracy
confMatrix.sensitivity
confMatrix.specificity
  
