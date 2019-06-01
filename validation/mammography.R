setwd('validation/');

library(rmatio)
library(DMwR)
library(pROC)

data = read.mat('../data/mammography.mat')
attributesOfData = data[["X"]]
classOfData = data.frame(data[["y"]])
data = cbind(attributesOfData, classOfData)
data[is.na(data)] = 0
ndata = data[ ,1:6]

# Normalizacja
ndata = (ndata - min(ndata))/(max(ndata) - min(ndata))

dataSize = nrow(ndata)
trainSetSize = floor(0.7 * dataSize)
testSetSize = dataSize - trainSetSize

set.seed(123)

trainIds = sample(seq_len(dataSize), size = trainSetSize)

trainData = ndata[trainIds, ]
trainClass = data[trainIds, 7]

## szum
trainData[ ,1] = (trainData[ ,1] + runif(nrow(trainData))/1e6)
testData= ndata[-trainIds, ]
testClass = data[-trainIds, 7]

## Ciekawostka
# Gdy dajemy do knn za duza populacje, on zwraca blad too many ties in knn, co znaczy, ze obiekty
# zachodza na siebie. Rozwiazanie: wprowadzenie szumu.

library(class)
nn3 <- knn(trainData[, 1:6], testData[, 1:6], trainClass, k=2)
nn3 = as.numeric(nn3)
nn3[nn3 == 1] = 0
nn3[nn3 == 2] = 1
# KNN miary jakości
t_obj = roc(testClass, nn3)
plot(t_obj)
auc(t_obj)

confMatrix = table(nn3, testClass)
confMatrix

n = sum(confMatrix) # number of instances
diag = diag(confMatrix) # number of correctly classified instances per class

confMatrix.accuracy = sum(diag) / n 
confMatrix.sensitivity = sensitivity(confMatrix)
confMatrix.specificity = specificity(confMatrix)
confMatrix.accuracy
confMatrix.sensitivity
confMatrix.specificity


library("e1071")
library(caret)
# svm = e1071::svm(trainData[, 1:6], nu=0.09, type="one-classification", kernel="polynomial")
# out_svm = as.integer(predict(svm))

best_model = 0
for(i in seq(0.01, 0.2, 0.01)){
  for(j in seq(-2, 2, 0.5)){
    svm.model = svm(trainData,y = trainClass,
                    type='one-classification',
                    nu= i,
                    gamma = 2^j,
                    kernel="radial")
    svm.predTest = as.integer(!predict(svm.model, testData))
    
    # table(Predicted = svm.predTrain, Reference = trainClass)
    # print(table(Predicted = svm.predTest, Reference = testClass))
    t_obj = roc(testClass, svm.predTest)
    # print(auc(t_obj))
    if(auc(t_obj) > best_model) {
      best_roc = t_obj
      best_model = auc(t_obj)
      best_gamma_model = 2^j
      best_nu_model = i
    }
    # svm.predTrain = as.integer(!predict(svm.model))
  }
}

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