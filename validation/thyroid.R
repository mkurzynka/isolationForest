setwd('/home/michal/R/Projects/isolationForest')
source('R/isolationForest.R')
library(rmatio)
library(DMwR)
library(pROC)
library("e1071")
library(caret)
library(class)

################################PRE-PROCESSING################################
data = read.mat('data/thyroid.mat')

attributesOfData = data[["X"]]
classOfData = data.frame(data[["y"]])
data = cbind(attributesOfData, classOfData)

data_noClass_normalize = as.data.frame(lapply(data[1 : 6], normalize))
data_onlyClass = data[, 7]

dataSize = nrow(data_noClass_normalize)
trainSetSize = floor(0.7 * dataSize)
testSetSize = dataSize - trainSetSize

set.seed(123)
trainIds = sample(seq_len(dataSize), size = trainSetSize)

trainData = data_noClass_normalize[trainIds, ]
trainClass = data_onlyClass[trainIds]

testData = data_noClass_normalize[-trainIds, ]
testClass = data_onlyClass[-trainIds]

################################KNN################################
set.seed(123)
nn3 <- knn(trainData, testData, trainClass, k=1)
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
                nu= 0.15,
                gamma = 0.3535534,
                kernel="radial")
svm.predTest = as.integer(!predict(svm.model, testData))

# SVM quality
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
forest = buildIsolationForest(trainData, 125, 512)
prediction = predict(forest, testData, 512)

table(prediction, testClass)

confMatrix = table(prediction, testClass)
t_obj = roc(testClass, prediction)
auc(t_obj)
n = sum(confMatrix) # number of instances
diag = diag(confMatrix) # number of correctly classified instances per class

confMatrix.accuracy = sum(diag) / n 
confMatrix.sensitivity = sensitivity(confMatrix)
confMatrix.specificity = specificity(confMatrix)
confMatrix.accuracy
confMatrix.sensitivity
confMatrix.specificity

