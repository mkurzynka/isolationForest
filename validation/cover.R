setwd('validation/');

library(rmatio)
library(DMwR)
library(e1071)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

data = read.mat('../data/cover.mat')

attributesOfData = data[["X"]]
classOfData = data.frame(data[["y"]])
data = cbind(attributesOfData, classOfData)
    
data_noClass_normalize = as.data.frame(lapply(data[1 : 10], normalize))
data_noClass_normalize = data_noClass_normalize[1:10000, ]
data_onlyClass = data[1:10000, 11]

dataSize = nrow(data_noClass_normalize)
trainSetSize = floor(0.7 * dataSize)
testSetSize = dataSize - trainSetSize

set.seed(123)

trainIds = sample(seq_len(dataSize), size = trainSetSize)

trainData = data_noClass_normalize[trainIds, ]
trainClass = data_onlyClass[trainIds]

testData = data_noClass_normalize[-trainIds, ]
testClass = data_onlyClass[-trainIds]

library(class)
nn3 <- knn(trainData, testData, trainClass, k=2)
nn3 = as.numeric(nn3)
nn3[nn3 == 1] = 0
nn3[nn3 ==2 ] = 1
table(nn3, testClass)

library("ROCR")
pred = prediction(nn3, testClass)
perf = performance(pred, "tpr", "fpr")
plot(perf, col="blue")
abline(0, 1)

svm.model = svm(trainData,y = trainClass,
                       type='one-classification',
                       nu=0.5,
                       cost=1e5,
                       scale=TRUE,
                       kernel="radial")

svm.predTrain = as.integer(!predict(svm.model))

svm.predTest = as.integer(!predict(svm.model, testData))

table(Predicted = svm.predTrain, Reference = trainClass)
table(Predicted = svm.predTest, Reference = testClass)

confTrain = table(Predicted = svm.predTrain, Reference = trainClass)
confTest = table(Predicted = svm.predTest, Reference = testClass)
