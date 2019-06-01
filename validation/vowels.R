setwd('validation/');

library(rmatio)
library(DMwR)
library(e1071)
library(pROC)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

data = read.mat('../data/vowels.mat')

attributesOfData = data[["X"]]
classOfData = data.frame(data[["y"]])
data = cbind(attributesOfData, classOfData)

data_noClass_normalize = as.data.frame(lapply(data[1 : 12], normalize))
# data_noClass_normalize = data_noClass_normalize[1:10000, ]
data_onlyClass = data[, 13]

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
set.seed(1)
nn3 <- knn(trainData, testData, trainClass, k=2)
nn3 = as.numeric(nn3)
# nn3[nn3 == 1] = 0
# nn3[nn3 ==2 ] = 1
table(nn3, testClass)

set.seed(123)


svm.model = svm(trainData,y = trainClass,
                type='one-classification',
                nu=0.4,
                #gamma = 0.5,
                scale=TRUE,
                kernel="radial")

best_model = 0
best_gamma_model = 0
best_nu_model = 0

for(i in seq(0.01, 0.2, 0.01)){
  for(j in seq(-6, -1, 0.5)){
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
    best_nu_model = i/100
  }
# svm.predTrain = as.integer(!predict(svm.model))
  }
}
svm.predTest = as.integer(!predict(svm.model, testData))

table(Predicted = svm.predTrain, Reference = trainClass)
table(Predicted = svm.predTest, Reference = testClass)

confTrain = table(Predicted = svm.predTrain, Reference = trainClass)
confTest = table(Predicted = svm.predTest, Reference = testClass)

