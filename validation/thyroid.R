setwd('validation/');

library(rmatio)
library(DMwR)
library(e1071)
library(pROC)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

data = read.mat('../data/thyroid.mat')

attributesOfData = data[["X"]]
classOfData = data.frame(data[["y"]])
data = cbind(attributesOfData, classOfData)

data_noClass_normalize = as.data.frame(lapply(data[1 : 6], normalize))
# data_noClass_normalize = data_noClass_normalize[1:10000, ]
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

library(class)
best_k = 0
best_k_auc = 0
for(i in 1:20){
set.seed(1)
nn3 <- knn(trainData, testData, trainClass, k=i)
nn3 = as.numeric(nn3)
nn3[nn3 == 1] = 0
nn3[nn3 ==2 ] = 1
table(nn3, testClass)

t_obj = roc(testClass, nn3)
plot(t_obj)
auc(t_obj)
if(auc(t_obj) > best_k_auc){
  best_k_auc = auc(t_obj)
  best_k = i
}
}

set.seed(1)
nn3 <- knn(trainData, testData, trainClass, k=1)
nn3 = as.numeric(nn3)
nn3[nn3 == 1] = 0
nn3[nn3 ==2 ] = 1

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

set.seed(123)
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


