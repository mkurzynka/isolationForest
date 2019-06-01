setwd('/home/michal/R/Projects/isolationForest');

library(rmatio)
library(DMwR)

data = read.mat('mammography.mat')
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
trainData[ ,1] = (trainData[ ,1] + runif(nrow(trainData))/1e12)
testData= ndata[-trainIds, ]
testClass = data[-trainIds, 7]

## Ciekawostka
# Gdy dajemy do knn za duza populacje, on zwraca blad too many ties in knn, co znaczy, ze obiekty
# zachodza na siebie. Rozwiazanie: wprowadzenie szumu.

library(class)
nn3 <- knn(trainData[, 1:6], testData[, 1:6], trainClass, k=2)
nn3 = as.numeric(nn3)
table(nn3, testClass)

library("ROCR")
pred = prediction(nn3, testClass)
perf = performance(pred, "tpr", "fpr")
plot(perf, col="blue")
abline(0, 1)


library("e1071")
svm = e1071::svm(trainData[, 1:6], nu=0.09, type="one-classification", kernel="polynomial")
out_svm = as.integer(predict(svm))
