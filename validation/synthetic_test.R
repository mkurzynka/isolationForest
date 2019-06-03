source('R/isolationForest.R')
# Normal data generation
a = runif(400, 0, 0.3)
dim(a) = c(200, 2)
a = cbind(a, replicate(200, 0))

# Anomaly generation
contamination = runif(60, -1, 1)
dim(contamination) = c(30, 2)
contamination = cbind(contamination, replicate(30, 1))

data = rbind(a, contamination)
data = as.data.frame(data)

# Create training and testing set
trainIds = sample(seq_len(nrow(data)), size = 170)
trainData = data[trainIds, ]
testData= data[-trainIds, ]

# Build isolation forest
forest = buildIsolationForest(as.data.frame(trainData), 100, 50)

# Predict
pred <- predict(forest, as.data.frame(testData), 50)

table(pred, testData[,3])


