require(caret)

data <- read.csv("data/egyption_skulls.csv", header=TRUE)


trainIdx <- createDataPartition(y = as.factor(data$PeriodNum), p=0.75, list=FALSE)

train <- data[trainIdx,]
test <- data[-trainIdx,]
write.csv(train, "data/egyption_skulls_train.csv", row.names=FALSE)
write.csv(test, "data/egyption_skulls_test.csv", row.names=FALSE)


data2 <- read.csv("data/test_mandible_data.csv", header=TRUE)

trainIdx <- createDataPartition(y=as.factor(data2$Group), p=0.75, list=FALSE)
train2 <- data2[trainIdx,]
test2 <- data2[-trainIdx,]
write.csv(train2, "data/test_mandible_train.csv", row.names=FALSE)
write.csv(test2, "data/test_mandible_test.csv", row.names=FALSE)




