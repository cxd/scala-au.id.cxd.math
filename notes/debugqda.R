
require(MASS)

data <- read.csv("data/test_mandible_train.csv", header=TRUE)
data2 <- read.csv("data/test_mandible_test.csv", header=TRUE)

data3 <-  read.csv("data/test_mandible_data.csv", header=TRUE)

attach(data)
model <- lda(factor(Group) ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9)

p <- predict(model, data2)

actual <- data2$Group
predicted <- p$class

table(actual,predicted)
detach(data)


attach(data3)

head(data3[,3:11])



model2 <- qda(factor(Group) ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9, data3)

p2 <- predict(model2, data2)

actual <- data2$Group
predicted <- p2$class

table(actual,predicted)


