
require(MASS)

data <- read.csv("data/test_mandible_data.csv", header=TRUE)

attach(data)
model <- lda(factor(Group) ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9)

model

X <- data[,3:11]

projection <- as.matrix(X)%*%model$scaling

data2 <- as.data.frame(projection)
data2$Group <- data$Group

plot(data2[,1], data2[,2], type="n")
text(data2[,1], data2[,2], labels=data2$Group, col=data2$Group)