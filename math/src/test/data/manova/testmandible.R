require(MVN)

data <- read.table("mandible.dat", header=TRUE)

# write the data to csv for use by unit tests in scala.
write.csv(data, "test_mandible_data.csv", row.names=FALSE)

# test manova
attach(data)
Y <- cbind(X1,X2,X3,X4,X5,X6,X7,X8,X9)
data.manova <- manova(Y ~ as.factor(Group), data=data)

summary(data.manova, test="Wilks")
summary(data.manova)
summary(data.manova, test="Roy")
summary(data.manova, test="Hotelling-Lawley")


