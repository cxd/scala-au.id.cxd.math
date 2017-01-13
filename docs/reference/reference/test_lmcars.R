data <- mtcars 
test1 <- lm(mpg ~ cyl + disp + hp + drat + wt + qsec + vs + am + gear + carb, data)
summary(test1)

M <- as.matrix(data[,2:ncol(data)])

t1 <- t(M)%*%M
e <- eigen(t1)
plot(e$values)