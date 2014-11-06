
y <- c(0:100)
f <- dgamma(y, 1, 1)

plot(f, type="l")

dgamma(0, 1,1)
dgamma(1,1,1)

dgamma(c(0,1,5), 1, 1)


dgamma(c(0,1,5), 10, 1)

lst <- c(0,1,8,9)
vals <- dbinom(lst, 9, 0.5)
sum(vals)

lst2 <- c(0,1,2,7,8,9)
vals2 <- dbinom(lst2, 9, 0.5)
sum(vals2)




lst2 <- c(0,1,9,10)
vals2 <- dbinom(lst2, 9, 0.5)
sum(vals2)

