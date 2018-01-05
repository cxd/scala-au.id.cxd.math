
## debugging henze zirkler method


data1 <- read.csv("data/iris_virginica.csv", header=TRUE)
data <- scale(data1[,1:4])

data4 <- iris
data4 <- scale(data4[,1:4])

data2 <- read.csv("data/test_mandible_data.csv", header=TRUE)
data2 <- scale(data2[,2:10])

data3 <- read.csv("data/test_sparrows.csv", header=TRUE)
data3 <- scale(data3[,2:6])

hzTest(data)

hzTest(data2)

hzTest(data3, qqplot=TRUE)

hzTest(data4)

mardiaTest(data2)

n <- nrow(data)
p <- ncol(data)
S <- ((n-1)/n)*cov(data)
d <- as.matrix(data)
Y <- d%*%solve(S)%*%t(d)

I <- matrix(c(rep(1,n)),1,n)
A <- matrix(diag(t(Y)))%*%I
B <-  matrix(c(rep(1,n)),n,1)%*%diag(t(Y))

dif <- scale(data, scale = FALSE)


Dj <- diag(dif%*%solve(S)%*%t(dif))

Djk <- - 2*t(Y) + A + B

b <- 1/(sqrt(2))*((2*p + 1)/4)^(1/(p + 4))*(n^(1/(p + 4)))

a1 <- (sum(exp( - (b^2)/2 * Djk)))
b1 <- sum(exp( - ((b^2)/(2 * (1 + (b^2)))) * Dj)) 

#a1 <- 122.579
#b1 <- 19.052

hz <- n * (1/(n*n) * a1 - 2*((1+(b^2))^(-p/2)) * (1/n)* b1 + ((1+(2*b^2))^(-p/2)))

logHz <- log(hz)

wb <- (1 + b^2)*(1 + 3*b^2)

a <- 1 + 2*b^2

mu <- 1 - a^(- p/2)*(1 + p*b^2/a + (p*(p + 2)*(b^4))/(2*a^2)) #HZ mean

si2 <- 2*(1 + 4*b^2)^(- p/2) +
  2*a^( - p)*(1 + (2*p*b^4)/a^2 + 
                (3*p*(p + 2)*b^8)/(4*a^4)) - 
  4*wb^( - p/2)*(1 + (3*p*b^4)/(2*wb) + (p*(p + 2)*b^8)/(2*wb^2)) #HZ variance

pmu <- log(sqrt(mu^4/(si2 + mu^2))) #lognormal HZ mean
psi <- sqrt(log((si2 + mu^2)/mu^2)) #lognormal HZ variance

P <- 1 - plnorm(hz,pmu,psi) #P-value associated to the HZ statistic

data2 <- iris
data2 <- data2[,1:4]
