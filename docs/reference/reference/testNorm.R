x <- seq(from = -4, to = 4, by = 0.1)
y <- dnorm(x, mean = 0, sd = 1)
plot(y~x, type="l")