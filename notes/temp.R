
nodes <- c("A","B","C","D","E","F","G","H")

adj <- t(as.matrix(data.frame(
  A=c(0, 1, 1, 0, 0, 0, 0, 0),
  B=c(0, 0, 0, 0, 0, 1, 0, 0),
  C=c(0, 0, 0, 1, 0, 0, 0, 0),
  D=c(0, 0, 0, 0, 1, 0, 0, 1),
  E=c(0, 0, 0, 0, 0, 1, 1, 1),
  F=c(0, 0, 0, 0, 0, 0, 1, 0),
  G=c(0, 0, 0, 0, 0, 0, 0, 1),
  H=c(0, 0, 0, 0, 0, 0, 0, 0)
)))

colnames(adj) <- nodes

weights <- t(as.matrix(data.frame(
  A=c(0, 100, 30, 0, 0, 0, 0, 0),
  B=c(0, 0, 0, 0, 0, 300, 0, 0),
  C=c(0, 0, 0, 200, 0, 0, 0, 0),
  D=c(0, 0, 0, 0, 80, 0, 0, 90),
  E=c(0, 0, 0, 0, 0, 50, 150, 30),
  F=c(0, 0, 0, 0, 0, 0, 70, 0),
  G=c(0, 0, 0, 0, 0, 0, 0, 70),
  H=c(0, 0, 0, 0, 0, 0, 0, 0)
)))

colnames(weights) <- nodes

adj

(out1 <- t(adj)%*%adj)

(out2 <- t(out1)%*%(adj))

(out3 <- t(out2)%*%(adj))

(out4 <- t(out3)%*%(adj))

(out5 <- t(out4)%*%(adj))

(out6 <- t(out5)%*%(adj))


(out7 <- t(out6)%*%(adj))


(out8 <- t(out7)%*%(adj))


(out9 <- t(out8)%*%(adj))


(out10 <- t(out9)%*%(adj))


(out11 <- t(out10)%*%(adj))
