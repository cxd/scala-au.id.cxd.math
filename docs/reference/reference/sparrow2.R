sp <-read.table("../../../data/sparrow2.txt", header=TRUE)
# writing as csv
write.csv(sp, "../../../data/test_sparrows.csv", row.names=FALSE)

sp
attach(sp)
myvars <- c("Bird", "Length", "Survival")
spsubset<-sp[myvars]
spsubset
str(sp)
boxplot(Length~Survival, xlab="Survival", ylab="Length (mm)") 
tapply(sp$Length, sp$Survival, summary)
tapply(sp$Length, sp$Survival, mean)
tapply(sp$Length, sp$Survival, var)

#subset groupa
survival1<-subset(sp, Survival<=1, select=Length:Sternum )
survival2<-subset(sp, Survival>=2, select=Length:Sternum )
#calculate t-test from formula for length by survival
(mean1<- mean(survival1$Length))
(n1<-nrow(survival1))
(var1<-var(survival1$Length))
(mean2<- mean(survival2$Length))
(n2<-nrow(survival2))
(var2<-var(survival2$Length))
#pooled variance
(var={(n1-1)*var1 + (n2-1)*var2}/(n1+n2-2))
#t-test
(t=(mean1-mean2)/{sqrt(var*(1/n1+1/n2))})

#in R t values are one tailed so use 0.025
(tcrit=qt(0.025,47, lower.tail=FALSE) )
#in R t probabilities are one tailed so *2
(tprob=pt(-0.993,47, lower.tail=TRUE) *2)

#or the easy way
t.test(Length~Survival, var.equal = TRUE)

#calculate Hotellings'T^2 multivariate statistic 
#create vectors of means and covariance matrices for survivor (1) and non-survivor (2) groups
(xbar1<- colMeans(survival1))
(C1<-cov(survival1))
(n1<-nrow(survival1))
(xbar2<- colMeans(survival2))  
(C2<-cov(survival2))
(n2<-nrow(survival2))
#pooled covariance matrix 
(C={(n1-1)*C1 + (n2-1)*C2}/(n1+n2-2))
(Cinv=solve(C))
#check: C*Cinv=identity
round(Cinv %*% C)
#calculate T^2
(T=((n1*n2)*(t(xbar1-xbar2))%*% Cinv %*% (xbar1-xbar2))/(n1+n2))
#equivalent F where p=5
(F=(n1+n2-5-1)* T/{(n1+n2-2)*5})
(Fcrit=qf(0.05,5,43, lower.tail=FALSE) )
(Fprob=pf(0.517,5,43, lower.tail=FALSE) )

#or the easy way
library(DescTools)
(result <- HotellingsT2Test(cbind(Length, Extent, Head , Humerus, Sternum) ~ Survival))
names(result)
result$p.value
result$alternative
result$statistic

##anova
# Total SS 
(mean<- mean(sp$Length))
(T_Length<-sum((Length-mean)^2))

# Within SS
#remember:(mean1<- mean(survival1$Length)) AND meann2<- mean(survival2$Length))
(W_Length<-(sum((survival1$Length-mean1)^2))+(sum((survival2$Length-mean2)^2)))

# Between SS
(B_Length<-T_Length-W_Length)

##check
(sp.anova<-anova(lm(Length~Survival)))

attach(sp)
X <- as.matrix(sp[,c("Length","Extent","Head", "Humerus", "Sternum")])
sp.manova <- manova(X ~ as.factor(Survival))

summary(sp.manova, test="Pillai")

summary(sp.manova, test="Wilks")

summary(sp.manova, test="Roy")

summary(sp.manova, test="Hotelling-Lawley")



