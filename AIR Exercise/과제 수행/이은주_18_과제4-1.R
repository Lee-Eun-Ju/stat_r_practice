#####AIR 동아리 과제#####

#example 1
x <- rnorm(50)
y <- rnorm(50)
group <- rbinom(50, size=1, prob=.5)

plot(x, y)
plot(x, y, xlab="X", ylab="Y", main="Y vs X", pch=0, col="red")
plot(x, y, xlab="X", ylab="Y", main="Y vs X", pch=5, col="blue")

plot(x, y, xlab="X", ylab="Y", main="Y vs X",
pch=ifelse(group==1, 5, 0),
col=ifelse(group==1, "red", "blue"))

plot(x, y, xlab="X", ylab="Y", main="Y vs X", type="n")
points(x[group==1], y[group==1], pch=5, col="red")
points(x[group==0], y[group==0], pch=19, col="blue")

plot(x, y, xlab="X", ylab="Y", main="Y vs X", type="n") 
points(cbind(x,y)[group==1,], pch=5, col="red") 
points(cbind(x,y)[group==0,], pch=19, col="blue")

#example 2
plot(x, y, type="l", lty=2, lwd=2, col="blue") 
plot(sort(x), sort(y), type="l", lty=2, lwd=2, col="blue")

plot(x, y, type="n")
lines(sort(x), sort(y), type="p")
lines(sort(x), sort(y), type="l")
lines(cbind(sort(x),sort(y)), type="l", lty=1, col="blue")

plot(sort(x), type="n")
lines(sort(x), type="l", pch=6, col="red") 
lines(sort(y), type="l", lty=6, col="blue")

#example 3
set.seed(10011)
x <- rnorm(1000)
group <- sample(0:1, 1000, replace=T)

hist(x, main="Histogram of X", col="orange")
hist(x, freq=FALSE, col="orange", main="Histogram with Normal Curve")

boxplot(x, main="Boxplot of X", border="orange", lwd=1)
boxplot(x~group, main="Boxplot of X by Group", names=c("Group 0", "Group 1"), border=c("red", "blue"), lwd=1)

#example 4
curve(3*x^5-5*x^3+2*x, from=-1.25, to=1.25, lwd=2, col="blue")

set.seed(100)
r <- rnorm(1000, 0, 1)
summary(r)

hist(r, prob=T)
curve(dnorm(x), from=-4, to=4, add=T)

n <- 500
set.seed(1001)
r2 <- rbinom(n, 20, 0.5)
hist(r2, breaks=0:20-1/2,
	border="red", col="pink")

curve(dgamma(x, shape=2, scale=1), from=0, to=7, lwd=2, col="red")
curve(dnorm, from=-3, to=5, lwd=2, col="red")
curve(dnorm(x, mean=2), lwd=2, col="blue", add=TRUE)

lines(c(0, 0), c(0, dnorm(0)), lty=2, col="red")
lines(c(2, 2), c(0, dnorm(2, mean=2)), lty=2, col="blue")

#example 5
n <- c(1, 10, 30, 5)
r <- 10000

a <- 0
b <- 7

mu <- (a+b)/2
sigma <- (b-a)/sqrt(12)

for(i in 1: length(n)){
	xbar <- rep(NA, r)
	sxbar <- sigma/sqrt(n[i])
	for(j in 1:r){
		x <- runif(n[i], a, b)
		xbar[j] <- mean(x)
	}
	hist(xbar, prob=T, ylim=c(0,1.5))
	nor.pdf <- dnorm(seq(mu-3*sigma,mu+3*sigma,0.1), mu, sxbar)
	lines(seq(mu-3*sigma,mu+3*sigma,0.1), nor.pdf, lty=2, col="red")
}
