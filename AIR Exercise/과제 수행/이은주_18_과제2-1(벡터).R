#######AIR 동아리 과제2-1(벡터)#######

#Exercise 1
a <- 1:1000
sum(a)

#Exercise 2
k <- 1:100
b <- 3*k-1
log10(sum(b))

#Exercise 3
n <- 0:100
c <- 2^(-n)
sum(c)

#Exercise 4
n <- 0:100
d <- 1/factorial(n)
sum(d)



#Exercise
x <- seq(-3,3)
f <- exp(-x^2/2)/sqrt(2*pi)

#1)
min(f)
max(f)

#2)
mean(f)
median(f)

#3)
q <- quantile(f)
q[2]
q[4]

#4)
var(f)
sd(f)