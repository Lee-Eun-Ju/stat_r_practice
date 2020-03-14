#####AIR 동아리 과제3-3#####

##Exercise 1
#(1)
m <- sample(c(0,1), 1000, replace=T, prob=c(.7,.3))

#(2)
table(m)

#(3)
mean(m)
var(m)

#(4)
n <- sample(0:10, 10000, replace=T, prob=c(.07,.07,.07,.07,.07,.07,.07,.07,.07,.07,.3))

#(5)
table(n)

#(6)
mean(n)
var(n)


##Exercise 2
#(1)
p <- rnorm(10000, mean=0 , sd=1)
mean(p)
median(p)

#(2)
var(p)
sd(p)

#(3)
sum(p)

#(4)
q <- rnorm(10000, mean=1 , sd=2)
mean(q)
median(q)
var(q)
sd(q)
sum(q)