#####AIR RTEST2#####

#모집단의 이항분포
set.seed(987)
k <- dbinom(0:500, size=500, prob=.3)
plot(0:500, k, xlab="확률변수X", ylab="Probability") 

#이론적인 그래프
a <- mean(k) #모평균
b <- var(k) #모분산
n1 <- 0:500 #표본의 크기
y1 <- b/n1 #표본분산=모분산/표본의 크기
plot(n1, y1, xlab="sample size1", ylab="varience of Xbar1", col="red")

#실험적 그래프
a <- mean(k) #모평균
b <- var(k) #모분산
n2 <- c(1, 15, 25, 50, 100, 300, 500) #표본의 크기
r <- 1000 #모집단에서 표본을 뽑을 수

for(i in 1: length(n2)){
	for(j in 1:r){
		t <- rep(NA, r)
		t[j] <- mean(sample(k, n2[i],replace=T))
		}
	for(u in 1:r){
		g <- rep(NA, r)
		g[u] <- (mean(t)-t[u])^2		
		}
	q <- rep(NA, length(n2))
	q[i] <- sum(g)/(r-1)
}

plot(n2, q, xlab="sample size2", ylab="varience of Xvar2", col="blue")
	
