#####AIR 동아리 과제3-1#####

#example 2-1
x <- 2
if(x >= 0) sqrt(x)

#example 2-2
score <- 85
if(score >= 60) cat("합격입니다.","\n")

#example 2-3
a <- 1
b <- 4
c <- 5

discrim <- b^2 - 4*a*c
if(discrim > 0) {
	roots <- c((-b-sqrt(b^2-4*a*c))/(2*a), (-b+sqrt(b^2-4*a*c))/(2*a))
} else if(discrim == 0){
	roots <- -b/2*a
} else {
	roots <- NULL
}

roots

#example 2-4
x <- c(4,5,7,10,8)
n <- length(x) #자료의 개수
sort.x <- sort(x) #순서 통계량

if(n %% 2 == 0){
	median <- (sort.x[n/2] + sort.x[1+n/2])/2
} else{
	median <- sort.x[(n+1)/2]
}

median

#example 3-1
x <- c(-1,-2,2,3)
ifelse( x>0, "Positive", "Negative")

x <- c(-1,-2,0,2,3)
ifelse(x>0, "Positive", ifelse(x==0, "Zero","Negative")) #두 개 이상의 조건식

#example 3-2
y <- matrix(1:6, nrow=2)
y

ifelse(y>=2 & y<5, 1, 0)

#example 4-1
x_list <- seq(1,7,by=2)
sum_x <- 0 #초기값 0으로 설정

for (i in x_list) {
	sum_x <- sum_x + i
	cat("현재 i의 값은 ",i,"\n")
	cat("지금까지 누적합은 ",sum_x,"\n")
}

#example 4-3
n <- 10
fibonacci <- numeric(n) #숫자 저장하는 방 10개 확보

fibonacci[1] <- 1 #첫번째 방은 1
fibonacci[2] <- 1 #두번째 방은 1
for (i in 3:n) fibonacci[i] <- fibonacci[i-2] + fibonacci[i-1]
fibonacci

#example 4-4
##PGM1
n <- 10000
x <- rep(0,n)
for (i in 1:n) x[i] <- i

##PGM2
n <- 10000
x <- 1
for (i in 2:n) x[i] <- i

#example 4-5(행렬의 열의 합을 계산)
big.mat <- matrix(1:1e+6, nrow=1000) #(1~1e6) 구성요소를 가지는 행렬
colsums <- rep(NA, dim(big.mat)[2])  #열의 합을 저장할 변수 생성

for(i in 1:dim(big.mat)[2]){
	s <- 0
	for(j in 1:dim(big.mat)[1]){
		s <- s + big.mat[j,i]
	}
	colsums[i] <- s
}

#example 5-1
F <- c(1,1) #처음 2개의 값
n <- 2 #현재 피보나치수열 F의 길이

while(F[n] <+ 300){
	n <- n+1
	F[n] <- F[n-2] + F[n-1]
}

cat("처음으로 300이 넘는 피보나치 수는 F(",n,") =", F[n], "\n")
F

#example 6-1
eps <- 1e-12 #초기값 설정
x <- 0.5 #초기값 설정
n <- 0 #초기값 설정
log1_x <- 0 #초기값 설정

repeat{
	n <- n+1
	last.term <- (-1)^(n+1)*x^n/n
	log1_x <- log1_x + last.term
	if(abs(last.term)<eps)break
}

cat("테일러 급수 값 =", log1_x, "\n")
cat("log(1+x) = ", log(1+x), "\n")