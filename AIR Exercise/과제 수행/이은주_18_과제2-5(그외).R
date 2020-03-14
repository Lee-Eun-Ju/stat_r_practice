###AIR 동아리 과제2-5###

#Exercise
x <- c(-1:1)
(exp(x)+exp(-x))/2^abs(x) > 1+sqrt(abs(x)*(1-abs(x)))/(x+sqrt(2))

#결측치와 다른 특수한 값들
#NA: 무언가 있어야할 위치의 자료값을 알 수 없는 것
a <- NA  #a에 NA 할당
is.na(a) #a의 값은 결측치인가?
a <- c(0,1,2,NA,5)
is.na(a)

mean(a) #자료값 하나가 NA이기 때문에 평균 또한, NA -> NA는 전파된다.
mean(a,na.rm=TRUE) #NA를 제거한 결과값

which(a >= 2) #which함수는 항상 결측치를 무시


#NULL: 존재하지 않는 자료값
b <- c(0,1,2,NULL,5)
mean(b) #NULL은 무시하고 계산

length(a) #자료의 개수를 반환해주는 함수, NA는 포함
length(b) #NULL은 무시


#NaN: 계산의 결과를 정할 수 없는 숫자
a/a
1/a
round(log(a),3)

a
a/0
is.na(a/0)       #자료가 NA 또는 NAN을 포함하고 있는지 확인
is.nan(a/0)      #자료가 NAN을 포함하고 있는지 확인
is.infinite(a/0) #자료가 무한의 형태인지 확인
is.finite(a/0)   #자료가 무한하지 않는 잘 정의된 숫자인지 확인
sum(is.na(a/0))  #TRUE=1, FALSE=0