###Chap7.데이터 종류에 따른 분석 기법
library(ggplot2)
library(dplyr)

glimpse(mpg)
summary(mpg)

###7.3 수량형 변수의 분석
#요약 통계량
summary(mpg$hwy)
var(mpg$hwy)
mad(mpg$hwy) #중앙값절대편차 -관측값에서 중앙값을 뺀 값들의 중앙값

#데이터분포 시각화, 데이터 정규성 검사
opar = par(mfrow=c(2,2))
hist(mpg$hwy)
boxplot(mpg$hwy)
qqnorm(mpg$hwy)
qqline(mpg$hwy)
par(opar)

#가설검정(일변량 t검정)과 신뢰구간
t.test(mpg$hwy,mu=22.9,alternative="greater") #단측검정
#대립가설; 22.9보다 mu가 크다
#p-값=0.08328

t.test(mpg$hwy,mu=22.9,alternative="two.sided")
#대립가설; 22.9와 mu가 같지 않다.
#p-값=0.1666
#신뢰구간; 22.67<=x<=24.21 ->신뢰구간은 양측검정을 통해서 알 수 있다.

#이상점 찾기-로버스트 통계량 계산
#IQR:Q3-Q1(사분위편차)
#이상점:[Q1-1.5*IQR,Q3+1.5*IQR]
quantile(mpg$hwy)
#[18-1.5(27-18),27+1.5(27-18)]

#로버스트 통계방법 - 평균->중앙값/표준편차->mad
c(median(mpg$hwy),mad(mpg$hwy))

###7.4 성공-실패값 범주형 변수의 분석
#요약통계량
set.seed(1234)
x= rbinom(100,1,0.5) #rbinom; 이항분포로부터 난수 추출(p=0.5, 0/1 중 하나 100개 난수)
x= factor(x,levels=c(0,1),labels=c("no","yes")) 

table(x) #도수분포
#xtabs(x) -???
prop.table(table(x)) #상대도수분포

#데이터분포 시각화
barplot(table(x))

#가설검정(binom.test-성공률에 대한 검정)과 신뢰구간
binom.test(x=length(x[x=='yes']),n=length(x),p=0.5,alternative="two.sided")
#yes수,전체수,p=0.5,양측검정
#yes수/전체수->표본에 따른 성공확률

#오차한계,표본크기,sqrt(n)
#오차한계:신뢰구간 크기의 절반(허용할 수 있는 최대의 오차)
#오차한계;(0.55-0.35)/2 -> 10%오차한계

binom.test(x=4500,n=10000)
#표본이 클때 신뢰구간;(0.44,0.46)
#표본이 클때 오차한계;(0.46-0.44)/2 ->1%오차한계
#오차한계는 표본크기의 제곱근에 비례하여 줄어든다.

n = c(100,1000,2000,10000,1e6)
data.frame(n=n,moe=round(1.96*sqrt(1/(4*n)),4))
#오차한계가 줄어듦을 알 수 있다.
curve(1.96*sqrt(1/(4*x)),10,1000) #curve(함수,fromx,tox)
curve(1.96*sqrt(1/(4*x)),10,1000,log='x') #한쪽으로 치우쳐진 분포-로그변환
grid() #격자를 그리는 함수

###7.6 수량형X, 수량형Y의 분석
##1. 산점도
summary(mpg)
ggplot(mpg,aes(cty,hwy)) + geom_jitter() + geom_smooth(method='lm')
#geom_smooth(method='') -> lm,glm,gam,rlm,loess 가 있다.

##2. 상관계수
#기본적으로 피어슨상관계수 사용
cor(mpg$cty,mpg$hwy)
with(mpg,cor(cty,hwy)) 

#이상치의 영향을 줄이기 위해 kendall방법 or spearman방법
with(mpg,cor(cty,hwy,method="kendall"))
with(mpg,cor(cty,hwy,method="spearman"))

##3. 선형 모형 적합
hwy_lm = lm(hwy~cty,data=mpg) #하나의 설명변수로 이루어진 모형:단순회귀분석모형
summary(hwy_lm)

#선형회귀 모형 예측
#예측값, 잔차
predict(hwy_lm)
resid(hwy_lm)

predict(hwy_lm, newdata=data.frame(cty=c(10,20,30)))
predict(hwy_lm, newdata=data.frame(cty=c(10,20,30)), se.fit=TRUE)

#선형회귀 모형의 가정 진단
class(hwy_lm)
opar= par(mfrow=c(2,2), oma=c(0,0,1.1,0))
plot(hwy_lm,las=1)
par(opar)

#수량형 변수에 이상치 있는 경우; 로버스트 통계방법
install.packages("MASS")
library(MASS)
set.seed(123)
lqs(stack.loss~.,data=stackloss)

#비선형일 경우; 평활법(중 LOESS)
plot(hwy~displ,data=mpg)
mpg_l=loess(hwy~displ,data=mpg)
mpg_l
summary(mpg_l)

xs=seq(2,7,length.out=100)
mpg_pre=predict(mpg_l,newdata=data.frame(displ=xs),se=TRUE)
lines(xs,mpg_pre$fit)
lines(xs,mpg_pre$fit-1.96*mpg_pre$se.fit,lty=2)
lines(xs,mpg_pre$fit+1.96*mpg_pre$se.fit,lty=2)

ggplot(mpg,aes(displ,hwy))+geom_point()+geom_smooth()

###7.7 범주형X, 수량형Y 분석
##1.분산분석(ANOVA) - 선형모형의 특별한 종류
ggplot(mpg,aes(class,hwy)) + geom_boxplot()
hwy_lm2 = lm(hwy~class,data=mpg)
summary(hwy_lm2)

opar= par(mfrow=c(2,2), oma=c(0,0,1.1,0))
plot(hwy_lm2,las=1)
par(opar)

###7.8 수량형X, 범주형Y(성공, 실패)
##1.일반화 선형 모형, 로짓/로지스틱 함수
chall = read.csv('https://raw.githubusercontent.com/stedy/Machine-Learning-with-R-datasets/master/challenger.csv')
chall = tbl_df(chall)
glimpse(chall)

ggplot(chall, aes(temperature,distress_ct)) + geom_point()
ggplot(chall, aes(factor(distress_ct),temperature)) + geom_boxplot()

chall_glm = glm(cbind(distress_ct,o_ring_ct-distress_ct)~temperature,data=chall,family='binomial')
summary(chall_glm)

predict(chall_glm,data.frame(temperature=30)) #선형예측값을 출력

#성공확률값-> 로지스틱변환/predict.glm()에서 type="response"
exp(3.45)/(exp(3.45)+1)
predict(chall_glm,data.frame(temperature=30),type='response')

#로지스틱모형 적합결과의 시각화
logistic= function(x){exp(x)/(exp(x)+1)}
plot(c(20,85),c(0,1),type="n",xlab="temperature",ylab="prob")
tp= seq(20,85,1)
chall_glm_pred = predict(chall_glm,data.frame(temperature=tp),se.fit=TRUE)
lines(tp,logistic(chall_glm_pred$fit))
lines(tp,logistic(chall_glm_pred$fit-1.96*chall_glm_pred$se.fit),lty=2)
lines(tp,logistic(chall_glm_pred$fit+1.96*chall_glm_pred$se.fit),lty=2)
abline(v=30,lty=2,col='blue')
