#AIR 첫번째주

data <- read.table("C:\\Users\\이은주\\Desktop\\AIR 동아리\\여름방학\\1주\\customer.txt", sep='\t',header=T)
summary(data)

library(dplyr)
glimpse(data) #데이터가 어떤 형태인지 정렬

#lgl(논리연산자)형태를 factor형으로 변환
data$is.employed <- as.factor(data$is.employed)
data$health.ins <- as.factor(data$health.ins)
data$recent.move <- as.factor(data$recent.move)
glimpse(data)
summary(data)

data$num.vehicles <- as.factor(data$num.vehicles)  
summary(data)

x <- data[,-1] #customer id는 필요없으므로 행을 삭제
summary(x)

#logistic 회귀분석 -> 0과 1로 구분되는 범주형자료일 때 사용
#건강보험유무 체크
#-> y가 값이 아니라 0일 확률이거나 1일 확률을 의미이다.

#회귀분석하기 전에 EDA 필요!!!! -> 자료를 정리하기 위해서 EDA 과정이 필요로하다.

#################################################EDA(탐색적 자료분석)


#각각의 변수를 정리 -> y값은 건드리면 안된다!!!
#특히 y에 NA는 대체값을 쓰면 안되고 무조건 빼야 한다.


##########is.employed 변수에 대해 알아보기
#-> NA처리 : 연속형자료일 때, 최빈값/평균값/중간값 등으로 처리할수 있지만
#데이터를 변형시킬 때는 무조건 신중!!

#NA -> income 평균 이상인 고객들은 직업을 가지고 있다고 할 수 있지 않을까?

upincome = which(x$income >= mean(x$income)) #수입이 평균이상인 index 뽑기(사람 뽑기)
a = x[upincome,] #수입이 평균이상인 사람들의 자료 a 생성
summary(a) #is.employed 변수에서 FALSE인 자료 4명이 있으므로 이 생각은 잘못된 것이라 볼 수 있다.

#그러므로 그냥 직업이 NA인 사람(index) 빼기
xna_index = which(is.na(x$is.employed)==TRUE)
x = x[-xna_index,]



##########income 변수에 대해 알아보기
summary(x)
boxplot(x$income) 
#income (-) 인 경우 : 사업을 하는 사람에게, 수입이 가변비용보다 낮을 때 (-)일 수 있다.

#income 최대인 경우 어떻게 처리??
boxplot(x$income)$stats #boxplot의 기초통계량 값을 나타냄
#5번째 값을 기준으로 그 이상은 이상치로 처리
length(which(x$income > 165200))

#이상치인 자료 빼기
x = x[-which(x$income > 165200),]
boxplot(x$income)



##########marital.stat 변수에 대해 알아보기
summary(x)
#divorced/separated 변수와 widowed 변수 병합
#(widowed의 자료가 12개로 분석을 할 때 자료가 적어 인식을 하지 못한다)

x$marital.stat <- as.character(x$marital.stat)
glimpse(x)

mar_ds_index = which(x$marital.stat == "Divorced/Separated")
mar_wi_index = which(x$marital.stat == "Widowed")

x[mar_ds_index,4] = "alone" #4번째 변수가 marital.stat
x[mar_wi_index,4] = "alone"

x$marital.stat <- as.factor(x$marital.stat)
summary(x)



##########housing.type 변수에 대해 알아보기
#NA값 삭제
x = x[-which(is.na(x$housing.type) == TRUE),]
summary(x)


##########num.vehicles 변수에 대해 알아보기
##4 이상인 변수 병합

#character은 아예 글자로 보지만, 프로그래밍을 할 수 없다(에러가 남).
#factor는 범주로 보는 것으로써, 프로그래밍을 할 수 있다.

x$num.vehicles <- as.character(x$num.vehicles)
x$num.vehicles <- as.integer(x$num.vehicles) #numeric이나 integer이나 상관없

x[which(x$num.vehicles >=4),8] = "4>="
x$num.vehicles <- as.factor(x$num.vehicles)
summary(x)


##########age 변수에 대해 알아보기
boxplot(x$age)$stats
x = x[-which(x$age == 0),]
x = x[-which(x$age > 77),]
summary(x)


##########state.of.res 변수에 대해 알아보기
#범주(53개)가 너무 많아 재범주화함
#각 나라를 남부,서부,동부,북부 등으로 나눠서 다시 범주를 재정렬 
#변수를 part라 하고 west/northeast/south/west로 범주를 나누었다.

x$part[x$state.of.res=='Nevada']<-'west'
x$part[x$state.of.res=='New Mexico']<-'west'
x$part[x$state.of.res=='Montana']<-'west'
x$part[x$state.of.res=='Arizona']<-'west'
x$part[x$state.of.res=='Idaho']<-'west'
x$part[x$state.of.res=='Alaska']<-'west'
x$part[x$state.of.res=='Oregon']<-'west'
x$part[x$state.of.res=='Wyoming']<-'west'
x$part[x$state.of.res=='Washington']<-'west'
x$part[x$state.of.res=='Utah']<-'west'
x$part[x$state.of.res=='California']<-'west'
x$part[x$state.of.res=='Colorado']<-'west'
x$part[x$state.of.res=='Hawaii']<-'west'
x$part[x$state.of.res=='Nebraska']<-'midwest'
x$part[x$state.of.res=='North Dakota']<-'midwest'
x$part[x$state.of.res=='Minnesota']<-'midwest'
x$part[x$state.of.res=='Michigan']<-'midwest'
x$part[x$state.of.res=='Missouri']<-'midwest'
x$part[x$state.of.res=='South Dakota']<-'midwest'
x$part[x$state.of.res=='Ohio']<-'midwest'
x$part[x$state.of.res=='Iowa']<-'midwest'
x$part[x$state.of.res=='Wisconsin']<-'midwest'
x$part[x$state.of.res=='Indiana']<-'midwest'
x$part[x$state.of.res=='Illinois']<-'midwest'
x$part[x$state.of.res=='Kansas']<-'midwest'
x$part[x$state.of.res=='New Jersey']<-'northeast'
x$part[x$state.of.res=='New York']<-'northeast'
x$part[x$state.of.res=='New Hampshire']<-'northeast'
x$part[x$state.of.res=='Rhode Island']<-'northeast'
x$part[x$state.of.res=='Massachusetts']<-'northeast'
x$part[x$state.of.res=='Maine']<-'northeast'
x$part[x$state.of.res=='Vermont']<-'northeast'
x$part[x$state.of.res=='Connecticut']<-'northeast'
x$part[x$state.of.res=='Pennsylvania']<-'northeast'
x$part[x$state.of.res=='Alabama']<-'south'
x$part[x$state.of.res=='Delaware']<-'south'
x$part[x$state.of.res=='Florida']<-'south'
x$part[x$state.of.res=='Georgia']<-'south'
x$part[x$state.of.res=='Kentucky']<-'south'
x$part[x$state.of.res=='Louisiana']<-'south'
x$part[x$state.of.res=='Maryland']<-'south'
x$part[x$state.of.res=='Mississippi']<-'south'
x$part[x$state.of.res=='North Carolina']<-'south'
x$part[x$state.of.res=='Oklahoma']<-'south'
x$part[x$state.of.res=='South Carolina']<-'south'
x$part[x$state.of.res=='Tennessee']<-'south'
x$part[x$state.of.res=='Texas']<-'south'
x$part[x$state.of.res=='Virginia']<-'south'
x$part[x$state.of.res=='West Virginia']<-'south'
x$part[x$state.of.res=='Arkansas']<-'south'
x$part <- as.factor(x$part)
summary(x$part)


##########변수 간의 관계
###성별과 y값 관계
summary(x)
table(x$sex,x$health.ins)
#둘 다 범주형이므로 boxplot은 할 수 없다.
#비로 보아야 한다.(여성의 수와 남성의 수가 같지 않으므로, 자료의 개수는 의미가없다.)
211/(39+211) #여성이 보험가입되어있는 경우(0.844)
291/(69+291) #남성이 보험가입되어있는 경우(0.8083333)

###직접의유무와 y값 관계
is_y = table(x$is.employed,x$health.ins)
43/(27+43) #직업이 없을 때 보험가입되어있는 경우(0.6142857)
459/(81+459) #직업이 있을 때 보험가입되어있는 경우(0.85)
#보험가입을 하면 내야할 보험료가 있기 때문에 직업이 있을 때 보험가입율이 높을 것이다.

###income과 y값 관계
library(ggplot2)
ggplot(data=x,aes(x=health.ins, y=income,fill=health.ins)) + geom_boxplot()
#수입이 높을수록 보험가입율이 높은 것으로 보인다.

###marital.stat와 y값 관계
table(x$marital.stat,x$health.ins)
#결혼을 한적이 있다면 보험가입율이 높은 것으로 보인다.
#왜그럴까> 1)결혼을 한다는 것은 일정한 수입이 있는 것으로 보인다.
#		2)이후 한번 등록하고 나서 잘 해지하지 않게 된다.

###housing.type과 y값 관계
table(x$housing.type,x$health.ins)
#mortage/loan -> free and clear -> rented -> no rent 순으로
#보험가입율이 높으므로 
#우리가 생각했던 이상적인 모형과 다르다.
#즉, housing.type은 health.ins에 영향을 주지 않는다고 볼 수 있다.
#주의 : 만약, 어느 한 범주가 False가 높다면 y에 영향을 준다고 볼 수 있다.
#	but 모두 true가 높음.

###recent.move와 y값 관계 -> 굳이 보지 않아도 됨

###num.vehicles와 y값 관계
table(x$num.vehicles,x$health.ins)
#자동차수가 2대 이상이면 보험가입이 true일 확률이 높다.

###age와 y값 관계
ggplot(data=x,aes(x=health.ins, y=age,fill=health.ins)) + geom_boxplot()
#나이가 많을수록 보험에 가입되있을 확률이 높다.

###state.of.res를 보았을 때 어떠한 주들은, 관측치가 1개인 경우도 있을 정도로
###고르지 않으므로 자료가 유의하지 않다.

###part와 y값 관계
table(x$part,x$health.ins) #각 part별로 큰 차이가 드러나지 않는다.


########################################################분석 - logistic 회귀분석
#데이터를 training set, validation set, test set을 나눈다.(6:2:2)
#training set을 통해 모형을 만들고
#모형이 데이터에 대한 과적합인지 검증하기 위해 validation set을 통해 모형진단
#test set은 결론을 낼 때 최종적으로 사용하는 set이다.

#이번에는 training과 test 두가지로 나눈다.(7:3) 

#분석의 재현성을 위해 set.seed 사용하여 sampling함으로써 데이터 분할
set.seed(1234)
n = nrow(x) #데이터의 행의 개수
idx = 1:n #벡터형으로 index를 둔다.

train_idx = sample(idx,n*0.7,replace=F) #각 행의 index 중 70%뽑기
length(train_idx)
training = x[train_idx,]
training
test = x[-train_idx,]
length(test[,1]) #183개로 확인할 수 있다.

glm_0 = glm(health.ins~.-state.of.res,training,family="binomial") #y가 범주형인지 연속형인지 정의
summary(glm_0) #AIC는 346.8

###glm의 summary를 보는 방법
#AIC : 작을수록 좋다. -> 상실된 정보의 양(회귀분석에서 추정값과 관측값의 차를 에러라고 하고 아마도 그것..)
#귀무가설 : 변수가 y값에 영향을 주지 않는다.
#p-value가 유의확률(0.05)보다 작으면 기각
#그 옆의 별 3개일 때는 유의확률 0.01보다 작고, 2개일 때는 0.05보다 작고, 1개일 때는 0.1보다 작다.

#intercept 기각하지 못하므로, -1로 제거함.
glm_1 = glm(health.ins~.-state.of.res,training,family="binomial")
summary(glm_1)

#변수_범주 원리: 가변수화

#forward,backward,stepwise를 통해 aic가 가장 낮은 것을 뽑는다.
glm_s = step(glm_0,direction="both")
summary(glm_s)

##glm에서 로지스틱회귀는 일반회귀와 다르게 해석된다.
-----------------------------------------------------------------
###test data에 적합해보기
pred = predict(glm_s,newdata=test) 
#만들어진 모델에 test data(x)를 적합하여 추정된 값(f(x))

library(ROCR)
pr = prediction(pred,test$health.ins) 
#[test data의 실제 y값 : test$health.ins] 과 [추정된 값(f(x)) : pred]를 비교
#예시 -> 추정F일 때, 실제F일 확률 + 추정T일 때, 실제T일 확률 : 정분류율 -> 그 반대 : 오분류율

#pr은 ROC 커브 그리기 위한 것
perf = performance(pr,measure="tpr",x.measure="fpr")
plot(perf)
abline(0,1)

#ROC 커브만으로 모두 알 수 없으므로 정확한 값
#=ROC커브의 적분 -> ROC 커브가 abline(0,1)보다 낮으면 쓸모없는 모델이다.
#=AUC=정분류율
auc = performance(pr,measure="auc")@y.values[[1]]
auc #최종 모델의 auc값은 0.6921182로, 정분류율이 그 값이다.

############최종결론########### 
#stepwise를 통해 AIC가 가장 낮은 값을 최종 모델로 잡았고 그 auc값은 0.69이다.
summary(glm_s)
#변수는 sex,intercept,income,marital.stat,housing.type만이 사용되었고
#AIC값은 330.86로 가장 작아 최종 모델로 잡았고
#그 최종모델의 AUC값은 0.69이다.	









