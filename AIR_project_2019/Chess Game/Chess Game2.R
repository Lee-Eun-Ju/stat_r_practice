library(dplyr)
library(ggplot2)

data <- read.csv("C://Users//이은주//Desktop//AIR 동아리//#Sillicon Valley//Chess game//games.csv", header=TRUE)
glimpse(data)
summary(data)

#################################################################################변수정리
###TRUE와 True / False와 FALSE
data$rated <- as.character(data$rated)
data[which(data$rated=="True"),2] = "TRUE"
data[which(data$rated=="False"),2] = "FALSE" #열의 index를 지정(data[Ti,rated]는 error)
data$rated <- as.factor(data$rated)
summary(data)


###id 변수 -> 동일한 id, 모든 변수 또한 다르므로 행 제거
data[data$id=="XRuQPSzH",]
data[data$id=="0gTDO6Av",]
data[data$id=="CvakmVNb",]
data[data$id=="VW8jn694",]

data = data[-which(duplicated(data$id)),] #id 고유한 행만 추출
data <- data[,-1]


###turns
ggplot(data,aes(x=winner,y=turns)) + geom_boxplot()
#이상치가 보이나 바로 제거한다기 보다, 다른 변수값들도 확인해보아야 함!
data[which(data$turns == 349),]

#자신의 턴이 아닐 때 이기는 경우는 "resign"
#턴수가 짝수일 때 white가 winner인 경우
summary(data[data$turns %% 2 == 0 & data$winner == "white", 5])
#턴수가 홀수일 때 black이 winner인 경우
summary(data[data$turns %% 2 == 1 & data$winner == "black", 5])

#mate -> 턴수가 짝수:black, 턴수가 홀수:white 
summary(data[data$turns %% 2 == 0 & data$victory_status == "mate",6]) #black
summary(data[data$turns %% 2 == 1 & data$victory_status == "mate",6]) #white

#turns가 하나일 경우, resign or outoftime이다.
summary(data[which(data$turns == 1),5])


###increment_code
summary(data$increment_code)
data$increment_code <- as.character(data$increment_code) 

inc <- data$increment_code
inc <- gsub("[:+:]", " ", inc) #특수문자일경우 패턴[: :]을 표시해야함.
inc <- strsplit(inc," ") #" "단위로 끊기
head(inc)

time_start = vector()
for (i in 1:length(inc)){
  time_start[i] <- inc[[i]][1]
}

time_plus = vector()
for (i in 1:length(inc)){
  time_plus[i] <- inc[[i]][2]
}
#time_plus가 1~30까지 모두 존재하므로 pass
data$increment_code <- as.factor(data$increment_code) 


###white_rating과 black_rating
#winner:white->rating이 낮을 경우
#winner:black->rating이 낮을 경우
rating_difference = vector()
for (k in 1:dim(data)[1]){
  if(data$winner=='white'){
    rating_difference[k] = data[k,9] - data[k,11]
  }
  else if(data$winner=='black'){
    rating_difference[k] = data[k,11] - data[k,9]
  }
  else{
    rating_difference[k] = 0
  }
}
data <- cbind(data,rating_difference)
ggplot(data,aes(x=rating_difference,group=victory_status,colour=victory_status)) +geom_freqpoly()
ggplot(data,aes(x=victory_status,y=rating_difference)) +geom_boxplot()

###created_at과 last_move_at -> time 변수(created_at과 last_move_at이 같을 경우 랜포를 통해 예측)
time = vector()
for (t in 1:length(data$created_at)){
  time[t] = data$last_move_at[t] - data$created_at[t]
  }

data <- cbind(data,time)
data <- data[,-c(2,3)] #created_at,move_at 삭제

dim(data)
str(data)

mydata = data[,-c(5,6,8,10:12)] # 필요없는 column들 없앰
                        #factor형이 너무 많을 때, 함수 실행x
summary(mydata)

# lm1 <- glm(winner~., data = data)
library(randomForest)
rf1 <- randomForest(y=factor(mydata$winner),x=mydata[,-4])
importance(rf1)
pred1 = predict(rf1,mydata) ## 모델 rf1을 통한 예측

library(caret)
install.packages("e1071")
library(e1071)
confusionMatrix(pred1, mydata$winner) ## 예측값과 실제값의 CONFUsion MATRIx

str(time)
length(time)
length(time[-which(time == 0)])
time1 <- time[-which(time==0)]
data2 = data[-which(time == 0),-c(5,6,8,10:12,15)]
data3 <- cbind(data2, time1)
str(data3)
rf2 <- randomForest(y=data3$time1, x=data3[,-9])

str(mydata)
colnames(data3)
dim(mydata)
mydata$time1 = 0
colnames(mydata[,-c(2,3)])
fill = predict(rf2, mydata[which(time==0),])

data[which(time==0),15] = fill
str(data)
summary(data)


###필요없는 변수 삭제
#opening_eco-> a,b,c,d,e로 범주
#summary(data)
#data[which(data$opening_eco=="A00" & data$opening_name=="Hungarian Opening"),c(3,4)]
#사이트에서 opening_eco와 opening_name의 일치성이 없다.

summary(data)
data<- data[,-c(5,6,7,8,9,10,11,12)]


#########################################################################################EDA
###분석 목적: victory_status에 영향을 주는 요인
#draw(무승부) 신청 가능 , mate(궁이 몰려 지게 됨) , outoftime(시간초과), resign(포기를 인정)

#is.na-> 그래프로 보는 방법
#install.packages("Amelia")
#library(Amelia)
#missmap(data, col= c('white','cyan')  , main ='missmap of data')

#상관계수 그리는 방법
#corrplot(cor(data[,-c(1,2,6,7,8,9,11,13,14,15)]), method='number')


###rated <-> victory_status
ggplot(data,aes(x=rated,group=victory_status,fill=victory_status)) + geom_bar()
#로그변환
ggplot(data,aes(x=rated,group=victory_status,fill=victory_status)) + geom_bar() +scale_y_log10()
#rated에 따라 victory_status에 영향이 없어보임.



###turns
ggplot(data,aes(x=turns,group=victory_status,colour=victory_status)) + geom_freqpoly()
ggplot(data,aes(x=turns,group=victory_status,colour=victory_status)) + geom_freqpoly() +scale_x_log10()

ggplot(data,aes(x=victory_status,y=turns)) + geom_boxplot() + geom_jitter(alpha=0.1)
#큰 차이는 없으나 turn수가 길어질수록 draw신청이 많아진다.
#turn수가 많아질수록 게임을 이기는 방법은 resign->mate->outoftime->draw순인가..? 
#독립표본t-test
var.test(data[which(data$victory_status=="draw"),2],data[which(data$victory_status=="mate"),2])
var.test(data[which(data$victory_status=="draw"),2],data[which(data$victory_status=="outoftime"),2])
var.test(data[which(data$victory_status=="draw"),2],data[which(data$victory_status=="resign"),2])
var.test(data[which(data$victory_status=="mate"),2],data[which(data$victory_status=="outoftime"),2])
var.test(data[which(data$victory_status=="mate"),2],data[which(data$victory_status=="resign"),2])
var.test(data[which(data$victory_status=="outoftime"),2],data[which(data$victory_status=="resign"),2])
#p-value가 0.05보다 작으므로 등분산성을 만족한다.

#paired=TRUE이면 대응표본, False이면 독립표본
#var.equal=TRUE이면 등분산성 만족
#conf.level은 신뢰구간으로, 95%신뢰구간

t.test(data[which(data$victory_status=="draw"),2],data[which(data$victory_status=="outoftime"),2],  
       paired =FALSE, var.equal = TRUE, conf.level = 0.95)
#draw turn수 > outoftime turn수
t.test(data[which(data$victory_status=="mate"),2],data[which(data$victory_status=="outoftime"),2],  
       paired =FALSE, var.equal = TRUE, conf.level = 0.95)
#mate turn수 < outoftime turn수
t.test(data[which(data$victory_status=="mate"),2],data[which(data$victory_status=="resign"),2],  
       paired =FALSE, var.equal = TRUE, conf.level = 0.95)
#mate turn수 > resign turn수
###turn수가 많아질수록 resign<mate<outoftime<draw



###winner
ggplot(data,aes(x=winner,group=victory_status,fill=victory_status)) + geom_bar() + scale_y_log10()

#outoftime인데 draw인 경우?-> 시간초과가 된 플레이어가 지게 되므로 이경우는 이상치..?
#nono,, 위키책에 따르면 시간초과가 된경우에도 동점상황가능.

ggplot(data,aes(x=victory_status,fill=winner)) + geom_bar()
#mate나 resign일 경우에는 동점이 될 수 없다.



###opening_ply
ggplot(data,aes(x=victory_status,y=opening_ply)) + geom_boxplot() 
ggplot(data,aes(x=opening_ply,group=victory_status,colour=victory_status)) +geom_freqpoly() 
#큰 연관이 보이지 않는다.->t-검정으로 확인

summarise(group_by(data,victory_status),mean(opening_ply))
#draw>resign>outoftime>mate

var.test(data[which(data$victory_status=="draw"),5],data[which(data$victory_status=="resign"),5])
#등분산성 만족
t.test(data[which(data$victory_status=="draw"),5],data[which(data$victory_status=="resign"),5],
       paired=FALSE, var.equal=TRUE,conf.level=0.95)
#draw와 resign의 opening_ply는 동일하다 볼 수 있다.

var.test(data[which(data$victory_status=="draw"),5],data[which(data$victory_status=="outoftime"),5])
#등분산성 만족
t.test(data[which(data$victory_status=="draw"),5],data[which(data$victory_status=="outoftime"),5],
       paired=FALSE, var.equal=TRUE,conf.level=0.95)
#draw opening_ply >outoftime opening_ply

var.test(data[which(data$victory_status=="resign"),5],data[which(data$victory_status=="outoftime"),5])
#등분산을 만족하지 않는다.
t.test(data[which(data$victory_status=="resign"),5],data[which(data$victory_status=="outoftime"),5],
       paired=FALSE, var.equal=FALSE,conf.level=0.95)
#resign opening_ply >outoftime opening_ply

var.test(data[which(data$victory_status=="outoftime"),5],data[which(data$victory_status=="mate"),5])
#등분산성 만족
t.test(data[which(data$victory_status=="outoftime"),5],data[which(data$victory_status=="mate"),5],
       paired=FALSE, var.equal=TRUE,conf.level=0.95)
#outoftime opening_ply >mate opening_ply
#opening_ply는 draw=resign>outoftime>mate



###rating_difference
ggplot(data,aes(x=victory_status,y=rating_difference)) + geom_boxplot() +geom_jitter(alpha=0.01)
ggplot(data,aes(x=abs(rating_difference),group=victory_status,colour=victory_status)) + geom_freqpoly() +scale_x_log10()

#순위차가 마이너스인 경,우 순위가 낮은사람이 이긴것
for (i in seq(50,900,50)){
  th_rd = data[which(abs(data$rating_difference)<=i & data$rating_difference<0),6]
  th_g = data[which(abs(data$rating_difference)<=i),6]
  percent = (length(th_rd)/length(th_g))*100
  cat(i-50,"<x<=",i,"  ",percent,"%\n")
  }
#순위차가 커진다고 해서, 승패에 영향을 주진 않는다.



###time
ggplot(data,aes(x=victory_status,y=time)) + geom_boxplot() +scale_y_log10()
ggplot(data,aes(x=time,group=victory_status,colour=victory_status)) +geom_freqpoly() + scale_x_log10()

summarise(group_by(data,victory_status),mean(time))
#평균; draw>outoftime>mate>resign

var.test(data[which(data$victory_status=="draw"),7],data[which(data$victory_status=="outoftime"),7])
#등분산성을 만족하지 않는다.
t.test(data[which(data$victory_status=="draw"),7],data[which(data$victory_status=="outoftime"),7],
       paired=FALSE, var.equal=FALSE,conf.level=0.95)
#draw와 outoftime의 time은 동일하다 볼 수있다.

var.test(data[which(data$victory_status=="outoftime"),7],data[which(data$victory_status=="mate"),7])
#등분산성 만족
t.test(data[which(data$victory_status=="outoftime"),7],data[which(data$victory_status=="mate"),7],
       paired=FALSE, var.equal=TRUE,conf.level=0.95)
#outoftime time >mate time

var.test(data[which(data$victory_status=="mate"),7],data[which(data$victory_status=="resign"),7])
#등분산성 만족
t.test(data[which(data$victory_status=="mate"),7],data[which(data$victory_status=="resign"),7],
       paired=FALSE, var.equal=TRUE,conf.level=0.95)
#mate time >resign time

#time은 draw=outoftime>mate>resign

######################################################################모델링
#lm -> 목적변수가 수량형
#glm -> 목적변수가 범주형

#목적변수의 범주가 3개 이상일 때 할 수 있는 분석
#: 부스팅 - gbm,xgboost

############################################################################
#white_Rating,black_rating 다중공선성 확인..? vif를 통해서..?
#install.packages("VIF")
#library(VIF)
#vif는 회귀분석시, 독립변수들간의 상관관계를 확인

set.seed(1234)
n = nrow(data) 
idx = 1:n 

train_idx = sample(idx,n*0.7,replace=F)
length(train_idx)
training = data[train_idx,]
training
test = data[-train_idx,]
length(test[,1])


#####glm은 목적변수의 범주가 2개여야한다.
#####randomForest
install.packages("randomForest")
library(randomForest)

set.seed(1234)
ad_rf <- randomForest(victory_status~.,training,importance=TRUE)
ad_rf #오분류율:38.61%

#변수중요도
varImpPlot(ad_rf) #rating_difference ->turns->victory_status
plot(ad_rf) #초록->mate에 대한 오분류율
            #빨강->draw에 대한 오분류율
            #찐파랑->outoftime에 대한 오분류율
            #연파랑->resign에 대한 오분류율
            #검정->전체 오분류율
#범주가 3개 이상일 때, auc구할 수 없다.













