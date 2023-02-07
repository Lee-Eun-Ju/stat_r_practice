#여름방학 4주차 - FIFA 분석

library(dplyr)
library(ggplot2)

setwd("C:\\Users\\이은주\\Desktop\\AIR 동아리\\여름방학\\3주")
data0 = read.csv("FIFA 2018 Statistics.csv", header = TRUE)
data = read.csv("FIFA 2018 Statistics.csv", header = TRUE)
glimpse(data)
#변수 독립적으로 만들어주기
attach(data)
glimpse(data)
#numeric 아닌 변수들이랑 yellow&red랑 red가 거의 0이라서 삭제
data=data[,c(4:20,22)]
glimpse(data)
dim(data)
#최종 : 관측치 128개 변수 16개

#NA를 0으로 바꿔주기
data[is.na(data)]=0
head(data)

###############################################################
win = rep(0,128)
n = 2*(1:64)-1
for (i in n) {
		if (data[i,1]>data[i+1,1]){
			win[i] = "Winner"
			win[i+1] = "Loser"
			}
		else if (data[i,1] == data[i+1,1]){
			if (data0[i,24] == "Yes" & data0[i,25]>data0[i+1,25]){
				win[i] = "Winner"
				win[i+1] = "Loser"
				}
			else if (data0[i,24] == "Yes" & data0[i,25]<data0[i+1,25]){
				win[i] = "Loser"
				win[i+1] = "Winner"
				}
			else{
				win[i] = NA
				win[i+1] = NA
				}
			}
		else {
			win[i] = "Loser"
			win[i+1] = "Winner"
			}
	}

data = cbind(data,win)
data = data[,-1]
data = data[-which(is.na(data$win)),]
##############################################################랜덤포레스트
set.seed(123)
n <- nrow(data)
idx <- 1:n
train_idx <- sample(idx,n*.7)
training <- data[train_idx,]
validation <- data[-train_idx,]

set.seed(123)
library(randomForest)
ad_rf = randomForest(win~.,training)
ad_rf
plot(ad_rf)

library(ROCR)
y_obs = ifelse(validation$win == "Winner",1,0)
yhat_rf = predict(ad_rf,newdata=validation,type="prob")[,"Winner"]
pred_rf = prediction(yhat_rf,y_obs)
perf_rf = performance(pred_rf,measure="tpr",x.measure="fpr")
plot(perf_rf)
abline(0,1)

performance(pred_rf,'auc')@y.values[[1]]

#########################################################GLM
data["win"] = ifelse(data["win"]=="Winner",1,0)

set.seed(123)
n <- nrow(data)
idx <- 1:n
train_idx <- sample(idx,n*.7)
training <- data[train_idx,]
validation <- data[-train_idx,]

data_mod<-glm(win~.,data=training)

y_obs=ifelse(validation$win==1,1,0)
yhat_glm=predict(data_mod,validation)
pred_glm=prediction(yhat_glm,y_obs)
perf_glm=performance(pred_glm,measure = 'tpr',x.measure = 'fpr')
plot(perf_glm)
abline(0,1)
performance(pred_glm,'auc')@y.values[[1]]

			 