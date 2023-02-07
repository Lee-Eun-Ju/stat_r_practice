#여름방학 3주차 - FIFA 분석

install.packages("corrplot")
install.packages("psych")
library(dplyr)
library(corrplot)
library(psych)
library(ggplot2)
library(tidyr)


setwd("C:\\Users\\이은주\\Desktop\\AIR 동아리\\여름방학\\3주")
data0 = read.csv("FIFA 2018 Statistics.csv", header = TRUE)
data = read.csv("FIFA 2018 Statistics.csv", header = TRUE)
glimpse(data)
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

#다중산점도 그리기->변수너무 많아서 안보임
plot(data)

#주성분분석(PCA;principal component analysis) 
#차원의 축소, 자료의 요약이 주목적이다.->자료한눈에 파악하기 쉬움 
#시각화를 하면 변수와 개체간의 연관성도 살펴볼 수 있다. 
df <- data[,-1]
df1 <- df
head(df1)
df1_corr <- cor(df1) # Create a correlation matrix
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(round(df1_corr, 2), method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         diag=FALSE) # hide correlation coefficient on the principal diagonal
round(df1_corr, 2) # Correlation matrix in table form

KMO(df1_corr)

nfactors <- 4 # Based on the screeplot suggestion

evalue = eigen(df1_corr)$values
evec = eigen(df1_corr)$vectors

# Plot Eigenvalues / Represented Variance
eigenvalues <- data.frame(eigen(df1_corr)$values)
colnames(eigenvalues) <- c("Values")
eigenvalues$Number <- 1:nrow(df1_corr)
eigenvalues$RepresentedVariance <- NA
for (i in 1:nrow(df1_corr)) {
  eigenvalues$RepresentedVariance[i] <- sum(evalue[1:i])/sum(evalue) * 
    100
}
eigenvalues$RepresentedVariance_text <- paste(round(eigenvalues$RepresentedVariance, 
                                                    0), " %")

e1 <- ggplot(eigenvalues, aes(Number, y = Values), group = 1)
e1 <- e1 + geom_bar(stat = "identity")
e1 <- e1 + geom_line(aes(y = Values), group = 2)
e1 <- e1 + xlab("Number [-]")
e1 <- e1 + ylab("Eigenvalue [-]")
e1 <- e1 + geom_hline(aes(yintercept = 1), col = "red")
e1 <- e1 + geom_text(aes(label = RepresentedVariance_text), nudge_y = 0.2)
e1 <- e1 + ggtitle("Eigenvalues and explained Variance")
e1 <- e1 + theme_bw()
e1 <- e1 + scale_x_continuous(breaks = seq(1, 10, 1))
x11()
e1


## PCA
gof = evalue/sum(evalue)*100
gof
round(gof,3)
sum(gof[1:7]) 

V = evec[,1:7]
V
rowname = colnames(df)
round(V,3)
rowname = colnames(df)
rownames(V) = rowname
V
Z = scale(df)
PS = Z%*%V

dim(PS)
PS = round(PS,3) #반올림
#####################################################
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

PS = cbind(PS,win)
head(PS)

PS = data.frame(PS)
PS$V1 = as.character(PS$V1)
PS$V2 = as.character(PS$V2)
PS$V3 = as.character(PS$V3)
PS$V4 = as.character(PS$V4)
PS$V5 = as.character(PS$V5)
PS$V6 = as.character(PS$V6)
PS$V7 = as.character(PS$V7)

PS$V1 = as.numeric(PS$V1)
PS$V2 = as.numeric(PS$V2)
PS$V3 = as.numeric(PS$V3)
PS$V4 = as.numeric(PS$V4)
PS$V5 = as.numeric(PS$V5)
PS$V6 = as.numeric(PS$V6)
PS$V7 = as.numeric(PS$V7)

glimpse(PS)
PS = PS[-which(is.na(PS$win)),]
#########################################################랜덤포레스트
set.seed(123)
n <- nrow(PS)
idx <- 1:n
train_idx <- sample(idx,n*.7)
training <- PS[train_idx,]
validation <- PS[-train_idx,]

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
#GLM은 목적변수가 문자가 아닌 1과 0으로 변경하여 주는 것이 좋음
PS["win"] = ifelse(PS["win"]=="Winner",1,0)

set.seed(123)
n <- nrow(PS)
idx <- 1:n
train_idx <- sample(idx,n*.7)
training <- PS[train_idx,]
validation <- PS[-train_idx,]

ps_mod<-glm(win~.,data=training)

y_obs=ifelse(validation$win==1,1,0)
yhat_glm=predict(ps_mod,validation)
pred_glm=prediction(yhat_glm,y_obs)
perf_glm=performance(pred_glm,measure = 'tpr',x.measure = 'fpr')
plot(perf_glm)
abline(0,1)
performance(pred_glm,'auc')@y.values[[1]]

#설명변수가 모두 수치형이므로 glm분석에 따른 auc가 더 크다.


###################################################예측값과 실제값 분포알아보기
install.packages('gridExtra')
library(gridExtra)

########rf########
p1=ggplot(data.frame(y_obs,yhat_rf),aes(y_obs,yhat_rf,group=y_obs,fill=factor(y_obs)))+geom_boxplot()
p2=ggplot(data.frame(y_obs,yhat_rf),aes(yhat_rf,fill=factor(y_obs)))+geom_density(alpha=0.5)
grid.arrange(p1,p2,ncol=2) #2개의 표 배열

########glm######
p3=ggplot(data.frame(y_obs,yhat_glm),aes(y_obs,yhat_glm,group=y_obs,fill=factor(y_obs)))+geom_boxplot()
p4=ggplot(data.frame(y_obs,yhat_glm),aes(yhat_glm,fill=factor(y_obs)))+geom_density(alpha=0.5)
grid.arrange(p3,p4,ncol=2)


