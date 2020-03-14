#####Chap9. 빅데이터 분류분석II: 라쏘와 랜덤 포레스트
install.packages(c("dplyr","ggplot2","ISLR","MASS","glmnet",
                   "randomForest","gbm","rpart","boot"))
install.packages("ROCR")
install.packages("performance")

library(dplyr)
library(ggplot2)
library(ISLR)
library(MASS)
library(glmnet)
library(randomForest)
library(gbm)
library(rpart)
library(boot)
library(ROCR)
library(performance)

binomial_deviance = function(y_obs, yhat){
  epsilon = 0.0001
  yhat = ifelse(yhat < epsilon, epsilon, yhat)
  yhat = ifelse(yhat > 1-epsilon, 1-epsilon, yhat)
  a = ifelse(y_obs==0, 0, y_obs*log(y_obs/yhat))
  b = ifelse(y_obs==1, 0, (1-y_obs)*log((1-y_obs)/(1-yhat)))
  return(2*sum(a+b))
}

###9.1 glmnet 함수를 통한 라쏘 모형, 능형회귀, 변수 선택
adult = read.csv("C:\\Users\\eunju\\Desktop\\GitHub\\stat_r_practice\\Sillicon Valley\\adult.data", 
                 header=FALSE, strip.white=TRUE)
names(adult) = c('age','workclass','fnlwgt','education',
                 'education_num','marital_status','occupation',
                 'relationship','race','sex',
                 'capital_gain','capital_loss',
                 'hours_per_week','native_country','wage')
glimpse(adult)
summary(adult)

set.seed(1234)
n = nrow(adult)
idx = 1:n
training_idx = sample(idx, n*.6)
idx = setdiff(idx, training_idx)
validate_idx = sample(idx, n*.2)
test_idx = setdiff(idx, validate_idx)
length(training_idx)
length(validate_idx)
length(test_idx)
training = adult[training_idx,]
validation = adult[validate_idx,]
test = adult[test_idx,]

#glm
ad_glm_full = glm(wage~., data=training, family=binomial)
y_obs = ifelse(validation$wage == ">50K", 1, 0)
yhat_lm = predict(ad_glm_full, newdata=validation, type='response')
pred_lm = prediction(yhat_lm, y_obs)
perf_lm = performance(pred_lm, measure="tpr", x.measure="fpr")

#모형행렬 수동 작성(설명변수에 범주형변수가 포함하므로)
xx = model.matrix(wage~.-1, adult) #wage는 빼기
x = xx[training_idx,]
y = ifelse(training$wage == ">50K", 1, 0)
dim(x)

#glmnet; 기본(알파=1로 라쏘회귀 이용) 
#가장 간단한 모형부터 복잡한 모형까지 볼 수 있음.
ad_glmnet_fit = glmnet(x,y) #x;입력메트릭 y;응답벡터
plot(ad_glmnet_fit)
ad_glmnet_fit

#coef; Lambda값에 해당하는 모수 추정값들 확인
coef(ad_glmnet_fit, s=c(0.1736, 0.1313)) #s; 확인하고자 하는 Lambda값

#cv.glmnet; 자동 모형 선택
#lambda.min - 최적의 예측력, lambda.1se - 해석 가능한 모형
ad_cvfit = cv.glmnet(x,y,family="binomial")
plot(ad_cvfit)

log(ad_cvfit$lambda.min)
log(ad_cvfit$lambda.1se)

coef(ad_cvfit, s=ad_cvfit$lambda.min)
coef(ad_cvfit, s="lambda.1se")

#선택된 모형의 모수개수 확인
length(which(coef(ad_cvfit, s="lambda.min")>0))
length(which(coef(ad_cvfit, s="lambda.1se")>0))

#다른 알파값 설정을 통해 비교
set.seed(1234)
foldid = sample(1:10, size=length(y), replace=TRUE)
cv1 = cv.glmnet(x,y, foldid=foldid, alpha=1, family='binomial')
cv.5 = cv.glmnet(x,y, foldid=foldid, alpha=.5, family='binomial')
cv0 = cv.glmnet(x,y, foldid=foldid, alpha=0, family='binomial')

par(mfrow=c(2,2))
plot(cv1, main="Alpha=1.0")
plot(cv.5, main="Alpha=0.5")
plot(cv0, main="Alpha=0.0")
plot(log(cv1$lambda),cv1$cvm,pch=19,col="red",
     xlab="log(Lambda)",ylab=cv1$name,main="alpha=1.0")
points(log(cv.5$lambda),cv.5$cvm,pch=19,col="grey")
points(log(cv0$lambda),cv0$cvm,pch=19,col="blue")
legend("topleft",legend=c("alpha=1","alpha=.5","alpha=0"),
       pch=19, col=c("red","grey","blue"))

#예측
predict(ad_cvfit, s="lambda.1se", newx=x[1:5,], type="response")

##모형 평가
y_obs = ifelse(validation$wage==">50K",1,0) #실제 관측값
yhat_glmnet = predict(ad_cvfit, s="lambda.1se",newx=xx[validate_idx,],type="response") #예측값
yhat_glmnet = yhat_glmnet[,1] #n*1 matrix로 바꾸기
#이항편차
binomial_deviance(y_obs,yhat_glmnet)
#ROC곡선 및 AUC
pred_glmnet = prediction(yhat_glmnet,y_obs)
perf_glmnet = performance(pred_glmnet,measure="tpr",x.measure="fpr")
plot(perf_lm,col="black",main="ROC Curve")
plot(perf_glmnet, col='blue',add=TRUE)
abline(0,1)
legend('bottomright',inset=.1,
       legend=c("GLM","glmnet"),col=c('black','blue'),
       lty=1,lwd=2)
performance(pred_glmnet,"auc")@y.values[[1]]


###9.2 나무 모형
cvr_tr = rpart(wage~., data=training)
cvr_tr

printcp(cvr_tr) #cp최대지점
summary(cvr_tr)

opar = par(mfrow=c(1,1), xpd=NA)
plot(cvr_tr)
text(cvr_tr, use.n=TRUE)
par(opar)

##나무 모형 평가
yhat_tr = predict(cvr_tr, newdata=validation)
yhat_tr = yhat_tr[,">50K"]
#이항편차
binomial_deviance(y_obs,yhat_tr)
#ROC곡선 및 AUC
pred_tr = prediction(yhat_tr,y_obs)
perf_tr = performance(pred_tr,measure="tpr",x.measure="fpr")
plot(perf_lm, col='black',main="ROC Curve")
plot(perf_tr, col='blue',add=TRUE)
abline(0,1)
legend('bottomright', inset=.1,
       legend= c("GLM","Tree"), col=c('black','blue'),
       lty=1, lwd=2)
performance(pred_tr,"auc")@y.values[[1]]


###9.3 랜덤 포레스트
set.seed(1234)
ad_rf = randomForest(wage~., training)
ad_rf
plot(ad_rf)

#변수중요도
tmp = importance(ad_rf)
head(round(tmp[order(-tmp[,1]),1,drop=FALSE], 2), n=10)
varImpPlot(ad_rf)

#예측
predict(ad_rf, newdata=adult[1:5,]) #type="response" 예측결과 도출
predict(ad_rf, newdata=adult[1:5,], type="prob") #클래스확률행렬 출력

#모형 평가
yhat_rf = predict(ad_rf, newdata=validation, type="prob")[,'>50K']
binomial_deviance(y_obs,yhat_rf)
pred_rf = prediction(yhat_rf,y_obs)
perf_rf = performance(pred_rf,measure="tpr",x.measure="fpr")
plot(perf_lm, col='black',main="ROC Curve")
plot(perf_tr, col='blue',add=TRUE)
plot(perf_rf, col='red', add=TRUE)
abline(0,1)
legend('bottomright', inset=.1,
       legend= c("GLM","Tree","RF"), col=c('black','blue','red'),
       lty=1, lwd=2)
performance(pred_tr,"auc")@y.values[[1]]

#예측 확률값 자체의 비교
p1 = ggplot(data.frame(yhat_glmnet,yhat_rf), aes(yhat_glmnet,yhat_rf)) +
  geom_point(alpha=.5) + geom_abline() + geom_smooth()
p2 = ggplot(reshape2::melt(data.frame(yhat_glmnet,yhat_rf)), aes(value,fill=variable)) +
  geom_density(alpha=.5)
grid.arrange(p1,p2,ncol=2)


###9.4 부스팅
set.seed(1234)
adult_gbm = mutate(training, wage=ifelse(wage == ">50K",1,0))
ad_gbm = gbm(wage~., data=adult_gbm, distribution='bernoulli',
             n.trees=5000, cv.folds=3, verbose=TRUE)
(best_iter = gbm.perf(ad_gbm, method="cv"))

ad_gbm2 = gbm.more(ad_gbm, n.new.trees=10000)
(best_iter = gbm.perf(ad_gbm2, method="cv"))

#예측
predict(ad_gbm, n.trees=best_iter, newdata=adult_gbm[1:5,],type="response")

#모형 평가
yhat_gbm = predict(ad_gbm, n.trees=best_iter, newdata=validation, type="response")
binomial_deviance(y_obs,yhat_gbm)
pred_gbm = prediction(yhat_gbm,y_obs)
perf_gbm = performance(pred_gbm,measure="tpr",x.measure="fpr")
plot(perf_lm, col='black',main="ROC Curve")
plot(perf_glmnet, col='blue',add=TRUE)
plot(perf_rf, col='red', add=TRUE)
plot(perf_gbm, col='cyan', add=TRUE)
abline(0,1)
legend('bottomright', inset=.1,
       legend= c("GLM","glmnet","RF","GBM"), col=c('black','blue','red','cyan'),
       lty=1, lwd=2)
performance(pred_tr,"auc")@y.values[[1]]


###9.5 모형 비교, 최종 모형 선택, 일반화 능력 평가
#각 방법의 예측확률값의 관계와 상관계수 시각화
#상관계수 구하는 함수
panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

pairs(data.frame(y_obs=y_obs, yhat_lm=yhat_lm,
                 yhat_glmnet=c(yhat_glmnet),
                 yhat_rf=yhat_rf, yhat_gbm=yhat_gbm),
      lower.panel=function(x,y){points(x,y); abline(0,1, col='red')},
      upper.panel=panel.cor)

#테스트 세트 이용
y_obs_test = ifelse(test$wage==">50K",1,0)
yhat_gbm_test = predict(ad_gbm, n.trees=best_iter, newdata=test, type='response')
binomial_deviance(y_obs_test, yhat_gbm_test)
pred_gbm_test = prediction(yhat_gbm_test, y_obs_test)
performance(pred_gbm_test,"auc")@y.values[[1]]

