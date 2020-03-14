#####Chap13. 빅데이터 회귀분석I: 부동산 가격 예측

###13.1 회귀분석이란?
#RMSE 예측오차값이 작을수록 더 정확한 모혀
rmse = function(yi, yhat_i){
  sqrt(mean((yi-yhat_i)^2))
}

###13.2 회귀분석 예제: 부동산 가격 예측
###13.3 환경 준비와 기초 분석
library(dplyr)
library(ggplot2)
library(MASS)
library(glmnet)
library(randomForest)
library(gbm)
library(rpart)
library(boot)
library(data.table)
library(ROCR)
library(gridExtra)

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

data = tbl_df(read.table("C:\\Users\\eunju\\Desktop\\GitHub\\stat_r_practice\\Sillicon Valley\\housing.data",
                         strip.white=TRUE))
names(data) = c('crin','zn','indus','chas','nox','rm','age',
                'dis','rad','tax','ptratio','b','lstat','medv')
glimpse(data)
summary(data)

#산점도
pairs(sample_n(data, min(1000,nrow(data))),
      lower.panel = function(x,y){points(x,y); abline(0,1,col="red")},
      upper.panel = panel.cor)

###13.4 훈련, 검증, 테스트세트의 구분
set.seed(1234)
n = nrow(data)
idx = 1:n
training_idx = sample(idx, n*.6)
idx = setdiff(idx, training_idx)
validation_idx = sample(idx, n*.2)
test_idx = setdiff(idx, validation_idx)
training = data[training_idx,]
validation = data[validation_idx,]
test = data[test_idx,]

###13.5 선형회귀 모형
#가장 간단한 모형
data_lm = lm(medv ~., data=training)
summary(data_lm)

#이차상호작용을 고려한 모형->지나치게 복잡한 모형(과적합 및 해석의 어려움)
data_lm_2 = lm(medv ~.^2, data=training)
summary(data_lm_2)
length(coef(data_lm_2))

#가장 간단한 모형부터 복잡한 모형까지 탐색함으로써
#가장 중요한 모수 선택 -> MASS::stepAIC()
data_step = stepAIC(data_lm,
                    scope=list(upper=~.^2, lower= ~1))
data_step
anova(data_step) #분산분석에 따른 F-통계량 확인
summary(data_step)
length(coef(data_step))

#모형평가
y_obs = validation$medv
yhat_lm = predict(data_lm, newdata=validation)
yhat_lm_2 = predict(data_lm_2, newdata=validation)
yhat_step = predict(data_step, newdata=validation)

rmse(y_obs,yhat_lm)
rmse(y_obs,yhat_lm_2)
rmse(y_obs,yhat_step)


###13.6 라쏘 모형 적합
xx = model.matrix(medv ~.^2-1, data)
x = xx[training_idx,]
y = training$medv

data_cvfit = cv.glmnet(x,y)
plot(data_cvfit)

coef(data_cvfit, s="lambda.min")
coef(data_cvfit, s="lambda.1se")

#모형평가
yhat_glmnet = predict(data_cvfit, s="lambda.min", newx = xx[validation_idx,])
yhat_glmnet = yhat_glmnet[,1]
rmse(y_obs, yhat_glmnet)


###13.7 나무 모형
data_tr = rpart(medv ~., data=training)
data_tr

printcp(data_tr)
summary(data_tr)

opar = par(mfrow=c(1,1), xpd=NA)
plot(data_tr)
text(data_tr, use.n=TRUE)
par(opar)

#모형평ㄱ
yhat_tr = predict(data_tr, newdata=validation)
rmse(y_obs, yhat_tr)


###13.8 랜덤 포레스트
set.seed(1234)
data_rf = randomForest(medv~., data=training)
data_rf

plot(data_rf)
varImpPlot(data_rf)

#모형평가
yhat_rf = predict(data_rf, newdata=validation)
rmse(y_obs, yhat_rf)


###13.9 부스팅
set.seed(1234)
data_gbm = gbm(medv~., data=training,
               n.trees=50000, cv.folds=3, verbose=TRUE)
(best_iter = gbm.perf(data_gbm, method="cv"))

#모형평가
yhat_gbm = predict(data_gbm, n.trees=best_iter, newdata=validation)
rmse(y_obs, yhat_gbm)


###13.10 최종 모형 선택과 테스트세트 오차 계산
data.frame(lm = rmse(y_obs, yhat_step),
           glmnet = rmse(y_obs, yhat_glmnet),
           rf = rmse(y_obs, yhat_rf),
           gbm = rmse(y_obs, yhat_gbm)) %>%
  reshape2::melt(values.name='rmse', variable.name='method')

#테스트세트
rmse(test$medv, predict(data_rf, newdata=test))

#예측오차의 분포 시각화
boxplot(list(lm = y_obs - yhat_step,
             glmnet = y_obs - yhat_glmnet,
             tr = y_obs - yhat_tr,
             rf = y_obs - yhat_rf,
             gbm = y_obs - yhat_gbm),
        ylab = "Error in Validation Set")
abline(h=0, lty=2, col="blue")

#예측값들의 산점도행렬 및 상관계수
pairs(data.frame(y_obs=y_obs,
                 yhat_step=yhat_step,
                 yhat_glmnet=c(yhat_glmnet),
                 yhat_tr=yhat_tr,
                 yhat_rf=yhat_rf,
                 yhat_gbm=yhat_gbm),
      upper.panel = panel.cor,
      lower.panel = function(x,y){points(x,y); abline(0,1,col="red")})
