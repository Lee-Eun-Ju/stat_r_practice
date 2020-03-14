#####Chap14. 빅데이터 회귀분석II: 와인 품질 예측

###14.1 와인 품질 데이터 소개
###14.2 환경 준비와 기초 분석
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

rmse = function(yi, yhat_i){
  sqrt(mean((yi-yhat_i)^2))
}

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

data = tbl_df(read.table("C:\\Users\\eunju\\Desktop\\GitHub\\stat_r_practice\\Sillicon Valley\\winequality-white.csv",
                         strip.white=TRUE, sep=";", header=TRUE))
glimpse(data)
summary(data)

###14.3 데이터의 시각화
pairs(sample_n(data, min(1000, nrow(data))),
      upper.panel = panel.cor,
      lower.panel = function(x,y){points(x,y);abline(0,1,col="red")})

p1 = ggplot(data, aes(quality)) +geom_bar()
p2 = ggplot(data, aes(factor(quality), alcohol)) + geom_boxplot()
p3 = ggplot(data, aes(factor(quality), density)) + geom_boxplot()
p4 = ggplot(data, aes(alcohol, density)) +
  geom_jitter(alpha=.1) +geom_smooth()
grid.arrange(p1,p2,p3,p4, ncol=2)


###14.4 훈련, 검증, 테스트세트의 구분
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

###14.5 선형회귀 모형
data_lm = lm(quality ~., data=training)
summary(data_lm)

data_lm_2 = lm(quality ~.^2, data=training)
summary(data_lm_2)
length(coef(data_lm_2))

data_step = MASS::stepAIC(data_lm,
                          scope = list(upper=~.^2, lower=~1))
data_step
anova(data_step)
summary(data_step)
length(coef(data_step))

#모형평가
y_obs = validation$quality
yhat_lm = predict(data_lm, newdata=validation)
yhat_lm_2 = predict(data_lm_2, newdata=validation)
yhat_step = predict(data_step, newdata=validation)

rmse(y_obs, yhat_lm)
rmse(y_obs, yhat_lm_2)
rmse(y_obs, yhat_step)


###14.6 라쏘 모형 적합
xx= model.matrix(quality~.^2-1, data)
x= xx[training_idx,]
y= training$quality

data_cvfit = cv.glmnet(x,y)
plot(data_cvfit)
coef(data_cvfit, s="lambda.min")
coef(data_cvfit, s="lambda.1se")

#모형평가
yhat_glmnet = predict(data_cvfit, s="lambda.min", newx=xx[validation_idx,])
yhat_glmnet = yhat_glmnet[,1]
rmse(y_obs, yhat_glmnet)


###14.7 나무 모형
data_tr = rpart(quality~., data=training)
data_tr

printcp(data_tr)
summary(data_tr)

opar = par(mfrow=c(1,1), xpd=NA)
plot(data_tr)
text(data_tr, use.n=TRUE)
par(opar)

#모형평가
yhat_tr = predict(data_tr, newdata=validation)
rmse(y_obs,yhat_tr)

###14.8 랜덤 포레스트
set.seed(1234)
data_rf = randomForest(quality~., data=training)
summary(data_rf)

plot(data_rf)
varImpPlot(data_rf)

#모형평가
yhat_rf = predict(data_rf, newdata=validation)
rmse(y_obs, yhat_tr)


###14.9 부스팅
set.seed(1234)
data_gbm = gbm(quality~., data=training,
               n.trees=50000, cv.folds=3, verbose=TRUE)
(best_iter = gbm.perf(data_gbm, method="cv"))

#모형평가
yhat_gbm = predict(data_gbm, n.trees=best_iter, newdata=validation)
rmse(y_obs, yhat_gbm)


###14.10 최종 모형 선택과 테스트세트 오차 계산
data.frame(lm = rmse(y_obs, yhat_step),
           glmnet = rmse(y_obs, yhat_glmnet),
           rf = rmse(y_obs, yhat_rf),
           gbm = rmse(y_obs, yhat_gbm)) %>%
  reshape2::melt(value.name = 'rmse', variable.name = 'method')

#테스트세트
rmse(test$quality, predict(data_rf, newdata=test))

#예측값 시각화
pairs(data.frame(y_obs = y_obs,
                 yhat_lm = yhat_lm,
                 yhat_glmnet = c(yhat_glmnet),
                 yhat_rf = yhat_rf,
                 yhat_gbm = yhat_gbm),
      upper.panel = panel.cor,
      lower.panel = function(x,y){points(x,y); abline(0,1,col="red")})
