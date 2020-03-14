#####Chap10. 빅데이터 분류분석 III: 암 예측

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

binomial_deviance = function(y_obs, yhat){
  epsilon = 0.0001
  yhat = ifelse(yhat < epsilon, epsilon, yhat)
  yhat = ifelse(yhat > 1-epsilon, 1-epsilon, yhat)
  a = ifelse(y_obs==0, 0, y_obs*log(y_obs/yhat))
  b = ifelse(y_obs==1, 0, (1-y_obs)*log((1-y_obs)/(1-yhat)))
  return(2*sum(a+b))
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

###10.1 위스콘신 유방암 데이터
###10.2 환경 준비와 기초 분석
data = tbl_df(read.table("C:\\Users\\eunju\\Desktop\\GitHub\\stat_r_practice\\Sillicon Valley\\wdbc.data",
                  sep=",",header=FALSE,strip.white=TRUE))

feature_names = c('radius','texture','perimeter','area','smoothness',
                  'compactness','concavity','conave_points','symmetry','fractal_dim')

names(data) = c('id','class',paste0('mean_',feature_names),
                paste0('se_',feature_names),
                paste0('worst_',feature_names))
glimpse(data)
summary(data)

#id 변수제거
data = dplyr::select(data,-id)
#class 변수 factor화(B이면 0 M이면 1)
data$class = factor(ifelse(data$class=="B",0,1))


###10.3 데이터의 시각화
pairs(sample_n(dplyr::select(data,class,starts_with('mean_')), min(1000,nrow(data))),
      lower.panel = function(x,y){points(x,y);abline(0,1,col="red")},
      upper.panel = panel.cor)
pairs(sample_n(dplyr::select(data,class,starts_with('se_')), min(1000,nrow(data))),
      lower.panel = function(x,y){points(x,y);abline(0,1,col="red")},
      upper.panel = panel.cor)
pairs(sample_n(dplyr::select(data,class,starts_with('worst_')), min(1000,nrow(data))),
      lower.panel = function(x,y){points(x,y);abline(0,1,col="red")},
      upper.panel = panel.cor)

p1 = ggplot(data,aes(class)) + geom_bar()
p2 = ggplot(data,aes(class, mean_conave_points)) + 
  geom_boxplot(alpha=0.5) + geom_jitter(col='gray')
p3 = ggplot(data,aes(class, mean_radius)) +
  geom_boxplot(alpha=0.5) + geom_jitter(col='gray')
p4 = ggplot(data,aes(mean_conave_points, mean_radius)) +
  geom_smooth() + geom_jitter(col='gray')
grid.arrange(p1,p2,p3,p4,ncol=2)
  

###10.4 훈련, 검증, 테스트세트의 구분
set.seed(1234)
n = nrow(data)
idx = 1:n
training_idx = sample(idx,n*0.6)
idx = setdiff(idx,training_idx)
validation_idx = sample(idx,n*0.2)
test_idx = setdiff(idx,validation_idx)
training = data[training_idx,]
validation = data[validation_idx,]
test = data[test_idx,]


###10.5 로지스틱 회귀분석(glm)
data_lm_full = glm(class~., data=training, family=binomial)
summary(data_lm_full)

predict(data_lm_full, newdata=data[1:5,], type='response')

#모형평가
y_obs = as.numeric(as.character(validation$class))
yhat_lm = predict(data_lm_full, newdata=validation, type="response")
pred_lm = prediction(yhat_lm,y_obs)
performance(pred_lm, "auc")@y.values[[1]]
binomial_deviance(y_obs,yhat_lm)


###10.6 라쏘 모형 적합(glmnet)
#직접 모형행렬 생성
xx = model.matrix(class~.-1,data)
x = xx[training_idx,]
y = as.numeric(as.factor(training$class))

data_cvfit = cv.glmnet(x,y,family='binomial')
plot(data_cvfit)

coef(data_cvfit,"lambda.min") #예측
coef(data_cvfit,"lambda.1se") #해석

predict(data_cvfit, s="lambda.min", newx=x[1:5,], type="response")

#모형 평가
yhat_glmnet = predict(data_cvfit, s="lambda.min", newx=xx[validaion_idx,], type="response")
yhat_glmnet = yhat_glmnet[,1]
pred_glmnet = prediction(yhat_glmnet,y_obs)
performance(pred_glmnet, "auc")@y.values[[1]]
binomial_deviance(y_obs,yhat_glmnet)


###10.7 나무 모형(rpart)
data_tr = rpart(class~., data=training)
data_tr

printcp(data_tr) #cp최대지점(xerror최소지점) 찾
summary(data_tr)

opar = par(mfrow=c(1,1), xpd=NA)
plot(data_tr)
text(data_tr,use.n=TRUE)
par(opar)

#모형평가
yhat_tr = predict(data_tr, newdata=validation)
yhat_tr = yhat_tr[,"1"]
pred_tr = prediction(yhat_tr,y_obs)
performance(pred_tr, "auc")@y.values[[1]]
binomial_deviance(y_obs,yhat_tr)


###10.8 랜덤 포레스트(randomForest)
set.seed(1234)
data_rf = randomForest(class~.,data=training)
data_rf

opar = par(mfrow=c(1,2))
plot(data_rf)
varImpPlot(data_rf)
par(opar)

#모형평가
yhat_rf = predict(data_rf,newdata=validation,type='prob')[,'1']
pred_rf = prediction(yhat_rf,y_obs)
performance(pred_rf, "auc")@y.values[[1]]
binomial_deviance(y_obs,yhat_rf)


###10.9 부스팅(gbm)
set.seed(1234)
data_for_gbm = mutate(training, class=as.numeric(as.character(class)))
data_gbm = gbm(class~., data=data_for_gbm, distribution='bernoulli',
               n.trees=10000, cv.folds=3, verbose=TRUE)
(best_iter = gbm.perf(data_gbm,method="cv"))

#모형평가
yhat_gbm = predict(data_gbm, n.trees=best_iter, newdata=validation, type='response')
pred_gbm = prediction(yhat_gbm,y_obs)
performance(pred_gbm,"auc")@y.values[[1]]
binomial_deviance(y_obs,yhat_gbm)


###10.10 최종 모형 선택과 테스트세트 오차 계산
data.frame(method=c("lm","glmnet","rf",'gbm'),
           auc=c(performance(pred_lm,"auc")@y.values[[1]],
                 performance(pred_glmnet,"auc")@y.values[[1]],
                 performance(pred_rf,"auc")@y.values[[1]],
                 performance(pred_gbm,"auc")@y.values[[1]]),
           bin_dev=c(binomial_deviance(y_obs,yhat_lm),
                     binomial_deviance(y_obs,yhat_glmnet),
                     binomial_deviance(y_obs,yhat_rf),
                     binomial_deviance(y_obs,yhat_gbm)))

perf_lm = performance(pred_lm, measure="tpr", x.measure="fpr")
perf_glmnet = performance(pred_glmnet, measure="tpr", x.measure="fpr")
perf_rf = performance(pred_rf, measure="tpr", x.measure="fpr")
perf_gbm = performance(pred_gbm, measure="tpr", x.measure="fpr")

plot(perf_lm, col='black', main="ROC Curve")
plot(perf_glmnet, add=TRUE, col='blue')
plot(perf_rf, add=TRUE, col='red')
plot(perf_gbm, add=TRUE, col='cyan')
abline(0,1)
legend('bottomright',inset=.1,
       legend=c("lm","glmnet","rf","gbm"),
       col=c('black','blue','red','cyan'),lty=1,lwd=2)

#테스트세트
y_obs_test = as.numeric(as.character(test$class))
yhat_gbm_test = predict(data_gbm, n.trees=best_iter, newdata=test, type="response")
pred_gbm_test = prediction(yhat_gbm_test, y_obs_test)
performance(pred_gbm_test, "auc")@y.values[[1]]
binomial_deviance(y_obs_test, yhat_gbm_test)

#예측의 시각화
pairs(data.frame(y_obs=y_obs,
                 yhat_lm=yhat_lm,
                 yhat_glmnet=yhat_glmnet,
                 yhat_rf=yhat_rf,
                 yhat_gbm=yhat_gbm),
      lower.panel = function(x,y){points(x,y);abline(0,1,col='red')},
      upper.panel = panel.cor)
