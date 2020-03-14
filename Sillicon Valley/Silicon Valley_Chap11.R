#####Chap11. 빅데이터 분류분석 IV: 스팸 메일 예측

###11.1 스팸 메일 데이터
###11.2 환경 준비와 기초 분석
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

data = tbl_df(read.table("C:\\Users\\eunju\\Desktop\\GitHub\\stat_r_practice\\Sillicon Valley\\spambase.data",
                         strip.white=TRUE, sep=",", header=FALSE))
names(data) <- c('word_freq_make', 'word_freq_address', 'word_freq_all', 'word_freq_3d', 'word_freq_our',
                 'word_freq_over', 'word_freq_remove', 'word_freq_internet', 'word_freq_order', 'word_freq_mail',
                 'word_freq_receive', 'word_freq_will', 'word_freq_people', 'word_freq_report', 'word_freq_addresses',
                 'word_freq_free', 'word_freq_business', 'word_freq_email', 'word_freq_you', 'word_freq_credit',
                 'word_freq_your', 'word_freq_font', 'word_freq_000', 'word_freq_money', 'word_freq_hp',
                 'word_freq_hpl', 'word_freq_george', 'word_freq_650', 'word_freq_lab', 'word_freq_labs',
                 'word_freq_telnet', 'word_freq_857', 'word_freq_data', 'word_freq_415', 'word_freq_85',
                 'word_freq_technology', 'word_freq_1999', 'word_freq_parts', 'word_freq_pm', 'word_freq_direct',
                 'word_freq_cs', 'word_freq_meeting', 'word_freq_original', 'word_freq_project', 'word_freq_re',
                 'word_freq_edu', 'word_freq_table', 'word_freq_conference', 'char_freq_;', 'char_freq_(',
                 'char_freq_[', 'char_freq_!', 'char_freq_$', 'char_freq_#', 'capital_run_length_average',
                 'capital_run_length_longest', 'capital_run_length_total',
                 'class')
data$class = factor(data$class)

glimpse(data)
summary(data)


###11.3 데이터의 시각화
pairs(sample_n(dplyr::select(data,1:10,58),min(1000,nrow(data))),
      upper.panel = panel.cor,
      lower.panel = function(x,y){points(x,y); abline(0,1,col="red")})

pairs(sample_n(dplyr::select(data,48:57,58),min(1000,nrow(data))),
      upper.panel = panel.cor,
      lower.panel = function(x,y){points(x,y); abline(0,1,col="red")})

#상관계수가 큰 순서대로 설명변수 나열
tmp = as.data.frame(cor(data[,-58], as.numeric(data$class)))
tmp = rename(tmp, cor=V1)
tmp$var = rownames(tmp)
ggplot(tmp, aes(reorder(var,cor),cor)) +
  geom_point() + coord_flip()

p1 = ggplot(data,aes(class)) +geom_bar()
p2 = ggplot(data,aes(class,`char_freq_$`)) +geom_boxplot(col="gray") +
  geom_jitter(alpha=.5) + scale_y_sqrt()
p3 = ggplot(data,aes(`char_freq_$`, group=class, fill=class)) +
  geom_density(alpha=.5) + scale_x_sqrt() + scale_y_sqrt()
p4 = ggplot(data,aes(class,capital_run_length_longest)) +
  geom_boxplot(col="gray") +geom_jitter(alpha=.5) + scale_y_log10()
grid.arrange(p1,p2,p3,p4,ncol=2)


###11.4 훈련, 검증, 테스트세트의 구분
#변수명 특수문자 처리
old_names = names(data)
new_names = make.names(names(data), unique=TRUE)
cbind(old_names, new_names)[old_names!=new_names,]
names(data) = new_names

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


###11.5 로지스틱 회귀분석
data_lm = glm(class~., data=training, family=binomial)
summary(data_lm)

y_obs = as.numeric(as.character(validation$class))
yhat_lm = predict(data_lm, newdata=validation, type="response")
pred_lm = prediction(yhat_lm, y_obs)
performance(pred_lm, "auc")@y.values[[1]]
binomial_deviance(y_obs, yhat_lm)


###11.6 라쏘 모형 적합
xx = model.matrix(class~.-1, data)
x = xx[training_idx,]
y = as.numeric(as.character(training$class))

data_cvfit = cv.glmnet(x, y, family="binomial")
plot(data_cvfit)
coef(data_cvfit, s="lambda.min")
coef(data_cvfit, s="lambda.1se")

yhat_glmnet = predict(data_cvfit, s='lambda.min',
                      newx=xx[validation_idx,], type='response')
yhat_glmnet = yhat_glmnet[,1]
pred_glmnet = prediction(yhat_glmnet, y_obs)
performance(pred_glmnet, "auc")@y.values[[1]]
binomial_deviance(y_obs, yhat_glmnet)


###11.7 나무 모형
data_tr = rpart(class~., data=training)
data_tr

printcp(data_tr)
summary(data_tr)

opar = par(mfrow=c(1,1), xpd=NA)
plot(data_tr)
text(data_tr, use.n=TRUE)
par(opar)

yhat_tr = predict(data_tr, newdata=validation)
yhat_tr = yhat_tr[,"1"]
pred_tr = prediction(yhat_tr, y_obs)
performance(pred_tr, "auc")@y.values[[1]]
binomial_deviance(y_obs, yhat_tr)


###11.8 랜덤 포레스트
set.seed(1234)
data_rf = randomForest(class~., data=training)
data_rf

opar=par(mfrow=c(1,2))
plot(data_rf)
varImpPlot(data_rf)
par(opar)

yhat_rf = predict(data_rf, newdata=validation, type='prob')[,'1']
pred_rf = prediction(yhat_rf, y_obs)
performance(pred_rf, "auc")@y.values[[1]]
binomial_deviance(y_obs, yhat_rf)


###11.9 부스팅
data_for_gbm = mutate(training, class=as.numeric(as.character(class)))
data_gbm = gbm(class~., data = data_for_gbm,
               n.trees=5000, cv.folds=3, verbose=TRUE)
(best_iter = gbm.perf(data_gbm, method="cv"))

yhat_gbm = predict(data_gbm, n.trees=best_iter,
                   newdata=validation, type="response")
pred_gbm = prediction(yhat_gbm, y_obs)
performance(pred_gbm, "auc")@y.values[[1]]
binomial_deviance(y_obs, yhat_gbm)


###11.10 최종 모형 선택과 테스트세트 오차 계산
data.frame(method=c('lm', 'glmnet', 'rf', 'gbm'),
           auc = c(performance(pred_lm, "auc")@y.values[[1]],
                   performance(pred_glmnet, "auc")@y.values[[1]],
                   performance(pred_rf, "auc")@y.values[[1]],
                   performance(pred_gbm, "auc")@y.values[[1]]),
           bin_dev = c(binomial_deviance(y_obs, yhat_lm),
                       binomial_deviance(y_obs, yhat_glmnet),
                       binomial_deviance(y_obs, yhat_rf),
                       binomial_deviance(y_obs, yhat_gbm)))

perf_lm = performance(pred_lm, measure="tpr", x.measure="fpr")
perf_glmnet = performance(pred_glmnet, measure="tpr", x.measure="fpr")
perf_rf = performance(pred_rf, measure="tpr", x.measure="fpr")
perf_gbm = performance(pred_gbm, measure="tpr", x.measure="fpr")

plot(perf_lm, col="black", main="ROC Curve")
plot(perf_glmnet, col="blue", add=TRUE)
plot(perf_rf, col="red", add=TRUE)
plot(perf_gbm, col="cyan", add=TRUE)
abline(0,1)
legend('bottomright', inset=.1,
       legend = c('GLM','glmnet','RF','GBM'),
       col = c('black','blue','red','cyan'), lty=1, lwd=2)

#테스트세트
y_obs_test = as.numeric(as.character(test$class))
yhat_rf_test = predict(data_rf, newdata=test, type="prob")[,"1"]
pred_rf_test = prediction(yhat_rf_test, y_obs_test)
performance(pred_rf_test, "auc")@y.values[[1]]
binomial_deviance(y_obs_test, yhat_rf_test)

pairs(data.frame(y_obs=y_obs,
                 yhat_lm=yhat_lm,
                 yhat_glmnet=c(yhat_glmnet),
                 yhat_rf=yhat_rf,
                 yhat_gbm=yhat_gbm),
      lower.panel=function(x,y){points(x,y);abline(0,1,col='red')},
      upper.panel=panel.cor)
