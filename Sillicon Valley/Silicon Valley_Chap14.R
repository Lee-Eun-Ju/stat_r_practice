#####Chap14. ������ ȸ�ͺм�II: ���� ǰ�� ����

###14.1 ���� ǰ�� ������ �Ұ�
###14.2 ȯ�� �غ�� ���� �м�
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

###14.3 �������� �ð�ȭ
pairs(sample_n(data, min(1000, nrow(data))),
      upper.panel = panel.cor,
      lower.panel = function(x,y){points(x,y);abline(0,1,col="red")})

p1 = ggplot(data, aes(quality)) +geom_bar()
p2 = ggplot(data, aes(factor(quality), alcohol)) + geom_boxplot()
p3 = ggplot(data, aes(factor(quality), density)) + geom_boxplot()
p4 = ggplot(data, aes(alcohol, density)) +
  geom_jitter(alpha=.1) +geom_smooth()
grid.arrange(p1,p2,p3,p4, ncol=2)


###14.4 �Ʒ�, ����, �׽�Ʈ��Ʈ�� ����
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

###14.5 ����ȸ�� ����
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

#������
y_obs = validation$quality
yhat_lm = predict(data_lm, newdata=validation)
yhat_lm_2 = predict(data_lm_2, newdata=validation)
yhat_step = predict(data_step, newdata=validation)

rmse(y_obs, yhat_lm)
rmse(y_obs, yhat_lm_2)
rmse(y_obs, yhat_step)


###14.6 ��� ���� ����
xx= model.matrix(quality~.^2-1, data)
x= xx[training_idx,]
y= training$quality

data_cvfit = cv.glmnet(x,y)
plot(data_cvfit)
coef(data_cvfit, s="lambda.min")
coef(data_cvfit, s="lambda.1se")

#������
yhat_glmnet = predict(data_cvfit, s="lambda.min", newx=xx[validation_idx,])
yhat_glmnet = yhat_glmnet[,1]
rmse(y_obs, yhat_glmnet)


###14.7 ���� ����
data_tr = rpart(quality~., data=training)
data_tr

printcp(data_tr)
summary(data_tr)

opar = par(mfrow=c(1,1), xpd=NA)
plot(data_tr)
text(data_tr, use.n=TRUE)
par(opar)

#������
yhat_tr = predict(data_tr, newdata=validation)
rmse(y_obs,yhat_tr)

###14.8 ���� ������Ʈ
set.seed(1234)
data_rf = randomForest(quality~., data=training)
summary(data_rf)

plot(data_rf)
varImpPlot(data_rf)

#������
yhat_rf = predict(data_rf, newdata=validation)
rmse(y_obs, yhat_tr)


###14.9 �ν���
set.seed(1234)
data_gbm = gbm(quality~., data=training,
               n.trees=50000, cv.folds=3, verbose=TRUE)
(best_iter = gbm.perf(data_gbm, method="cv"))

#������
yhat_gbm = predict(data_gbm, n.trees=best_iter, newdata=validation)
rmse(y_obs, yhat_gbm)


###14.10 ���� ���� ���ð� �׽�Ʈ��Ʈ ���� ���
data.frame(lm = rmse(y_obs, yhat_step),
           glmnet = rmse(y_obs, yhat_glmnet),
           rf = rmse(y_obs, yhat_rf),
           gbm = rmse(y_obs, yhat_gbm)) %>%
  reshape2::melt(value.name = 'rmse', variable.name = 'method')

#�׽�Ʈ��Ʈ
rmse(test$quality, predict(data_rf, newdata=test))

#������ �ð�ȭ
pairs(data.frame(y_obs = y_obs,
                 yhat_lm = yhat_lm,
                 yhat_glmnet = c(yhat_glmnet),
                 yhat_rf = yhat_rf,
                 yhat_gbm = yhat_gbm),
      upper.panel = panel.cor,
      lower.panel = function(x,y){points(x,y); abline(0,1,col="red")})