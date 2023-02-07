#####Chap 8. ������ �з��м� I: �⺻ ����� ������ƽ ����


###8.1 �з��м��̶�?
binomial_deviance = function(y_obs, yhat){
  epsilon = 0.0001
  yhat = ifelse(yhat < epsilon, epsilon, yhat)
  yhat = ifelse(yhat > 1-epsilon, 1-epsilon, yhat)
  a = ifelse(y_obs==0, 0, y_obs*log(y_obs/yhat))
  b = ifelse(y_obs==1, 0, (1-y_obs)*log((1-y_obs)/(1-yhat)))
  return(2*sum(a+b))
}


###8.2 ȯ�� �غ�
library(dplyr)
library(ggplot2)
library(ISLR)
library(MASS)
library(glmnet)
library(randomForest)
library(glm)
library(rpart)
library(boot)


###8.3 �з��м� ����: �߻��� ���� �����ϱ�
adult = read.csv("C:\\Users\\������\\Desktop\\GitHub\\stat_r_practice\\Sillicon Valley\\adult.data", 
                 header=FALSE, strip.white=TRUE)
names(adult) = c('age','workclass','fnlwgt','education',
                 'education_num','marital_status','occupation',
                 'relationship','race','sex',
                 'capital_gain','capital_loss',
                 'hours_per_week','native_country','wage')
glimpse(adult)
summary(adult)

#������ ���������� ���� ���⵵
levels(adult$race); adult$race[1:5]
levels(adult$sex); adult$sex[1:5]

x = model.matrix(~ race+sex+age, adult)
glimpse(x)
colnames(x)

x_orig = dplyr :: select(adult,sex,race,age)
View(x_orig)

x_mod = model.matrix(~ sex+race+age, adult)
View(x_mod)

x = model.matrix( ~.-wage, adult)
dim(x)


###8.4 �Ʒ�, ����, �׽�Ʈ��Ʈ�� ����
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


###8.5 �ð�ȭ
#age�� wage -> ���� �������� �� �� �ִ�.
ggplot(training, aes(age, fill=wage)) + geom_density(alpha=.5)

#race,age�� wage
ggplot(filter(adult, race %in% c('Black','White')), aes(age,fill=wage)) +
  geom_density(alpha=.5) + ylim(0,0.1) + facet_grid(race~sex, scales='free_y')

#education_num�� wage -> ���������� �������� �ӱ��� ����
ggplot(training, aes(education_num, fill=wage)) + geom_bar()


###8.6 ������ƽ ȸ�ͺм�
ad_glm_full = glm(wage~., data=training, family=binomial)
summary(ad_glm_full)

#������ ������ �ٸ� ������������ ������踦 �� �� ����
alias(ad_glm_full) 

#���յ� ������ ������ ���
predict(ad_glm_full, newdata=adult[1:5], type='response')

#����Ȯ�� - �ð�ȭ
y_obs = ifelse(validation$wage == ">50K", 1, 0)
yhat_lm = predict(ad_glm_full, newdata=validation, type='response')

library(gridExtra)
p1 = ggplot(data.frame(y_obs, yhat_lm), aes(y_obs, yhat_lm, group=y_obs, fill=factor(y_obs))) +
  geom_boxplot()
p2 = ggplot(data.frame(y_obs, yhat_lm), aes(yhat_lm, fill=factor(y_obs))) +
  geom_density(alpha=.5)
grid.arrange(p1,p2,ncol=2)

#����Ȯ�� - ��������(������ ��Ȯ���� ��Ÿ��)
binomial_deviance(y_obs, yhat_lm)

#����Ȯ�� - ROCĿ�� �� auc
library(ROCR)
pred_lm = prediction(yhat_lm, y_obs)
perf_lm = performance(pred_lm, measure="tpr", x.measure="fpr")
plot(perf_lm, col='black', main="ROC Curve for GLM")
abline(0,1)
performance(pred_lm, "auc")@y.values[[1]]