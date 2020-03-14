#####Chap 8. 빅데이터 분류분석 I: 기본 개념과 로지스틱 모형


###8.1 분류분석이란?
binomial_deviance = function(y_obs, yhat){
  epsilon = 0.0001
  yhat = ifelse(yhat < epsilon, epsilon, yhat)
  yhat = ifelse(yhat > 1-epsilon, 1-epsilon, yhat)
  a = ifelse(y_obs==0, 0, y_obs*log(y_obs/yhat))
  b = ifelse(y_obs==1, 0, (1-y_obs)*log((1-y_obs)/(1-yhat)))
  return(2*sum(a+b))
}


###8.2 환경 준비
library(dplyr)
library(ggplot2)
library(ISLR)
library(MASS)
library(glmnet)
library(randomForest)
library(glm)
library(rpart)
library(boot)


###8.3 분류분석 예제: 중산층 여부 예측하기
adult = read.csv("C:\\Users\\이은주\\Desktop\\GitHub\\stat_r_practice\\Sillicon Valley\\adult.data", 
                 header=FALSE, strip.white=TRUE)
names(adult) = c('age','workclass','fnlwgt','education',
                 'education_num','marital_status','occupation',
                 'relationship','race','sex',
                 'capital_gain','capital_loss',
                 'hours_per_week','native_country','wage')
glimpse(adult)
summary(adult)

#범주형 설명변수에 대한 복잡도
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


###8.4 훈련, 검증, 테스트세트의 구분
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


###8.5 시각화
#age와 wage -> 비선형 관계임을 알 수 있다.
ggplot(training, aes(age, fill=wage)) + geom_density(alpha=.5)

#race,age와 wage
ggplot(filter(adult, race %in% c('Black','White')), aes(age,fill=wage)) +
  geom_density(alpha=.5) + ylim(0,0.1) + facet_grid(race~sex, scales='free_y')

#education_num과 wage -> 교육수준이 높을수록 임금이 높음
ggplot(training, aes(education_num, fill=wage)) + geom_bar()


###8.6 로지스틱 회귀분석
ad_glm_full = glm(wage~., data=training, family=binomial)
summary(ad_glm_full)

#오류난 변수와 다른 설명변수간의 상관관계를 알 수 있음
alias(ad_glm_full) 

#적합된 모형의 예측값 얻기
predict(ad_glm_full, newdata=adult[1:5], type='response')

#에러확률 - 시각화
y_obs = ifelse(validation$wage == ">50K", 1, 0)
yhat_lm = predict(ad_glm_full, newdata=validation, type='response')

library(gridExtra)
p1 = ggplot(data.frame(y_obs, yhat_lm), aes(y_obs, yhat_lm, group=y_obs, fill=factor(y_obs))) +
  geom_boxplot()
p2 = ggplot(data.frame(y_obs, yhat_lm), aes(yhat_lm, fill=factor(y_obs))) +
  geom_density(alpha=.5)
grid.arrange(p1,p2,ncol=2)

#에러확률 - 이항편차(예측의 정확도를 나타냄)
binomial_deviance(y_obs, yhat_lm)

#에러확률 - ROC커브 및 auc
library(ROCR)
pred_lm = prediction(yhat_lm, y_obs)
perf_lm = performance(pred_lm, measure="tpr", x.measure="fpr")
plot(perf_lm, col='black', main="ROC Curve for GLM")
abline(0,1)
performance(pred_lm, "auc")@y.values[[1]]
