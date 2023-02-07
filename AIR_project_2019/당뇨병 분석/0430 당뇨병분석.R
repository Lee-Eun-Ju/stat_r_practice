#######################당뇨병 분석222########################

install.packages("dplyr")
install.packages("ggplot2")
install.packages("car")
install.packages("randomForest")
library(dplyr)
library(ggplot2)
library(car)
library(randomForest)
library(ROCR)

data <- read.csv("C:\\Users\\이은주\\Desktop\\AIR 동아리\\AIR0409\\diabetes.csv")
glimpse(data)
summary(data)

#자료가 0인 값이 있는 데이터 제거
data <- data[-which(data$Glucose==0),]
data <- data[-which(data$BloodPressure==0),]
data <- data[-which(data$SkinThickness==0),]
data <- data[-which(data$BMI==0),]
data <- data[-which(data$Insulin==0),]

#요인형으로 자료 변환
data$Outcome <- as.factor(data$Outcome)
summary(data)

#반응변수의 분포 확인 -> '0':'1' = 2:1
table(data$Outcome)
 
#sampling
set.seed(2019)
n <- nrow(data)
idx <- 1:n
training_idx <- sample(idx, n*.6)
idx <- setdiff(idx, training_idx) 
validate_idx <- sample(idx, n*.2)
test_idx <- setdiff(idx, validate_idx)
length(training_idx)
length(validate_idx)
length(test_idx)
training <- data[training_idx,]
validation <- data[validate_idx,]
test <- data[test_idx,]


##########################Analysis###########################

#Random Forest
da_rf <- randomForest(Outcome~., training, importance=T)
da_rf
plot(da_rf)

y_hat_rf <- predict(da_rf, newdata=validation, type='prob')[,'1']
y_obs <- validation$Outcome

pred_rf <- prediction(y_hat_rf, y_obs)
perf_rf <- performance(pred_rf, measure='tpr', x.measure='fpr')
plot(perf_rf, col='black',main='ROC Curve')
abline(0,1)
legend('bottomright',
       legend='RF', col='black', lty=1, lwd=2)
performance(pred_rf, "auc")@y.values[[1]] #auc 확인

tmp <- importance(da_rf)
varImpPlot(da_rf)
#bloodpressure과 skinthickness의 변수가 accuracy와 gini가 낮으므로 제거한다.

#다시 랜덤포레스트
da_rf <- randomForest(Outcome~.-BloodPressure-SkinThickness, training, importance=T)
da_rf
plot(da_rf)

y_hat_rf <- predict(da_rf, newdata=validation, type='prob')[,'1']
y_obs <- validation$Outcome

pred_rf <- prediction(y_hat_rf, y_obs)
perf_rf <- performance(pred_rf, measure='tpr', x.measure='fpr')
plot(perf_rf, col='black',main='ROC Curve')
abline(0,1)
legend('bottomright',
       legend='RF', col='black', lty=1, lwd=2)
performance(pred_rf, "auc")@y.values[[1]]

#auc 올리기 위해서 이상치 제거
data <- read.csv("C:\\Users\\이은주\\Desktop\\AIR 동아리\\AIR0409\\diabetes.csv")

data <- data[-which(data$Glucose==0),]
data <- data[-which(data$BloodPressure==0),]
data <- data[-which(data$SkinThickness==0),]
data <- data[-which(data$BMI==0),]
data <- data[-which(data$Insulin==0),]
data$Outcome <- as.factor(data$Outcome)

data=filter(data, Pregnancies!=17) 
data=filter(data, Insulin<=400)
data=filter(data, Age!=81)
data=filter(data, BMI<=50)
data=filter(data, DiabetesPedigreeFunction<=1.5)


#sampling
set.seed(2019)
n <- nrow(data)
idx <- 1:n
training_idx <- sample(idx, n*.6)
idx <- setdiff(idx, training_idx) 
validate_idx <- sample(idx, n*.2)
test_idx <- setdiff(idx, validate_idx)
length(training_idx)
length(validate_idx)
length(test_idx)
training <- data[training_idx,]
validation <- data[validate_idx,]
test <- data[test_idx,]




