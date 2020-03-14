#AIR 과제3
install.packages("C50")
install.packages("gmodels")
install.packages("randomForest")
library(C50)
library(gmodels)	#CrossTable()명령어를 실행하기 위한 패키지
library(randomForest)
library(ROCR)

data <- read.csv("C:\\Users\\이은주\\Desktop\\AIR 동아리\\0430과제\\credit.csv")
str(data) #전반적인 자료를 나타낸다.

data$default <- as.factor(data$default) #default자료를 factor로 바꾼다.
str(data$default)

data <- data[,-c(20)] #랜덤포레스트하여 가장 낮은 변수 제거(과제1에서 작업)

##랜덤하게 있는 객체들이 아니기때문에 난수로 추출
set.seed(2018)
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

data_options <- C5.0Control(winnow=T, noGlobalPruning=F, CF=0.5) 
data_model <- C5.0(training[-17], training$default, control = data_options, trials=5)
summary(data_model)	

#모델 vs validation and roc커브,auc확인
y_hat_rf <- predict(data_model, newdata=validation, type='prob')[,'2']
y_obs <- validation$default

pred_rf <- prediction(y_hat_rf, y_obs)
perf_rf <- performance(pred_rf, measure='tpr', x.measure='fpr')
plot(perf_rf, col='black',main='ROC Curve')
abline(0,1)
legend('bottomright',
       legend='RF', col='black', lty=1, lwd=2)
performance(pred_rf, "auc")@y.values[[1]]