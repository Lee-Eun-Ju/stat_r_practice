#############################당뇨병 원인 분석#############################

data <- read.csv("C:\\Users\\이은주\\Desktop\\AIR0409\\diabetes.csv")
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

#변수와 당뇨병과의 관련성 비교
install.packages("dplyr")
install.packages("ggplot2")
library("dplyr")
library("ggplot2")

#임신 횟수 - 당뇨병
data %>%
ggplot(aes(Outcome,Pregnancies)) + geom_boxplot()

#글루코즈 - 당뇨병
data %>%
ggplot(aes(Outcome,Glucose)) + geom_boxplot()

#혈압 - 당뇨병
data %>%
ggplot(aes(Outcome,BloodPressure)) + geom_boxplot()

#피부두께 - 당뇨병
data %>%
ggplot(aes(Outcome,SkinThickness)) + geom_boxplot()

#인슐린 - 당뇨병
data %>%
ggplot(aes(Outcome, Insulin)) + geom_boxplot()

#BMI - 당뇨병
data %>%
ggplot(aes(Outcome, BMI)) + geom_boxplot()

#당뇨병 혈통 기능 - 당뇨병
data %>%
ggplot(aes(Outcome,DiabetesPedigreeFunction)) + geom_boxplot()

#나이 - 당뇨병
data %>%
ggplot(aes(Outcome,Age)) + geom_boxplot()

##########################Analysis###############################
#두 변수 간의 관련성을 알기 위해서 회귀분석을 한다.
#이분형 로지스틱 회귀(독립변수가 연속형, 종속변수가 2개의 범주)
abc <- glm(Outcome~Pregnancies+Glucose+BloodPressure+SkinThickness+Insulin+BMI+DiabetesPedigreeFunction+Age
	,data=data,family='binomial') #family는 종속변수의 분포에 따라 다르다
summary(abc)
#value가 0.05보다 낮을 때 유의하다고 할 수 있고 더 낮을수록, *이 많을 수록 예측력이 높다고 할 수 있다. 

plot(abc)

