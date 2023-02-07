#########################################BlackFriday


library(dplyr) #glimpse를 실행하는 패키지
library(ggplot2)

#데이터를 불러온 후, 데이터를 int형, fct형에 따라 정리해 줌.
#잘못되어 있으면, 수정~!
data <- read.csv("C:\\Users\\이은주\\Desktop\\블랙프라이데이\\BlackFriday.csv")
glimpse(data) #데이터 어떤 형태인지 확인(dplyr 패키지 이용)
summary(data) #데이터 요약해서 보여줌
head(data) #상위 6개의 데이터 미리보기/ 하위6개-> tail
str(data) #glimpse와 비슷한 역할
colnames(data) #변수명 보여줌
dim(data) #객체의 차원 확인 -> 자료의 데이터 갯수와 자료의 변수명 갯수

attach(data) #data를 안붙여도 변수명을 저장하여 데이터 처리하기 쉬움.


#########################################새로운 데이터 만들기


glimpse(data)
max(data$User_ID) #User_ID가 1000001부터 1006040까지 있음을 알 수 있다.

head(unique(data$User_ID)) #unique는 정보없이 유일한 관측치를 선별함
length(unique(data$User_ID)) #그 관측치들의 수는 5891로 사용자가 5891명 있음

newdata<-matrix(NA,6040,8) #newdata에 6040개의 데이터와 8개의 변수가 존재하는 행렬 생성
colnames(newdata)<-c('Gender','Age','Occupation','City_Category','Stay_In_Current_City_Years','Martial_Status','Product_num','Purchase')
#newdata의 변수명 지정
head(newdata)

firstindex<-rep(0,6040)
num<-rep(0,6040)

for( i in 1000001 : 1006040 ) {
  which<-which(data$User_ID==i) #예를 들어 1000001일 때는 which에 1,2,3,4 벡터가 대입 
  i<-substr(i,4,7) #substr은 i에서 7자리 중 4자리만을 남기고 제거
  i<-as.numeric(i) #수치화
  firstindex[i]<-which[1] #중복되는 데이터에서 첫번째 데이터를 firstindex에 대입
  num[i]<-length(which) #중복되는 갯수, 즉 한 사람이 구매한 상품의 갯수 num에 대입
   for( n in 1:6 ) {
   newdata[i,n]<-data[firstindex[i],n+2]
   }
  newdata[i,7]<-num[i] #Product_num
  newdata[i,8]<-sum(data$Purchase[which]) #총 Purchase
  }
head(newdata)


length(which(is.na(newdata[,1]))) #결측치 수
na.index<-which(is.na(newdata[,1])) #결측치가 있는 위치 na.index에 대입
newdata <- newdata[-na.index,] #결측치를 제거한 newdata
sum(!complete.cases(newdata)) #complete.cases는 결측치를 확인하는것으로,
					#!complete.cases는 결측치가 있는 행을 의미
dim(newdata) #사용자가 5891로 알맞게 데이터 생성이 되었음을 알 수 있음.
summary(newdata)
newdata[,1] <- ifelse(newdata[,1]==1, 'F',ifelse(newdata[,1]==2,'M',NA))
newdata[,2] <- ifelse(newdata[,2]==1,'0-17',ifelse(newdata[,2]==7,'55+',ifelse(newdata[,2]==3,'26-35',
                                            ifelse(newdata[,2]==5,'46-50',ifelse(newdata[,2]==6,'51-55',
                                            ifelse(newdata[,2]==4,'36-45',ifelse(newdata[,2]==2,'18-25',NA)))))))

write.csv(newdata, "C:\\Users\\이은주\\Desktop\\블랙프라이데이\\newdata.csv")


newdata <- read.csv("C:\\Users\\이은주\\Desktop\\블랙프라이데이\\newdata.csv")
head(newdata)
str(newdata)
newdata$Occupation <- as.factor(newdata$Occupation)
newdata$City_Category <- as.factor(newdata$City_Category)
newdata$Martial_Status <- as.factor(newdata$Martial_Status)
newdata <- newdata[,-1]

#또는 다른 방식으로 데이터를 불러올 수 있다.
#is.matrix(newdata)
#newdata<-as.data.frame(newdata)
# 변수 형태 지정
#newdata$Stay_In_Current_City_Years <- as.integer(newdata$Stay_In_Current_City_Years)
#newdata$Product_num <- as.integer(newdata$Product_num)
#newdata$Purchase <- as.integer(newdata$Purchase)


############################################EDA

plot(newdata)
glimpse(newdata)
summary(newdata)

######is.na
for ( i in 1:8) {
  print( sum(!complete.cases(newdata[,i]))) # 결측치 없는 것 확인
}         

#table은 그 값에 해당하는 자료의 수를 나타냄

# Gender 
table(newdata$Gender)
boxplot(Purchase~Gender,data=newdata)
##F는 1666, M은 4225

# Age
table(newdata$Age)
##218  1069  2053  1167   531   481   372 

# Occupation
table(newdata$Occupation)
##688 517 256 170 740 111 228 669  17  88 192 128 376 140 294 140 235 491  67  71 273 

# City_Category
table(newdata$City_Category)
##1045 1707 3139

# Stay_In_Current_City_Years
sort(table(newdata$Stay_In_Current_City_Years),decreasing=T)

# Martial_Status
table(newdata$Martial_Status)

# Product_num
sort(table(newdata$Product_num), decreasing=T)[1:50] #내림차순

# Purchase
sort(table(newdata$Purchase),decreasing=T)[1:50]


#성별에 따른 구매 제품 수 차이 -> X
newdata %>%
ggplot(aes(x=Gender, y=Product_num)) + geom_boxplot()
ggplot(aes(x=Gender, y=Purchase)) + geom_boxplot()

#나이에 따라 구매 제품 수 차이 -> 26-35 나이대가 약간의 높은 구매율 
newdata %>%
ggplot(aes(x=Age, y=Product_num)) + geom_boxplot()
ggplot(aes(x=Age, y=Purchase)) + geom_boxplot()

#사는 곳에 따라 구매 제품 수 차이 -> 3번째 지역이 굉장히 낮은 구매율을 보임
newdata %>%
ggplot(aes(x=City_Category, y=Purchase)) + geom_boxplot()
ggplot(aes(x=City_Category, y=Product_num)) + geom_boxplot()

#혼인 여부에 따라 구매 제품 수 차이 -> X
newdata %>%
ggplot(aes(x=Martial_Status, y=Purchase)) + geom_boxplot()
ggplot(aes(x=Martial_Status, y=Product_num)) + geom_boxplot()

#직업에 따라 구매 제품 수 차이 -> 전반적으로 뚜렷한 차이가 없다.
newdata %>%
ggplot(aes(x=Occupation, y=Purchase)) + geom_boxplot()
ggplot(aes(x=Occupation, y=Product_num)) + geom_boxplot()




