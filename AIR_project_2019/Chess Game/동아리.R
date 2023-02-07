#install.packages('h2o')
library(h2o) #machine learning packages
#install.packages('tidyverse')
library(tidyverse) #for dplyr, ggplot
#install.packages('data.table')
library(data.table) #for data manipulation and faster data reading
#install.packages('Amelia')
library(Amelia) #for missing values plotting
#install.packages('tidyquant')
library(tidyquant) #for better ggplot theme
#install.packages('grid')
library(grid) #for multiple plots arranagement
#install.packages('gridExtra')
library(gridExtra) #for extra grid function
#install.packages('corrplot')
library(corrplot) #visualize correlation
#install.packages('ggplot2')
library(ggplot2)
#install.packages('randomForest')
library(randomForest)
#install.packages('NbClust')
library(NbClust)
#install.packages('DMwR')
library(DMwR)
#install.packages('ROCR')
library(ROCR)
#install.packages('car')
library(car)
#install.packages('caret')
library(caret)
#install.packages("pscl")
library(pscl)
#install.packages('cluster')
library(cluster)


setwd("C:/Users/jaho9/Desktop/동아리 air/chess")
data <- read.csv("games.csv")
summary(data)
glimpse(data)
dim(data)

### 변수 설명
# id : 게임 코드
# rated : 렝겜인지 아닌지
# created_at : 시작 시간?
# last_move_at : 마지막 말을 움직인 시간?
# turns : 게임이 끝날 때 까지 움직인 말 수
# victory_status : 이긴 상태(무승부/체크메이트/시간초과/항복)
# winner : 우승자(검정말/무승부/흰색말)
# increment_code : 처음 지정되는 분(첫수제외) + 매번 수를 놓은 후에 추가되는 초
# white_id : 흰색말 유저의 id
# white_rating : 흰색말 유저의 랭킹
# black_id : 검정말 유저의 id
# black_rating : 검정말 유저의 랭킹
# moves : 한 게임당 양쪽말이 움직인 경로
# opening_eco : 
# opening_name : 
# opening_ply : 




#TRUE와 True / False와 FALSE
data$rated <- as.character(data$rated)
data[which(data$rated=="True"),2] = "TRUE"
data[which(data$rated=="False"),2] = "FALSE"
data$rated <- as.factor(data$rated)
summary(data)

#id 변수 -> 동일한 id, 모든 변수 또한 다르므로 행 제거
data[data$id=="XRuQPSzH",]
data[data$id=="0gTDO6Av",]
data[data$id=="CvakmVNb",]
data[data$id=="VW8jn694",]

data = data[-which(duplicated(data$id)),] #id 고유한 행만 추출

#created_at과 last_move_at

diff <- vector()
n <- dim(data)[1]
i <- 1
for(i in 1:n){
  diff[i] <- data$last_move_at[i] - data$created_at[i]
  i <- i + 1
}
diff
summary(diff)

data1 <- cbind(data, diff)
data1 <- data1[,-c(3,4)]
summary(data)


# turns
boxplot(data$turns)
which(data$turns==1)
# 턴수가 1개일 경우 전부 시간초과 or 항복이다. 
data[which(data$turns==1),]

data%>%
  ggplot(aes(x=winner, y=victory_status, fill=winner)) + geom_point()


#턴수가 짝수일 때 white가 winner인 경우 전부 항복이다
summary(data[data$turns %% 2 == 0 & data$winner == "white", 6])
#턴수가 홀수일 때 black이 winner인 경우 전부 항복이다.
summary(data[data$turns %% 2 == 1 & data$winner == "black", 6])
# 턴수가 짝수일 때 승리한 경우 승리자는 전부 블랙이다.
summary(data[data$turns %% 2 == 0 & data$victory_status == "mate", 7])
# 턴수가 홀수일 때 승리한 경우 승리자는 전부 화이트다.
summary(data[data$turns %% 2 == 1 & data$victory_status == "mate", 7])


# increment
summary(data)
summary(data$increment_code)
data$increment_code <- as.character(data$increment_code) 

inc <- data$increment_code
inc <- gsub("[:+:]", " ", inc) #특수문자일경우 패턴[: :]을 표시해야함.
inc <- strsplit(inc," ") #" "단위로 끊기
head(inc)

time_start = vector()
for (i in 1:length(inc)){
  time_start[i] <- inc[[i]][1]
}

time_plus = vector()
for (i in 1:length(inc)){
  time_plus[i] <- inc[[i]][2]
}

data <- cbind(data,time_start,time_plus)
data$time_start <- as.integer(data$time_start)
data$time_plus <- as.integer(data$time_plus)
summary(data$time_start)
summary(data$time_plus)
plot(data$time_plus)
# time_plus가 1~32까지 모든 수에 다 존재하기 때문에 pass
data$increment_code <- as.factor(data$increment_code)

# white_rating vs black_rating
summary(data)



# moves -> " "를 기준으로 각 단어로 다 나눈 뒤에 체스판 어느 부위에서 제일 많이
# 끝났는지를 확인할것이다.
is.data.frame(data)
move <- data.frame()
move <- data$moves
summary(move)
head(move) == head(data$moves)
n <- length(move)
each <- data.frame()
move <- as.character(move)
i=1
for(i in 1:n){
  each[i] <- strsplit(move[i], " ")
}
move <- as.data.frame(move)
is.data.frame(each)
move <- cbind(move, each)
dim(move)
dim(each)

strsplit(move[1]," ")


dim(data)
str(data)
lm1 <- glm(winner~., data = data)

mydata = data[,-c(1,8:9,11,13:15)] # 필요없는 column들 없앰

str(mydata)
rf1 <- randomForest(y=factor(mydata$winner),x=mydata[,-6])
importance(rf1)
pred1 = predict(rf1,mydata) ## 모델 rf1을 통한 예측


confusionMatrix(pred1, mydata$winner) ## 예측값과 실제값의 CONFUsion MATRIx

str(diff)
length(diff)
length(diff[-which(diff == 0)])
diff1 <- diff[-which(diff==0)]
data2 = data[-which(diff == 0),-c(1,8,9,3,4,11,13,14,15)]
data3 <- cbind(data2, diff1)
str(data3)
rf2 <- randomForest(y=data3$diff1, x=data3[,-10])
str(mydata)
colnames(data3)
dim(mydata)
mydata$diff1 = 0
colnames(mydata[,-c(2,3)])
fill = predict(rf2, mydata[which(diff==0),-c(2,3)])

df = data[,-c(3,4)]
df <- cbind(df, diff)
str(df)
summary(df)
df[which(diff==0),17] = fill
str(df)
summary(df)
