library(dplyr)
library(ggplot2)

data <- read.csv("C://Users//이은주//Desktop//AIR 동아리//#Sillicon Valley//Chess game//games.csv", header=TRUE)
glimpse(data)
summary(data)

#TRUE와 True / False와 FALSE
data$rated <- as.character(data$rated)
Ti = which(data$rated=="True")
Fi = which(data$rated=="False")
data[Ti,2] <- "TRUE" 
data[Fi,2] <- "FALSE"
#열의 index를 지정(data[Ti,rated]는 error)
data$rated <- as.factor(data$rated)
summary(data)

#중복 id(동일 데이터) 제거
data <- unique(data)
summary(data)
#-> 제거되지 않은 데이터 : created_at/last_move_at 의 차이
filter(data,id=='079kHDqh')
filter(data,id=='07e0uVvn')
#-> 변수의 가치가 없다고 생각되어 변수 제거
data <- data[,-c(3,4)]
data <- unique(data)
summary(data)
data$id <- as.character(data$id)

#winner=1, loser=0에 대한 데이터 프레임 만들기
glimpse(data)
data$winner <- as.character(data$winner)
data <- data[-which(data$winner=="draw"),] #승부X인 데이터 제거
data$winner <- as.factor(data$winner)
summary(data)

data$victory_status <- as.character(data$victory_status)
data$victory_status <- as.factor(data$victory_status)

#moves->white와 black 구별 / 모든 움직임에 대한 패턴 분석은 의미가 없음.
movector = as.character(data$moves)
movector = strsplit(movector," ")
head(movector)

white_first_moving = vector()
black_first_moving = vector()
for (i in 1:length(movector)){
  white_first_moving[i] = movector[[i]][1] 
  black_first_moving[i] = movector[[i]][2]
}

head(white_first_moving)
head(black_first_moving)

#새로운 데이터프레임
summary(data)
rating_difference = vector()
first_moving = vector()

for (k in 1:length(data$winner)){
  if(data$winner=="white"){
    rating_difference[k] = data[k,8] - data[k,10]
    first_moving[k] = white_first_moving[k] 
  }
  else{
    rating_difference[k] = data[k,10] - data[k,8]
    first_moving[k] = black_first_moving[k] 
  }
}

data0 <- data[,-c(1,7,8,9,10,11)]
data0 <- cbind(data0,rating_difference,first_moving)
summary(data0)
glimpse(data0)

###EDA 
pairs(data0)

#turns
ggplot(data0,aes(winner,turns)) + geom_boxplot()
data0 <- data0[-which(data0$turns==349),] #이상치 제거
summary(data0)

#rating_difference
ggplot(data0,aes(rating_difference)) + geom_freqpoly() #등수->승패에 영향없음

#victory_status
ggplot(data0,aes(x=victory_status,fill=winner)) + geom_bar(stat = 'count')

#opening_eco
distinct(data$opening_eco)
ggplot(data0,aes(x=opening_eco,fill=winner)) + geom_bar(stat = 'count', position = 'fill')

#opening_name
summary(data0$opening_name)
ggplot(data0,aes(opening_name)) + geom_bar()

#opening_ply
ggplot(data0,aes(opening_ply)) + geom_freqpoly()
#6~7이동수를 보인 오프닝에서 우승 경험이 현저히 떨어진다.
#but 이것은 우승자,패배자 동일한 값이므로 우승에 영향을 준다 볼 수 없..?

#first_moving
ggplot(filter(data0,winner=="white"),aes(first_moving)) + geom_bar()
#첫번째 움직임이 e4일 때 승리확률 높다..

ggplot(filter(data0,winner=="black"),aes(first_moving)) + geom_bar()
#첫번째 움직임이 e4일 때 승리확률 높다..
summary(data$moves)
