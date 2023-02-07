###data transformation

##data
library(nycflights13)
library(tidyverse)
nycflights13::flights
?flights
str(flights) #time_hour : dttime형 - year,month,day,dep_time을 가지고 데이터 생성
             #연산 가능

##filter()
filter(flights, month==1, day==1) %>% head(10) #저장X
dec25 = filter(flights, month==1, day==1) #저장
# 양쪽에 ( ) 표시하면 저장하면서 동시에 보여주기

# == 표시
sqrt(2)^2 ==2 #소수점자리가 조금 달라서 같다고 표현하지 않음
near(sqrt(2)^2,2)
x=factor(c("a","b","c"))
x
x=="a" #1개 있음을 알 수 있다
sum(x!= "a") #a가 아닌것 2개
y

#& and , | or


###logical operators
nov_dec = filter(flights, month %in% c(11,12)) #or을 표현할 수 있따
filter(flights, month==11|month==12) %>% head(5)

#!(x&y)  !x|!y
filter(flights, )

##missing vlaues - 어떠케 처리?
#na가 들어가서 연산처리해주면 모두 na가 된다
x= c(1,2,NA)
is.na(x)
sum(is.na(x))

#condition이 true일때만 na를 보여줌
df= tibble(x=c(1,NA,3)) 
filter(df, is.na(x)|x>1)




##my turn 
#1
filter(flights, arr_delay>=120) 
filter(flights, dest=="IAH"|dest=="HOU") #%in% 사용
filter(flights, carrier=="UA"|carrier=="AA"|carrier=="DL")
filter(flights, month==7|month==8|month==9)
filter(flights, between(month,7,9))
filter(flights, arr_delay>=120 & dep_delay<=0) #빨리 출발도 가느

filter(flights, dep_delay>=60 & dep_delay-30>=arr_delay)
filter(flights, dep_delay>=60 & dep_delay-arr_delay>30)

summary(flights$dep_time)
filter(flights, (dep_time>0 & dep_time<600)|dep_time==2400)
filter(flights, dep_time %% 2400 <=600) #2400으로 나누었을 때의 나머지가 600보다 작거나 같으면

#2
sum(is.na(flights$dep_time))
filter(flights, is.na(dep_time)) %>% nrow()
filter(flights, is.na(dep_time)) #아예 출발 및 도착하지 않았다

###arrange()
arrange(flights, desc(dep_delay)) #내림차순
arrange(flights, dep_delay)
arrange(flights, year, month, desc(day)) #먼저 나온 순서대로 정렬

#missing은 맨 밑에 정렬 된다 무조건내림차순이든 오름차순이든

###select() : 내가 원하는 열만 선택 
select(flights, year:day)
select(flights, -(year:day))

#어떠한 말로 시작하는 열 성정
#첫번째 string
#x1,x2,x3 = > "x", 1:3
select(flights, starts_with("arr"))
select(flights, contains("dep"))
rename(flights, YEAR=year)
select(flights, time_hour, air_time, everything()) #시험 문제


###my turn
#1
filter(flights, is.na(dep_time)) %>% arrange(sched_dep_time, sched_arr_time)
#2
arrange(flights, desc(dep_delay))  







