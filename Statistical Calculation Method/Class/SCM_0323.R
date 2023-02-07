library(dplyr)
library(tidyverse)
library(nycflights13)

#mutate() - 새로운 열을 생성
flights_sml = select(flights, year:day, ends_with("delay"), distance, air_time)
mutate(flights_sml, gain=dep_delay - arr_delay, speed=distance/air_time*60)

#transmute() - 생성한 열만 데이터 생성함
#정수만 뽑아줌 : %/% 정수빼고 나머지만 생성:%% -> 100으로 나누어주어 시간 분, 구분할ㄹ 수 있다.

cumsum #1, 1+2, 1+2+3 ....
cummin #                    가장 작은 수
cummax #데이터수를 늘려가며 가장 큰 수

min_rank #
min_rank(dexc()) #가장 큰수에다가 가장 작은 순위
row_number
dense_rank()

##########################################################################################
###1 my turn

#hour = flights$dep_time %/% 100
#minute = flights$dep_time %% 100
#flights[which(flights$dep_time == 2400),]

flights_my = mutate(flights, cov = ifelse(dep_time==2400, 0, (dep_time%/%100)*60 + dep_time%%100))
select(flights, dep_time, cov)

flights = mutate(flights, dep_time_cov = ((dep_time%/%100)*60 + dep_time%%100) %% 1440,
sched_dep_time_cov = ((shed_dep_time%/%100)*60 + sched_dep_time%%100) %% 1440)

###2 my turn 
flights_my = mutate(flights,dep_time_cov = ((dep_time%/%100)*60 + dep_time%%100) %% 1440,
                    arr_time_cov = ((arr_time%/%100)*60 + arr_time%%100) %% 1440,
                    diff_cov = arr_time_cov - dep_time_cov)
#변수가 하나일 때 그림 - histogram, boxplot
ggplot(flights_my) +
  geom_histogram((aes(air_time-diff_cov)), binwidth=1)


###3 my turn -> delay되어 다음날 출발하게 된 경우
flights_my = mutate(flights, dep_time_cov = ((dep_time%/%100)*60 + dep_time%%100) %% 1440,
                             sched_dep_time_cov = ((sched_dep_time%/%100)*60 + sched_dep_time%%100) %% 1440,
                             dep_del = dep_time_cov - sched_dep_time_cov )
ggplot(flights_my) +
  geom_histogram(aes(dep_delay-dep_del))
ff = select(flights_my, dep_time, sched_dep_time, dep_time_cov, sched_dep_time_cov, dep_delay, dep_del)
filter(ff, dep_delay-dep_del !=0 )


###4
min_rank(desc(flights$dep_delay)),
row_number(desc(flights$dep_delay)),
dense_Rank(desc())

flights_rank = arrange(flights, min_rank<10|row_number<10|dense_rank<10)

##열: select slice:행 top_n(flights, 10,dep_delay) function 존재
###5
1:3 + 1:10 #원했던 계산 아니다 -> 주의해야함


######################################summarise()
summarise(flights, dep_delay=mean(dep_delay, na.rm=TRUE))
by_day = group_by(flights, year, month, day)
summarise(by_day, delay=mean(dep_delay, na.rm=TRUE))

#파이프라인이용하여 한번에 해보기
by_dest = group_by(flights, dest)
delay = summarise(by_dest, count=n(), dist=mean(distance), delay=mean(arr_delay))
delay = filter(delay, count>20, dest!="HNL")
ggplot(delay, mapping=aes(x=dist,y=delay)) +
  geom_point(aes(size=count), alpha=1/3) + geom_smooth(se=FALSE)

#error group이 하나씩밖에 없어서 계산할게 없다
flights %>% flights_rank=group_by(year,month, day) %>% 
  summarise(mean=mean(dep_delay, na.rm=TRUE))

#na인것 없애주기
not_canceld = flights

fregpoly() : density화



delays

n()
sum(!is.na(x))


#logical vlaue sum -> count가 된다
#logical value mean -> proportion 

#48
sum을 통해
하루-> 달별 -> 1년

#ungrouping - 파이프라인이용한 것
daily %>% 
  ungroup() %>% 
  summarise(flights=n())



########################################################################
#1 my turn
not_cancelled = flights %>% filter(!is.na(dep_delay), !is.na(arr_delay))
not_cancelled %>% count(dest)
dest_by = group_by(not_cancelled, dest)
summarise(dest_by, n())

not_cancelled %>% count(tailnum, wt=distance)
tail_by = group_by(not_cancelled, tailnum)
summarise(tail_by, sum_dist=sum(distance))

#2 my turn
per_day = group_by(flights, year, month, day)
cancel = summarise(per_day, prop = mean(is.na(dep_delay)|is.na(arr_delay)))
cancel

#3 my turn
flights %>% group_by(carrier, dest ) %>% summarise(n())

#4 my turn


###################mutate
flights_sml %>% 
  group_by(year,month,day) %>% 
  filter(rank(desc(arr_delay))<10)

ungroup 하지 않는 이상 group유지된ㄷ다



####practice
tibble(x=1:9, group=rep(c("a","b","c"), each=3)) %>% 
  mutate(x_mean = mean(x)) %>%  #group이 없었으므로 전체 평균
  group_by(group) %>% 
  mutate(x_mean_2 = mean(x)) #그룹별 평균

tibble(x=1:9, group=rep(c("a","b","c"), each=3)) %>% 
  mutate(y = x+2) %>% 
  group_by(group) %>% 
  mutate(z = x+2)

tibble(x=1:9, g)
lag : 이전값
lead : 다음 값
-> group안에서 lag, lead 값 달라지
짐

A %>%  arrange(X) %>% 
  mutate(lag_x=lag(x))

A %>% group_by()
















































