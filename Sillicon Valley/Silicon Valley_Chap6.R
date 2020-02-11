#Silicon_Valley Chap.6

library(dplyr)

#수면시간 예제
summary(sleep)
y = sleep$extra[sleep$group==1]
#y = filter(sleep,group==1) #extra,group,ID 모두 추출됨
summary(y)
sd(y)

par(mfrow=c(2,2))
hist(y)
boxplot(y)
qqnorm(y); qqline(y) #정규분포
hist(y,prob=TRUE) #상대도수 히스토그램
lines(density(y),lty=2) #밀도함수

#일변량t검정
t.test(y) #df(자유도), p값(유의확률),신뢰구간,평균 도출
          #양측검정
t.test(y,alternative="greater") #단측검정



