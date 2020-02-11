###Silicon Valley_Chap.4

###시각화의 중요성
library(dplyr)
library(gapminder)

glimpse(gapminder)
gapminder[,c(4,6)]
summary(gapminder$lifeExp)
summary(gapminder$gdpPercap)
cor(gapminder$lifeExp,gapminder$gdpPercap) #lifeExp와 gdpPercap의 상관계수

#상관관계 시각화
opar = par(mfrow=c(2,2))
hist(gapminder$lifeExp) #lifeExp에 대한 히스토그램
hist(gapminder$gdpPercap,nclass=50) #dbl에 대해 50씩 계급구간을 나누어 히스토그램
hist(log10(gapminder$gdpPercap),nclass=50) #변수변환(log)한 후 히스토그램
plot(log10(gapminder$gdpPercap),gapminder$lifeExp,cex=.5) #상관관계
par(opar)

###ggplot2
install.packages("ggplot2")
library(ggplot2)

#opar = par(mfrow=c(3,1)) #ggplot에서는 이용되지 않는다. 
ggplot(gapminder,aes(x=lifeExp)) + geom_histogram()
ggplot(gapminder,aes(x=gdpPercap)) + geom_histogram() #한쪽으로 치우쳐진 분포
ggplot(gapminder,aes(x=gdpPercap)) + geom_histogram() + scale_x_log10() #로그변환
ggplot(gapminder,aes(x=gdpPercap, y=lifeExp)) + geom_point() + scale_x_log10() + geom_smooth()
#par(opar)

#예제
example(ggplot)
set.seed(1234)
df = data.frame(gp=factor(rep(letters[1:3],each=10)),y=rnorm(30))

letters[1:3] #알파벳 순으로 첫번째(a)부터 세번째(c)까지
rep(c(1,2),5) #1212121212 -> (1,2)를 5번씩
rep(c(1,2),each=5) #1111122222 -> 각각 5번씩
rnorm(25) #정규분포를 따르는 난수 n개 생성(평균(mean)과 표준편차(sd) 지정 가능)

glimpse(df)
ds = summarize(group_by(df,gp),mean=mean(y),sd=sd(y))
ds

ggplot(df,aes(gp,y)) + geom_point() + geom_point(data=ds,aes(gp,mean),colour='red',size=3)
ggplot(df) + geom_point(aes(gp,y)) + geom_point(data=ds,aes(gp,mean),colour='red',size=3)
ggplot() + geom_point(data=df,aes(gp,y)) + 
  geom_point(data=ds,aes(gp,mean),colour='red',size=3) +
  geom_errorbar(data=ds,aes(gp,mean,ymin=mean-sd,ymax=mean+sd), colour='red',width=0.4)

#한수량형 변수 ggplot
library(gapminder)
library(ggplot2)
library(dplyr)
ggplot(gapminder,aes(x=gdpPercap)) + geom_histogram()
ggplot(gapminder,aes(x=gdpPercap)) + geom_histogram() + scale_x_log10() #로그변환
ggplot(gapminder,aes(x=gdpPercap)) + geom_freqpoly() + scale_x_log10() #도수폴리곤
ggplot(gapminder,aes(x=gdpPercap)) + geom_density() + scale_x_log10() #커널밀도측정

#한범주형 변수 ggplot
ggplot(diamonds,aes(cut)) + geom_bar() #막대그래프

table(diamonds$cut) #도수
round(prop.table(table(diamonds$cut))*100,1) #상대도수,퍼센트
mutate(tally(group_by(diamonds,cut)),pct=round(n/sum(n)*100,1))

#두수량형 변수 ggplot
glimpse(diamonds)
ggplot(diamonds,aes(carat,price)) + geom_point()
ggplot(diamonds,aes(carat,price)) + geom_point(alpha=.01) #alpha:점의 투명도

glimpse(mpg)
ggplot(mpg,aes(cyl,hwy)) + geom_point() #중복된 관측치로 인해 정확한 분포X
ggplot(mpg,aes(cyl,hwy)) + geom_jitter() #점들을 조금 흩어주어 분포 확인O

#두개이상의 연속변수
pairs(sample_n(diamonds,1000))

#수량형변수와 범주형변수
glimpse(mpg)

#boxplot+관측치
ggplot(mpg,aes(class,hwy)) + 
  geom_boxplot(alpha=.5) + geom_jitter(colour="gray")
#class변수의 범주 재정렬
ggplot(mutate(mpg,class=reorder(class,hwy,median)),aes(class,hwy))+
  geom_boxplot(alpha=.5) + geom_jitter(colour="gray")
#class변수의 범주 직접 재정렬
ggplot(mutate(mpg,class=factor(class,levels=c("2seater","subcompact","compact","midsize","minivan","suv","pickup")))
       ,aes(class,hwy)) + geom_boxplot(alpha=.5) + geom_jitter(colour="gray")
#x축과 y축 위치 변경
ggplot(mutate(mpg,class=factor(class,levels=c("2seater","subcompact","compact","midsize","minivan","suv","pickup")))
       ,aes(class,hwy)) + geom_boxplot(alpha=.5) + geom_jitter(colour="gray") + coord_flip()

#두범주형 변수
glimpse(Titanic)
xtabs(Freq~.,Titanic) #도수분포
mosaicplot(Titanic,main="Survival on the Titanic")
mosaicplot(Titanic,main="Survival on the Titanic",color=TRUE) #시각화 #colour과 color의 차이 주의!

apply(Titanic,c(3,4),sum)
round(prop.table(apply(Titanic,c(3,4),sum)),3)
round(prop.table(apply(Titanic,c(3,4),sum),margin=1),3) #margin=1 옵션의 차이!

apply(Titanic,c(2,4),sum)
round(prop.table(apply(Titanic,c(2,4),sum),margin=1),3)

t = data.frame(Titanic)
t2 = summarize(group_by(t,Sex),n=sum(Freq),survivors=sum(ifelse(Survived=="Yes",Freq,0)))
mutate(t2, rate_survival = survivors/n)
#↕차이점이 없다.
summarize(group_by(t,Sex),n=sum(Freq),survivors=sum(ifelse(Survived=="Yes",Freq,0)),rate_survival = survivors/n)

#이변량 이상의 변수일 때 - geom의 속성 사용
library(gapminder)
library(dplyr)
library(ggplot2)
glimpse(gapminder)

ggplot(filter(gapminder,year==2007),aes(gdpPercap,lifeExp)) + 
  geom_point() + scale_x_log10() + ggtitle("Gapminder data for 2007")
ggplot(filter(gapminder,year==2007),aes(gdpPercap,lifeExp)) + 
  geom_point(aes(size=pop,color=continent)) + scale_x_log10() + ggtitle("Gapminder data for 2007")

#이변량 이상의 변수일 때 - facet_* 함수 사용
ggplot(gapminder,aes(year,lifeExp,group=country)) + geom_line()
ggplot(gapminder,aes(year,lifeExp,group=country, col=continent)) + geom_line()
ggplot(gapminder,aes(year,lifeExp,group=country)) + geom_line() + facet_wrap(~continent)
ggplot(gapminder,aes(year,lifeExp,group=country)) + geom_line() + facet_grid(~continent)