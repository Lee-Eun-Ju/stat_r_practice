###Silicon Valley Chap.3

##데이터 가공 연습
install.packages("gapminder")
library(gapminder)
head(gapminder)
levels(gapminder$country)

#특정 조건을 만족하는 행, 열 추출
gapminder[gapminder$country=="Korea, Rep.", c('pop','gdpPercap')]
#정렬
gapminder[order(gapminder$year, gapminder$country),]
#변수명 바꾸기
names(gapminder) #열의 이름 나열
names(gapminder)[6] = 'gdp_per_cap' #6번째 열의 이름 변경
#변수 생성
gapminder$total_gdp = gapminder$pop * gapminder$gdp_per_cap
head(gapminder)
#요약 통계량
median(gapminder$gdp_per_cap) #중앙값
summary(gapminder) #문자->범주별 정리, 숫자->최소,평균,최대,사분위수
apply(gapminder[,4:6],2,mean) #margin=1 : 열(row)의 mean
#margin=2 : 행(col)의 mean

##dplyr 패키지 연습
library(dplyr)

tbl_df(iris) #스크린에 예쁘게 표시될 정도의 행과 열만 출력
glimpse(iris)
iris %>% glimpse

filter(gapminder,year=="1957"&country=="Korea, Rep.") #행 추출
filter(gapminder,c(year=="1957",country=="Korea, Rep.")) #조건이므로 벡터X

arrange(gapminder,year,country) #두 변수에 대해 정렬-> 변수1정렬 후 변수2정렬
arrange(gapminder,c(year,country)) #정렬에 순서가 필요하므로 오류

select(gapminder,c(country,year)) #열추출
select(gapminder,country,year) #열추출 -> 같은 결과 도출

gap_1 = mutate(gapminder,total_gdp=pop*gdpPercap) #변수변환 및 생성
gap_1

summarize(gapminder,n(),n_distinct(continent),
          mean(gdpPercap),nth(year,40)) #요약통계량

sample_n(gapminder,50) #n개 랜덤 샘플
sample_frac(gapminder,0.4) #40% 랜덤 샘플

distinct(gapminder,continent) #고유한 행 추출
distinct(gapminder,pop)

summarize(group_by(gapminder,continent),mean(pop)) #그룹화에 의한 요약통계량
summarize(group_by(filter(gapminder,year=="2007"),continent),median(lifeExp))