#####AIR 3월 24일 과제#####

#과제1
install.packages("ggplot2", repos="https://cran.rstudio.com/")

rm(list=ls())
library(ggplot2)
library(dplyr)
adult <- read.csv("C:\\Users\\이은주\\Desktop\\AIR 동아리\\adult.data", header=FALSE, strip.white=T)
names(adult) <- c('age','workclass','fnlwgt','education','education_num','marital_status','occupation',
			'relationship','race','sex','capital_loss','hours_per_week','native_country','wage')

adult_1 <- adult[complete.cases((adult[ , c("workclass")]), ) #workclass가 ?인 관측치 제거

adult_1 %>%
	ggplot(aes(x=workclass, y=age, fill=wage)) #wage별로 다른색깔
	+geom_boxplot(position='dodge')
	+coor_flip() #x축 y축 바꾸기



#과제2
install.packages("ggplot2", repos="https://cran.rstudio.com/")

rm(list=ls())
library(ggplot2)
library(dplyr)
adult <- read.csv("C:\users\이은주\desktop\AIR 동아리\adult.data", header=FALSE, strip.white=T)
names(adult) <- c('age','workclass','fnlwgt','education','education_num','marital_status','occupation',
			'relationship','race','sex','capital_loss','hours_per_week','native_country','wage')

adult_2 <- adult[complete.cases((adult[ , c("workclass")]), ) #workclass가 ?인 관측치 제거

adult_2 %>%
	group by(workclass, relationship, rate_mid) %>%
	tally() %>%
	ggplot(aes(x=relationship, y=rate_mid, group=workclass, color=workclass)) #workclass별로 다른색깔
	+ geom_point() + geom_line() #꺽은선그래프

