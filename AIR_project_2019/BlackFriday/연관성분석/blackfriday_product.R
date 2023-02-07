install.packages('arules')
install.packages('arulesViz')
install.packages('tidyverse')
library(arules)
library(arulesViz)
library(tidyverse)
data<- read.csv("C:\\Users\\이은주\\Desktop\\AIR 동아리\\블랙프라이데이\\BlackFriday.csv")
attach(data)  # 변수명 저장 
glimpse(data)
select <- dplyr::select   # MASS 패키지의 select 함수와 충돌하기 때문에 지정해야 함  


products<-data %>%
  select(User_ID,Product_ID) %>%
    group_by(User_ID)%>%
      arrange(User_ID)%>%
        mutate(id=row_number())%>%
          spread(User_ID,Product_ID)%>%
            t()                              # 한 열에 id번호


rownames(products)
products[1,]
products=products[-1,]  #id 열 지우기

write.csv(products, file = 'products.csv')  #데이터 프레임을 csv형식으로 

Products = read.transactions('products.csv', sep = ',',rm.duplicates = TRUE)
#transaction 데이터를 연관규칙분석을 위해 sparse format의 itemMatrix로 업로드하기 위해서
# 연관성분석은 sparse matrix 사용  

#glimpse(Products)  #안 됨
#str(Products)  # 안 됨
summary(Products)
#inspect(Products)
itemFrequencyPlot(Products, topN = 25,type='absolute')

Products@data



rules = apriori(data = Products,control=list(verbos=F),
                parameter = list(support = 0.008, confidence = 0.80, maxtime = 0))

rules1 = apriori(data = Products,control=list(verbos=F),
                parameter = list(support = 0.012, confidence = 0.65, maxtime = 0),appearance=list(rhs="P00265242",default='lhs'))



plot(rules, method = 'graph')

x11()
plot(rules1, method = 'graph')