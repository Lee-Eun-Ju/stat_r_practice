#####Chap15.데이터 시각화 II: 단어 구름을 사용한 텍스트 데이터의 시각화

###15.1 제퍼디! 질문 데이터
###15.2 자연어 처리와 텍스트 마이닝 환경 준비
install.packages(c("tm","SnowballC","wordcloud"))
library(tm)
library(SnowballC)
library(wordcloud)

data = read.csv("C:\\Users\\eunju\\Desktop\\GitHub\\stat_r_practice\\Sillicon Valley\\JEOPARDY_CSV.csv", 
                stringsAsFactors = FALSE, nrows=10000)
library(dplyr)
glimpse(data)

#텍스트변수를 말뭉치로 변환
data_corpus = Corpus(VectorSource(data$Question))
data_corpus

data_corpus = tm_map(data_corpus, content_transformer(tolower)) #소문자변경
as.character(data_corpus[[1]])#어떤 변화를 주는지 확인 가능
data_corpus = tm_map(data_corpus, removePunctuation) #구두점 제거
as.character(data_corpus[[1]])
data_corpus = tm_map(data_corpus, removeWords, stopwords('english')) #불용어 제거
as.character(data_corpus[[1]])
data_corpus = tm_map(data_corpus, stemDocument) #어간추출
as.character(data_corpus[[1]])

wordcloud(data_corpus, max.words=100, random.order=FALSE,
          colors=brewer.pal(8, "Dark2"), xpd=NA)

##Error in FUN(content(x), ...) : invalid multibyte string 2803 오류 해결
Sys.setlocale(category="LC_ALL", locale="us")
