#####Chap15.������ �ð�ȭ II: �ܾ� ������ ����� �ؽ�Ʈ �������� �ð�ȭ

###15.1 ���۵�! ���� ������
###15.2 �ڿ��� ó���� �ؽ�Ʈ ���̴� ȯ�� �غ�
install.packages(c("tm","SnowballC","wordcloud"))
library(tm)
library(SnowballC)
library(wordcloud)

data = read.csv("C:\\Users\\eunju\\Desktop\\GitHub\\stat_r_practice\\Sillicon Valley\\JEOPARDY_CSV.csv", 
                stringsAsFactors = FALSE, nrows=10000)
library(dplyr)
glimpse(data)

#�ؽ�Ʈ������ ����ġ�� ��ȯ
data_corpus = Corpus(VectorSource(data$Question))
data_corpus

data_corpus = tm_map(data_corpus, content_transformer(tolower)) #�ҹ��ں���
as.character(data_corpus[[1]])#� ��ȭ�� �ִ��� Ȯ�� ����
data_corpus = tm_map(data_corpus, removePunctuation) #������ ����
as.character(data_corpus[[1]])
data_corpus = tm_map(data_corpus, removeWords, stopwords('english')) #�ҿ�� ����
as.character(data_corpus[[1]])
data_corpus = tm_map(data_corpus, stemDocument) #�����
as.character(data_corpus[[1]])

wordcloud(data_corpus, max.words=100, random.order=FALSE,
          colors=brewer.pal(8, "Dark2"), xpd=NA)

##Error in FUN(content(x), ...) : invalid multibyte string 2803 ���� �ذ�
Sys.setlocale(category="LC_ALL", locale="us")