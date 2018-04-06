# 데이터마이닝
# 구조적인데이터 정형데이터를 대상으로 유용하고 가치있는 패턴을 추출
# 
# 텍스트마이닝
# 자연어로구성된 비구조적인 데이터 비정형데이터를 대상으로 개체명
# 패턴 혹은 단어-문장관계정보를 추출
# 
# 형태소 
# 의미가있는 최소의 단위로서 더이상 분리가 불가능한 가장 작은 의미
# 
# 형태소분석
# 주어진 단어 또는 어절을 구성하는 각 형태소를 분리한 후 분리된 형태소의 기본형및ㅍ품사 정보를 추출


# 텍스트마이닝패키지 
install.packages('tm')
library(tm)

inputText<-readLines('c:/Java/output2.txt')

# 택스트마이닝 작업위해 코퍼스 corpus 형으로변환 
corpus<-Corpus(VectorSource(inputText))
# 문서 전처리 과정 수행 문장부호 특정문대 대소문자 변환작업수행 
docs<-tm_map(corpus,content_transformer(tolower))
docs<-tm_map(docs,removeNumbers)
docs<-tm_map(docs,removePunctuation)
docs<-tm_map(docs,stripWhitespace)
docs<-tm_map(docs,removeWords,c('New Jersey'))

print(docs)
print(docs[1])
inspect(docs)#코퍼스 저장본문
inspect(docs[1:3])#코퍼스 저장된문장
summary(docs) #코퍼스 자료요약정보

strwap(docs[1])#코퍼스에저장된문잔출력

# 전처리된문서들을 행렬로변환
dtm<- TermDocumentMatrix(docs)#단어문서행렬생성
dtm<-as.matrix(dtm)
freq<-sort(rowSums(dtm),decreasing = T)#출현빈도에따라 정ㄹ
freq[1:10]
barplot(freq[1:10]) #출현빈도에따라 그래프 출력 

# 불용어제거
# 입력문서를 이루는 단어 성분중에는 문서의 정보 의미를 표현하지 못하는 단어 즉 문서과 관련선이 없것 으로 간주하는 단어

# 불용어가 아닌 단어들은 가용어라 부름 특히 가용어 중에서 문서의 중심이되는 주제어 :키워드
# 일반적으로 문서 내에서 발생 빈도가 높은 단어들을 키워드선 선정

docs<- tm_map(docs,removeWords,stopwords('english'))
dtm2<-TermDocumentMatrix(docs)
dtm2<-as.matrix(dtm2)
freq2<-sort(rowSums(dtm2),decreasing = T)
freq2[1:10]
barplot(freq2[1:10])



