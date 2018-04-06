#감성분석
# 특정대상에대하여 소비자가 느끼는 감성 과 그애 대한 이유를 분석
# 감석분석은 사건이 얼마나 많은양의 정제된 감성어를 포함하고 있는지에따라 분석의 품질달 달라질수 있음
# 감성분석 사전을 구축하기 위해서는 다양한 머신러닝 기법을 통해 구축할 수 있으나 정확성을 높이기 위해서는 사람이 직간접적으로 참여하는 과정이필요 

install.packages('twitteR')
install.packages('ROAuth')
install.packages('plyr')
install.packages('stringr')

# 패키지 불러오기
library(twitteR)
library(ROAuth)
library(plyr)
library(stringr)

load("C:/Java/apple.RData")
load("C:/Java/samsung.RData")
length(apple.tweets)
apple.tweets[1:5]

tweet<-apple.tweets[[1]]
tweet$getScreenName()
tweet$getText()

apple.text<-lapply(apple.tweets,function(t){t$getText()})
head(apple.text,3)
#감성분석을위한긍정 부정사전을불러옴
pos.word=scan("c:/Java/positive-words.txt",what="character",comment.char=";")
neg.word=scan("c:/Java/negative-words.txt",what="character",comment.char=";")옴

pos.words<-c(pos.word,"upgrade")
neg.words<-c(neg.word,"wait","waiting")


score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  scores <- laply(sentences, function(sentence, pos.words, neg.words){
    sentence <- gsub('[[:punct:]]', "", sentence)
    sentence <- gsub('[[:cntrl:]]', "", sentence)
    sentence <- gsub('\\d+', "", sentence)
    
    sentence <- tolower(sentence)
    word.list <- str_split(sentence, '\\s+')
    words <- unlist(word.list)
    
    pos.matches <- match(words, pos.words)
    neg.matches <- match(words, neg.words)
    
    pos.matches <- !is.na(pos.matches)
    neg.matches <- !is.na(neg.matches)
    
    score <- sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress=.progress)
  scores.df <- data.frame(score=scores, text=sentences)
  return(scores.df)
}

apple.scores=score.sentiment(apple.text,pos.words,neg.words,.progress='text')

hist(apple.scores$score)


