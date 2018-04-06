
install.packages('devtools')
library(devtools)

install.packages('rJava')
install.packages('memoise')
install.packages('KoNLP') 
install.packages('dplyr')

library(memoise)
library(KoNLP)
library(dplyr)
library(stringr)

Sys.setenv(JAVA_HOME='C:/Java/jdk1.8.0_152')

useSejongDic()

install_github('haven-jeon/NIADic/NIADic', build_vignettes=T)

library(NIADic)
sejong <- get_dic('sejong')
head(sejong)

read <- readLines('c:/Java/speech.txt') 
head(read, 3) 

docs <- str_replace_all(read, "\\W", " ")
docs <- str_replace_all(read, "\\[", "")
docs <- str_replace_all(read, "\\[0 ~ 9]", "")
docs <- str_replace_all(read, "\\[a-z]", "")
docs <- str_replace_all(read, "\\[ㄱ-ㅎ]", "") 
docs <- str_replace_all(read, "\\[~!@#$%^&*()_+=<>?]", "")
nouns <- extractNoun(docs) 
nouns

wordcount <- table(unlist(nouns))

df_word <- as.data.frame(wordcount, stringsAsFactor=F)
df_word

df_word <- rename(df_word, word=Var1, freq=Freq)


df_word$word <- as.character(df_word$word)
df_word <- filter(df_word, nchar(word) >= 2)

top20 <- df_word %>%
  arrange(desc(freq)) %>%
  head(20)

top20

install.packages('wordcloud')
library(wordcloud)
library(RColorBrewer) 

pal <- brewer.pal(8, "Dark2") 

wordcloud(word = df_word$word, 
          freq =df_word$freq, 
          min.freq = 2,        
          max.words = 250,  
          random.order = F,   
          rot.per = .1,      
          scale = c(4, 0.3),   
          colors = pal)      

