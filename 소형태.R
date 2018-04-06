# 형태소분석
# https://github.com/haven-jeon/KoNLP/blob/master/etcs/KoNLP-API.md

install.packages('devtools')
library(devtools)

install.packages('rJava')
install.packages('memoise')
install.packages('KoNLP') # 한글 형태소 분석 지원
install.packages('dplyr')

library(rJava)
library(memoise)
library(KoNLP)
library(dplyr)
library(stringr)

Sys.setenv(JAVA_HOME='C:/Java/jdk1.8.0_152')

# 형태소 분석을 위한 참고사전 필요
# 분석할 문장에 포함된 단어들이
# 사전에 알맞는 형태(품사)로 정의되어야 정확한 분석 가능
# KoNLP에는 세종 사전이 포함되어 있음
# useNIADic()
useSejongDic()

install_github('haven-jeon/NIADic/NIADic', build_vignettes=T)

library(NIADic)
sejong <- get_dic('sejong') # 세종사전 적재
head(sejong)

# 형태소 분리 : 명사 추출
sentence <- "아버지 가방에 들어가신다"
extractNoun(sentence)

sentence <- "분석할 문장에 포함된 단어들이 사전에 알맞는 형태(품사)로 정의되어야 정확한 분석이 가능 KoNLP에는 세종 사전이 포함되어 있음"
extractNoun(sentence)
