########
# Spring 2019 - ALDA - Group P09 
# 
# 
# 
#
#
#
#########
# clear workspace
rm(list=ls(all=T))
cat('\014') # clear console


#

# install all necessary packages
required_packages = c("caTools","caret","devtools","plyr", "uniqtag","rlist","stylo","stopwords","wordcloud")
for(package in required_packages){
  if(!(package %in% installed.packages())){
    if(package=="uniqtag"){
      devtools::install_github("sjackman/uniqtag")
    }else{
      install.packages(package, dependencies = T)
    }
  }   
}

library('plyr')
library('uniqtag')
library('stylo')
library('rlist')
library('caret')
library('caTools')
library('stopwords')
library('wordcloud')


