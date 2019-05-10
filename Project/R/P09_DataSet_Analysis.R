########
# Spring 2019 - ALDA - Group P09 
# 
# 
# 
#
#
#
#########
source("./P09_library_loader.R")
source("./P09_Authorship_Idenfication_functions.R")

result_load_corpus_enron<-readRDS('RObjects/corpus_enron.rds')

processed_corpus <- txt.to.features(result_load_corpus_enron, ngram.size = 1,features = "w")

frequent_features <-make.frequency.list(processed_corpus, head =3000,value = T ,relative = F)
word_list<-names(frequent_features)

png(filename ='plots/rawDataWordCloud.png' )
wordcloud(words = word_list, freq = frequent_features,max.words=1000, random.order = F,
          colors=brewer.pal(8, "Paired"))
dev.off()

frequent_features_to_plot<-frequent_features
word_index<-c(1:length(frequent_features_to_plot))

png(filename ='plots/barPlot_before_stop_words.png' )

barplot(frequent_features_to_plot,names.arg = word_index,border ="darkseagreen"
        , xlab = "Word Id", ylab = "Frequency in Corpus")
dev.off()
#removing stopwords
stop_words_snowball<-stopwords(language = "en",source = "snowball")
processed_corpus_no_snow_ball<-delete.stop.words(input.data = processed_corpus,stop.words = stop_words_snowball)



frequent_features_no_snowball <-make.frequency.list(processed_corpus_no_snow_ball, head =3000,value = T ,relative = F)
word_list_no_snowball<-names(frequent_features_no_snowball)

png(filename ='plots/rawDataWordCloud_no_snow.png' )
wordcloud(words = word_list_no_snowball, freq = frequent_features_no_snowball,max.words=1000, random.order = F,
          colors=brewer.pal(8, "Paired"))
dev.off()

fec_freq_no_snow<-frequent_features_no_snowball
word_index_no_snow<-c(1:length(fec_freq_no_snow))

options(scipen = 50) 

png(filename ='plots/barPlot_after_stop_words.png' )

barplot(fec_freq_no_snow,names.arg = word_index_no_snow,border ="darkseagreen"
        , xlab = "Word Id", ylab = "Frequency in Corpus")

dev.off()



