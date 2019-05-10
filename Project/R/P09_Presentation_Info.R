
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

stop_words_smart<-stopwords(language = "en",source="smart")
stop_words_snowball<-stopwords(language = "en",source = "snowball")

text_sample_BOW<-"This is an example of bag of words"
print(text_sample_BOW)
tokenized_BOW<-txt.to.words.ext(text_sample_BOW, language = "English.all",
                 preserve.case = FALSE)
BOW<-txt.to.features(tokenized.text = tokenized_BOW,ngram.size = 1,features = 'w')
print(BOW)

text_sample_ngram<-"This is an example of n-gram"
print(text_sample_ngram)
tokenized_ngram<-txt.to.words.ext(text_sample_ngram, language = "English.all",
                                preserve.case = FALSE)
ngram<-txt.to.features(tokenized.text = tokenized_ngram,ngram.size = 3,features = 'w')
print(ngram)

feature_vector_bag_of_words<-readRDS('./RObjects/feature_vector_bag_of_words.rds')
feature_vector_bag_of_words[1:3,1:20]



feature_vector_bag_of_words<-readRDS('RObjects/feature_vector_bag_of_words.rds')
original_class_dist<-get_class_distribution_from_data(feature_vector_bag_of_words)

png('plots/original_dist.png')
barplot(original_class_dist$Freq[1:20],names.arg = original_class_dist$class_id[1:20], las=2,border ="black",col="black",
        xlab = 'Class Id',ylab = 'Frequency')
dev.off()

fec_vec_bow_sample_strata<-get_stratified_sample_from_data(feature_vector =feature_vector_bag_of_words ,percentage = 0.05)
class_dist_005<-get_class_distribution_from_data(fec_vec_bow_sample_strata)
sum(class_dist_005$Freq)

png('plots/dist_005.png')
barplot(class_dist_005$Freq[1:20],names.arg = class_dist_005$class_id[1:20], las=2,border ="orange",col="orange", 
        xlab = 'Class Id',ylab = 'Frequency')
dev.off()

top_n_class_fec_vec<-filter_top_n_classes_from_data(feature_vector = fec_vec_bow_sample_strata,n_class = 10)
class_dist_top_n<-get_class_distribution_from_data(top_n_class_fec_vec)
sum(class_dist_top_n$Freq)

png('plots/top10.png')
barplot(class_dist_top_n$Freq,names.arg = class_dist_top_n$class_id, las=2,border ="skyblue",col="skyblue", 
        xlab = 'Class Id',ylab = 'Frequency')
dev.off()

train_test_set<-get_stratified_test_and_train_set(feature_vector = top_n_class_fec_vec, train_percentage=0.75)
train_dist<-get_class_distribution_from_data(train_test_set[[1]])

png('plots/training_set.png')
barplot(train_dist$Freq,names.arg = train_dist$class_id, las=2,border ="seagreen",col="seagreen",
        xlab = 'Class Id',ylab = 'Frequency')
dev.off()

test_dist<-get_class_distribution_from_data(train_test_set[[2]])

png('plots/test_set.png')

barplot(test_dist$Freq,names.arg = test_dist$class_id, las=2,border ="violet",col="violet", 
        xlab = 'Class Id',ylab = 'Frequency')

dev.off()





