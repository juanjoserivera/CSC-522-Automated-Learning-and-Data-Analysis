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


#creating corpus from raw_data to be used with stylo, this is creating files into output_folder
result_create_corpus_enron <-create_corpus_enron(input_folder='./data/',input_name='CleanData.csv',output_folder='./corpus/')

#load stylo.corpus
result_load_corpus_enron<-load_corpus_enron(corpus_folder = './corpus/')

#extract bow features, top 3000 features
result_extract_features_bow<-extract_features(corpus = result_load_corpus_enron,method_extraction="bag_of_words",max_size = 3000)

#extract ngram features, top 3000 features
result_extract_features_ngrams<-extract_features(corpus = result_load_corpus_enron,method_extraction="ngrams",ngram_size = 3,max_size = 6000)

#creating feature vector bow start: 12.31 finish
feature_vector_bag_of_words<-create_frequency_feature_vector(corpus=result_extract_features_bow[[2]],features =result_extract_features_bow[[1]] )

#creating feature vector ngrams start: 12.31 finish

feature_vector_ngrams<-create_frequency_feature_vector(corpus=result_extract_features_ngrams[[2]],features =result_extract_features_ngrams[[1]])

#given that generation of this feature vectors is resource intensive we could save this using saveRDS()


