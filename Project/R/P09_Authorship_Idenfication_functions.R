########
# Spring 2019 - ALDA - Group P09 
# 
# 
# 
#
#
#
#########
source("./P09_perform_delta.R")

create_corpus_enron <-function(input_folder='./data/',input_name='CleanData.csv',output_folder='./corpus/'){
  data_df<-read.csv(paste0(input_folder,input_name),header=F,stringsAsFactors = F,col.names = c("author_name","message"))
  
  author_as_factor <- as.factor(data_df$author_name)
  
  list_author_names<-unique(author_as_factor)
  list_author_code<-as.numeric(unique(author_as_factor))
  
  col_class_author_name <- data_df$author_name
  
  col_class_author_code <-mapvalues(col_class_author_name,from=list_author_names,to=list_author_code)
  
  data_df$author_code<-col_class_author_code
  
  col_file_name<-make_unique_all(as.character(data_df$author_code),sep="_")
  
  data_df$file_name<-col_file_name
  
  apply(data_df,1,write_row_in_corpus,output_folder)
  
  result<-list("data_frame"=data_df, "list_author_name"=list_author_names,"list_author_code"=list_author_code)
}

write_row_in_corpus <-function(row,data_folder){
  file_name<-paste0(data_folder,row[4],".txt")
  write(row[2],file_name,append = F)
  
}

load_corpus_enron<-function(corpus_folder= './corpus'){
  raw_corpus <- load.corpus(files = "all", corpus.dir = corpus_folder,
                            encoding = "UTF-8")
  
  tokenized_corpus <- txt.to.words.ext(raw_corpus, language = "English.all",
                                       preserve.case = FALSE)
  
  
  corpus_no_pronouns <- delete.stop.words(tokenized_corpus,
                                          stop.words = stylo.pronouns(language = "English"))
  
  result<-corpus_no_pronouns
  
  return(result)
}

extract_features<-function(corpus,method_extraction="bag_of_words", ngram_size,max_size){
  
  if(method_extraction=="bag_of_words"){
    processed_corpus <- txt.to.features(corpus, ngram.size = 1,features = "w")
    
    frequent_features <-make.frequency.list(processed_corpus, head =max_size )
    
    
  }else{
    
    clean_corpus<-list.clean(corpus,function(x)  length(x)<ngram_size)
    
    processed_corpus <- txt.to.features(clean_corpus, ngram.size = ngram_size,features = "w")
    
    frequent_features <-make.frequency.list(processed_corpus, head =max_size )
    
  }
  
  result<-list("frequent_features"=frequent_features,"corpus"=processed_corpus)
  return(result)
  
}

create_frequency_feature_vector<-function(corpus, features){
  
  feature_vector<-make.table.of.frequencies(corpus, features = features)
  
  return(feature_vector)
  
}

create_partition_feature_vector<-function(feature_vector, train_percentage){
  
  train_index<-createDataPartition(feature_vector[,1],p=train_percentage,list = F,times=1)
  
  train_feature_vector_bag_of_words<-feature_vector[train_index,]
  test_feature_vector_bag_of_words<-feature_vector[-train_index,]
  
  result<-list("train_set"=train_feature_vector_bag_of_words,"test_set"=test_feature_vector_bag_of_words)
  return(result)
}

get_top_n_classes_from_data<-function(class_ids, n_class){
  
  table_class_ids<-sort(table(class_ids),decreasing = T)
  
  data_frame_class_id<-as.data.frame(table_class_ids)
  names(data_frame_class_id)[1]<-"class_id"
  
  df_top_classes <-data_frame_class_id[1:n_class,]
  
  top_classes<-df_top_classes$class_id
  

  return(top_classes)
}

filter_top_n_classes_from_data<-function(feature_vector, n_class){
  
  class_ids<-c(gsub("_.*", "", rownames(feature_vector)))
  
  top_list_class<-get_top_n_classes_from_data(class_ids,n_class)
  index_filtered_classes <- which(class_ids %in% top_list_class)
  
  filtered_feature_vector<-feature_vector[index_filtered_classes,]
  
  return(filtered_feature_vector)
  
}

get_stratified_sample_from_data<-function(feature_vector, percentage){
  class_ids<-c(gsub("_.*", "", rownames(feature_vector)))
  
  index_stratified_sample<-sample.split(class_ids,SplitRatio = percentage)
  
  feature_vector_stratified_sample<-feature_vector[index_stratified_sample,]
  
  return(feature_vector_stratified_sample)
}

get_stratified_test_and_train_set<-function(feature_vector, train_percentage){
  class_ids<-c(gsub("_.*", "", rownames(feature_vector)))
  
  index_split_strata_index<-sample.split(class_ids,SplitRatio = train_percentage)
  
  train_strata_set<-feature_vector[index_split_strata_index,]
  test_strata_set<-feature_vector[!index_split_strata_index,]
  
  result<-list("training_set"=train_strata_set,"test_set"=test_strata_set)
  
  return(result)
}

get_class_distribution_from_data<-function(feature_vector){
  
  class_ids<-c(gsub("_.*", "", rownames(feature_vector)))
  
  table_class_ids<-sort(table(class_ids),decreasing = T)
  
  data_frame_class_id<-as.data.frame(table_class_ids)
  names(data_frame_class_id)[1]<-"class_id"
  
  return(data_frame_class_id)
  
}

get_samples_from_class<-function(feature_vector,class_id){
  class_ids<-c(gsub("_.*", "", rownames(feature_vector)))
  
  index_filtered_classes <- which(class_ids ==class_id)
  
  filtered_feature_vector<-feature_vector[index_filtered_classes,]
  
  return(filtered_feature_vector)

}

normalize_data <- function(data_matrix){

  normalized_data <-t(apply(data_matrix, 1, normalize_vector))
  return(normalized_data)
}

normalize_vector <- function(vector){
  
  normalized_vector <- (vector - min(vector))/(max(vector)-min(vector))
  return(normalized_vector)
}


get_accuracy_per_run<-function(train_feature_vec,test_feature_vec,feature_t,suffix_flav="bow",sufix_run_n){
  
  result_delta_delta <- get_max_accuracy_from_method(train_feature_vector = train_feature_vec,test_feature_vector = test_feature_vec,
                                                     method = "delta",dist = "delta", 
                                                     feature_type =feature_t ,suffix_flavor = suffix_flav,suffix_run_num = sufix_run_n)
  
  result_delta_eder <- get_max_accuracy_from_method(train_feature_vector = train_feature_vec,test_feature_vector = test_feature_vec,
                                                    method = "delta",dist = "eder", 
                                                    feature_type =feature_t ,suffix_flavor = suffix_flav,suffix_run_num = sufix_run_n)
  
  result_delta_argamon <- get_max_accuracy_from_method(train_feature_vector = train_feature_vec,test_feature_vector = test_feature_vec,
                                                       method = "delta",dist = "argamon", 
                                                       feature_type =feature_t ,suffix_flavor = suffix_flav,suffix_run_num = sufix_run_n)
  
  result_delta_simple <- get_max_accuracy_from_method(train_feature_vector = train_feature_vec,test_feature_vector = test_feature_vec,
                                                      method = "delta",dist = "simple", 
                                                      feature_type =feature_t ,suffix_flavor = suffix_flav,suffix_run_num = sufix_run_n)
  
  result_delta_cosine <- get_max_accuracy_from_method(train_feature_vector = train_feature_vec,test_feature_vector = test_feature_vec,
                                                      method = "delta",dist = "cosine", 
                                                      feature_type =feature_t ,suffix_flavor = suffix_flav,suffix_run_num = sufix_run_n)
  
  result_delta_wurzburg <- get_max_accuracy_from_method(train_feature_vector = train_feature_vec,test_feature_vector = test_feature_vec,
                                                        method = "delta",dist = "wurzburg", 
                                                        feature_type =feature_t ,suffix_flavor = suffix_flav,suffix_run_num = sufix_run_n)
  
  result_delta_entropy <- get_max_accuracy_from_method(train_feature_vector = train_feature_vec,test_feature_vector = test_feature_vec,
                                                       method = "delta",dist = "entropy", 
                                                       feature_type =feature_t ,suffix_flavor = suffix_flav,suffix_run_num = sufix_run_n)
  
  result_delta_euclidean <- get_max_accuracy_from_method(train_feature_vector = train_feature_vec,test_feature_vector = test_feature_vec,
                                                         method = "delta",dist = "euclidean", 
                                                         feature_type =feature_t ,suffix_flavor = suffix_flav,suffix_run_num = sufix_run_n)
  
  result_delta_manhattan <- get_max_accuracy_from_method(train_feature_vector = train_feature_vec,test_feature_vector = test_feature_vec,
                                                         method = "delta",dist = "manhattan", 
                                                         feature_type =feature_t ,suffix_flavor = suffix_flav,suffix_run_num = sufix_run_n)
  
  result_delta_canberra <- get_max_accuracy_from_method(train_feature_vector = train_feature_vec,test_feature_vector = test_feature_vec,
                                                        method = "delta",dist = "canberra", 
                                                        feature_type =feature_t ,suffix_flavor = suffix_flav,suffix_run_num = sufix_run_n)
  
  result_knn<-get_max_accuracy_from_method(train_feature_vector = train_feature_vec,test_feature_vector = test_feature_vec,
                                           method = "knn", 
                                           feature_type =feature_t ,suffix_flavor = suffix_flav,suffix_run_num = sufix_run_n)
  
  result_svm<-get_max_accuracy_from_method(train_feature_vector = train_feature_vec,test_feature_vector = test_feature_vec,
                                           method = "svm", 
                                           feature_type =feature_t ,suffix_flavor = suffix_flav,suffix_run_num = sufix_run_n)
  
  result_nsc<-get_max_accuracy_from_method(train_feature_vector = train_feature_vec,test_feature_vector = test_feature_vec,
                                           method = "nsc", 
                                           feature_type =feature_t ,suffix_flavor = suffix_flav,suffix_run_num = sufix_run_n)
  
  result_nb<-get_max_accuracy_from_method(train_feature_vector = train_feature_vec,test_feature_vector = test_feature_vec,
                                          method = "naivebayes", 
                                          feature_type =feature_t ,suffix_flavor = suffix_flav,suffix_run_num = sufix_run_n)
  
  
  
  result<-list("Delta.Delta"=result_delta_delta$Max_Accuracy,
               "Delta.Eder"=result_delta_eder$Max_Accuracy,
               "Delta.Argamon"=result_delta_argamon$Max_Accuracy,
               "Delta.Simple"=result_delta_simple$Max_Accuracy,
               "Delta.Cosine"=result_delta_cosine$Max_Accuracy,
               "Delta.Wurzburg"=result_delta_wurzburg$Max_Accuracy,
               "Delta.Entropy"=result_delta_entropy$Max_Accuracy,
               "Delta.Euclidean"=result_delta_euclidean$Max_Accuracy,
               "Delta.Manhattan"=result_delta_manhattan$Max_Accuracy,
               "Delta.Canberra"=result_delta_canberra$Max_Accuracy,
               "K-NN"=result_knn$Max_Accuracy,
               "SVM"=result_svm$Max_Accuracy,
               "NSC"=result_nsc$Max_Accuracy,
               "Naive-Bayes"=result_nb$Max_Accuracy)
  return(result)
}



get_max_accuracy_from_method<-function(train_feature_vector,test_feature_vector, method, dist=NULL, feature_type="BOW", 
                                       suffix_flavor, suffix_run_num){

  k_max<-length(unique(c(gsub("_.*", "", rownames(test_feature_vector)))))
  k_list<-c(1:k_max)
  accuracy_vector<-vector()
  k_max_index<-NULL
  
  if(method=="delta"){
    if(is.null(dist) || dist %in% c("euclidean", "manhattan", "canberra",
                                  "delta", "eder", "argamon",
                                  "simple", "cosine", "wurzburg",
                                  "entropy")){
      for(k in k_list){
        result_perform<-perform.delta_P09_ALDA(training.set = train_feature_vector,
                                                test.set = test_feature_vector,distance=dist,
                                                no.of.candidates = k)
        
        con_mat<-attr(result_perform,'confusion_matrix')
        
        accuracy<-sum(diag(con_mat))/sum(con_mat)
        print(accuracy)
        accuracy_vector<-c(accuracy_vector,accuracy)
        
      }
      
      png(paste0('./plots/',method,"_",dist,"_",feature_type,"_",suffix_flavor,"_",suffix_run_num,'.png'))
      plot(x=k_list,y=accuracy_vector,main = paste0('Plot ',feature_type,' ',method,"_",dist),type = "b",
           xlab = "K values",ylab = "Accuracy")
      dev.off()
      
      k_max_index<-order(accuracy_vector,decreasing = T)[1]
      max_accuracy<-accuracy_vector[k_max_index]
      
      
    }
  }else if(method=="knn"){
    for(k in k_list){
      result_perform<-perform.knn(training.set = train_feature_vector,
                                             test.set = test_feature_vector,k.value = k)
      
      con_mat<-attr(result_perform,'confusion_matrix')
      
      accuracy<-sum(diag(con_mat))/sum(con_mat)
      print(accuracy)
      accuracy_vector<-c(accuracy_vector,accuracy)
      
    }
    png(paste0('./plots/',method,"_",feature_type,"_",suffix_flavor,"_",suffix_run_num,'.png'))
    plot(x=k_list,y=accuracy_vector,main = paste0('Plot ',feature_type,' ',method),type = "b")
    dev.off()
    
    k_max_index<-order(accuracy_vector,decreasing = T)[1]
    max_accuracy<-accuracy_vector[k_max_index]
  }else if(method=="svm"){
    result_perform<-perform.svm(training.set = train_feature_vector,
                                test.set = test_feature_vector)
    
    con_mat<-attr(result_perform,'confusion_matrix')
    max_accuracy<-sum(diag(con_mat))/sum(con_mat)
    
  }else if(method=="nsc"){
    result_perform<-perform.nsc(training.set = train_feature_vector,
                                test.set = test_feature_vector)
    
    con_mat<-attr(result_perform,'confusion_matrix')
    max_accuracy<-sum(diag(con_mat))/sum(con_mat)
  }else if(method=="naivebayes"){
    result_perform<-perform.naivebayes(training.set = train_feature_vector,
                                test.set = test_feature_vector)
    
    con_mat<-attr(result_perform,'confusion_matrix')
    max_accuracy<-sum(diag(con_mat))/sum(con_mat)
  }
  
  result<-list("Max_Accuracy"=max_accuracy,"K_Max"=k_max_index,"Accuracy_List"=accuracy_vector)
  return(result)
}



