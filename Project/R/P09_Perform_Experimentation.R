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
  
  
  row_names<-c("Delta.Delta",         
               "Delta.Eder",          
               "Delta.Argamon",       
               "Delta.Simple",        
               "Delta.Cosine",        
               "Delta.Wurzburg",      
               "Delta.Entropy",       
               "Delta.Euclidean",     
               "Delta.Manhattan",     
               "Delta.Canberra",      
               "K-NN",                
               "SVM",                 
               "NSC",                 
               "Naive-Bayes"  )
  
  bow<-matrix(,nrow = 14,ncol = 0)
  bow_snowball<-matrix(,nrow = 14,ncol = 0)
  bow_smart<-matrix(,nrow = 14,ncol = 0)
  ngram_3000<-matrix(,nrow = 14,ncol = 0)
  ngram_6000<-matrix(,nrow = 14,ncol = 0)
  
  rownames(bow)<-row_names
  rownames(bow_snowball)<-row_names
  rownames(bow_smart)<-row_names
  rownames(ngram_3000)<-row_names
  rownames(ngram_6000)<-row_names
  
  for(feature_vec_name in c("feature_vector_bag_of_words","feature_vector_bag_of_words_snowball",
                            "feature_vector_bag_of_words_smart","feature_vector_ngrams_3000","feature_vector_ngrams_6000") ){
    
  
    
    if (feature_vec_name=="feature_vector_bag_of_words"){
      feature_vector<-readRDS('./RObjects/feature_vector_bag_of_words.rds')
      
    }else if (feature_vec_name=="feature_vector_bag_of_words_snowball"){
      feature_vector<-readRDS('./RObjects/feature_vector_bag_of_words.rds')
      stop_words_snowball<-stopwords(language = "en",source = "snowball")
      feature_vector<-delete.stop.words(input.data = feature_vector,stop.words = stop_words_snowball)
      
  
    }else if (feature_vec_name=="feature_vector_bag_of_words_smart"){
      feature_vector<-readRDS('./RObjects/feature_vector_bag_of_words.rds')
      stop_words_smart<-stopwords(language = "en",source = "smart")
      feature_vector<-delete.stop.words(input.data = feature_vector,stop.words = stop_words_smart)
      
    }else if (feature_vec_name=="feature_vector_ngrams_3000"){
      feature_vector<-readRDS('./RObjects/feature_vector_ngrams_3000.rds')
      
      
    }else if (feature_vec_name=="feature_vector_ngrams_6000"){
      feature_vector<-readRDS('./RObjects/feature_vector_ngrams_6000.rds')
      
      
    }
    
    
    for (sample in c(1:10)){
  
      top_n_class_fec_vec<-filter_top_n_classes_from_data(feature_vector = feature_vector,n_class = 10)
      
      fec_vec_sample_strata<-get_stratified_sample_from_data(feature_vector =top_n_class_fec_vec ,percentage = 0.05)
      
      train_test_set<-get_stratified_test_and_train_set(feature_vector = fec_vec_sample_strata, train_percentage=0.75)
      
      train_feature_vec<-train_test_set[[1]]
      
      test_feature_vec<-train_test_set[[2]]
      
      if (feature_vec_name=="feature_vector_bag_of_words"){
        result<-get_accuracy_per_run(train_feature_vec,test_feature_vec,feature_t="bow",suffix_flav="bow",sufix_run_n=sample)
        bow<-cbind(bow,unlist(result))
        
      }else if (feature_vec_name=="feature_vector_bag_of_words_snowball"){
        result<-get_accuracy_per_run(train_feature_vec,test_feature_vec,feature_t="bow",suffix_flav="snowball",sufix_run_n=sample)
        bow_snowball<-cbind(bow_snowball,unlist(result))
        
      }else if (feature_vec_name=="feature_vector_bag_of_words_smart"){
        result<-get_accuracy_per_run(train_feature_vec,test_feature_vec,feature_t="bow",suffix_flav="smart",sufix_run_n=sample)
        bow_smart<-cbind(bow_smart,unlist(result))
        
      }else if (feature_vec_name=="feature_vector_ngrams_3000"){
        result<-get_accuracy_per_run(train_feature_vec,test_feature_vec,feature_t="ngram",suffix_flav="3000",sufix_run_n=sample)
        ngram_3000<-cbind(ngram_3000,unlist(result))
        
      }else if (feature_vec_name=="feature_vector_ngrams_6000"){
        result<-get_accuracy_per_run(train_feature_vec,test_feature_vec,feature_t="ngram",suffix_flav="6000",sufix_run_n=sample)
        ngram_6000<-cbind(ngram_6000,unlist(result))
        
      }
    }
    
  }
  
  col_names<-c(1:10)
  col_names<-c(col_names,c("mean","sd","se"))
  
  ##bow
  mean_matrix<-apply(bow,1,mean)
  sd_matrix<-apply(bow,1,sd)
  se_matrix<-apply(bow,1,function(x) sd(x)/sqrt(length(x)))
  
  cbind(bow,mean_matrix,sd_matrix,se_matrix)
  colnames(bow)<-col_names
  
  #bow_snowball
  mean_matrix<-apply(bow_snowball,1,mean)
  sd_matrix<-apply(bow_snowball,1,sd)
  se_matrix<-apply(bow_snowball,1,function(x) sd(x)/sqrt(length(x)))
  
  cbind(bow_snowball,mean_matrix,sd_matrix,se_matrix)
  colnames(bow_snowball)<-col_names
  
  #bow_smart
  mean_matrix<-apply(bow_smart,1,mean)
  sd_matrix<-apply(bow_smart,1,sd)
  se_matrix<-apply(bow_smart,1,function(x) sd(x)/sqrt(length(x)))
  
  cbind(bow_smart,mean_matrix,sd_matrix,se_matrix)
  colnames(bow_smart)<-col_names
  
  #ngram_3000
  mean_matrix<-apply(ngram_3000,1,mean)
  sd_matrix<-apply(ngram_3000,1,sd)
  se_matrix<-apply(ngram_3000,1,function(x) sd(x)/sqrt(length(x)))
  
  cbind(ngram_3000,mean_matrix,sd_matrix,se_matrix)
  colnames(ngram_3000)<-col_names
  
  #ngram_6000
  mean_matrix<-apply(ngram_6000,1,mean)
  sd_matrix<-apply(ngram_6000,1,sd)
  se_matrix<-apply(ngram_6000,1,function(x) sd(x)/sqrt(length(x)))
  
  cbind(ngram_6000,mean_matrix,sd_matrix,se_matrix)
  colnames(ngram_6000)<-col_names
  
  
