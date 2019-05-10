###########################
# ALDA: hw2.R 
############################

require(caret)
require(rpart)


calculate_distance_matrix <- function(train_matrix, test_matrix, method_name){
  # NOTE: This function has already been implemented for you.
  # DO NOT modifiy this function.
  
  # INPUT:
  # Input: train_matrix: type: matrix n_sentences x sentence_length,
  # where n_sentences is total # training rows (100 in the dataset supplied to you) and
  # sentence_length is the total # features (100 in the dataset supplied to you).
  # Input: test_matrix: type: matrix of size 50 x 100 (i.e, 50 rows, 100 features)
  # Input: method_name: type: string, can be one of the following values: ('calculate_euclidean', 'calculate_cosine')
  
  # OUTPUT:
  # output: a 50 x 100 matrix of type double, containing the distance/similarity calculation using method_name between 
  # every row in test to every row in train 
  # This function has already been implemented for you. It takes the data matrix and method name, outputs the distance
  # matrix based on the method name.

  distance_matrix = matrix(0L, nrow = nrow(test_matrix), ncol = nrow(train_matrix))
  # the looping logic for pairwise distances is already provided for you
    for(i in seq(1, nrow(test_matrix))){
      for(j in seq(1, nrow(train_matrix))){
        distance_matrix[i,j] <- do.call(method_name, list(unlist(test_matrix[i,]), unlist(train_matrix[j,])))
      }
    }
  return(distance_matrix)
}

calculate_euclidean <- function(p, q) {
  # Input: p, q are vectors of size 1 x 100, each representing a row (i.e., a sentence) from the original dataset.
  # output: a single value of type double, containing the euclidean distance between the vectors p and q
  # Write code here to calculate the euclidean distance between pair of vectors p and q
  euclidean_distance <- sqrt(sum((p-q)^2))
  return(euclidean_distance)
}

calculate_cosine <- function(p, q) {
  # Input: p, q are vectors of size 1 x 100, each representing a row (i.e., a sentence) from the original dataset.
  # output: a single value of type double, containing the cosine distance between the vectors p and q
  # Write code here to calculate the cosine distance between pair of vectors p and q
  cosine_distance <- sum(p*q)/((calculate_vector_norm(p))*(calculate_vector_norm(q)))
  return(cosine_distance)
}

#util function to get vector norm
calculate_vector_norm <- function(p){
  
  vector_norm <- sqrt(sum(p^2))
  
  return(vector_norm)
}

knn_classifier <- function(x_train, y_train, x_test, distance_method, k){
  # You will be IMPLEMENTING a KNN Classifier here
  
  # Build a distance matrix by computing the distance between every test sentence 
  # (row in training TF-IDF matrix) and training sentence (row in test TF-IDF matrix).
  # Use the above calculate_distance_matrix function to calculate this distance matrix (code already given to you).
  # You can re-use the calculate_euclidean and calculate_cosine methods from HW1 here.
  # Once the distance matrix is computed, for each row in the distance matrix, calculate the 'k' nearest neighbors
  # and return the most frequently occurring class from these 'k' nearest neighbors.
  
  # INPUT:
  # x_train: TF-IDF matrix with dimensions: (number_training_sentences x number_features)
  # y_train: Vector with length number_training_sentences of type factor - refers to the class labels
  # x_test: TF-IDF matrix with dimensions: (number_test_sentences x number_features)
  # k: integer, represents the 'k' to consider in the knn classifier
  # distance_method: String, can be of type ('calcualte_euclidean' or 'calculate_cosine')
  # OUTPUT:
  # A vector of predictions of length = number of sentences in y_test and of type factor.
  
  # NOTE 1: Don't normalize the data before calculating the distance matrix
  
  # NOTE 2: For cosine, remember, you are calculating similarity, not distance. As a result, K nearest neighbors 
  # k values with highest values from the distance_matrix, not lowest. 
  # For euclidean, you are calculating distance, so you need to consider the k lowest values. 
  
  # NOTE 3:
  # In case of conflicts, choose the class with lower numerical value
  # E.g.: in 5NN, if you have 2 NN of class 1, 2 NN of class 2, and 1 NN of class 3, there is a conflict b/w class 1 and class 2
  # In this case, you will choose class 1. 
  
  # NOTE 4:
  # You are not allowed to use predefined knn-based packages/functions. Using them will result in automatic zero.
  # Allowed packages: R base, utils
  
  distance_matrix <- calculate_distance_matrix(x_train, x_test, distance_method)
  
  if (distance_method == "calculate_euclidean"){
    decre=FALSE
  }else if (distance_method =="calculate_cosine"){
    decre=TRUE
  }

  #matrix of indexes of sorted values for all testing values, ascending or descending order depending on  similarity measure
  nn_index_matrix.sorted<-t(apply(distance_matrix,1,order,decreasing=decre))
  
  #matrix of k first indexes
  knn_index_matrix<-nn_index_matrix.sorted[,1:k]
  
  #predicted classes for each test instance, based on majority vote
  predicted_classes<-t(apply(knn_index_matrix,1,get_max_class_from_row, y_train))
  
  return(as.factor(predicted_classes))
}

#util function to get predicted_classes per row
get_max_class_from_row <- function (row, class_train){
  #using table() to get frequencies of classes
  freq_table<-table(class_train[row])
  #getting the mode with lowest numerical values (index 1)
  result <-names(freq_table)[which (freq_table==max(freq_table))][1]
  
  return(result)
}


knn_classifier_confidence <- function(x_train, y_train, x_test, distance_method='calculate_cosine', k){
  # You will be trying to build a modified KNN classifier using the paper given in the HW
  # While most of the implementation is similar to the KNN classifier, there is one additional step you need to do.
  # Read the HW PDF for more details about this method
  
  # INPUT:
  # x_train: TF-IDF matrix with dimensions: (number_training_sentences x number_features)
  # y_train: Vector with length number_training_sentences of type factor - refers to the class labels
  # x_test: TF-IDF matrix with dimensions: (number_test_sentences x number_features)
  # k: integer, represents the 'k' to consider in the knn classifier
  # distance_method: String, can be of type ( 'calculate_cosine')
  
  # OUTPUT:
  # A vector of predictions of length = number of sentences in y_test and of type factor.
  
  # Read the NOTES from comments under knn_classifier.
  # Allowed packages: R base, utils
  
  
  distance_matrix <- calculate_distance_matrix(x_train, x_test, distance_method)
  
  if (distance_method == "calculate_euclidean"){
    decre=FALSE
  }else if (distance_method =="calculate_cosine"){
    decre=TRUE
  }
  
  #matrix of indexes of all nn in a sorted order, ascending or descending order depending on  similarity measure
  nn_index_matrix.sorted<-t(apply(distance_matrix,1,order,decreasing=decre))
  
  #matrix of distances for all of all nn, ascending or descending order depending on  similarity measure
  nn_distance_matrix.sorted<-t(apply(distance_matrix,1,sort,decreasing=decre))
  
  #matrix of knn indexes
  knn_index_matrix<-nn_index_matrix.sorted[,1:k]
  
  #matrix of distances to knn
  knn_distance_matrix<-nn_distance_matrix.sorted[,1:k]
  
  #matrix of k first classes corresponding to the knn
  knn_class<-t(apply(knn_index_matrix,1,get_classes_from_row))
  
  #initializing distance matrix
  confidence_matrix<- matrix(0L, nrow = nrow(x_test), ncol = length(levels(y_train)))
  #getting numeric sequence of classes (factors)
  levels_numeric<-as.numeric(levels(y_train))
  for (i in seq(1,nrow(x_test))){
    for(j in levels_numeric){
      #confidence matrix, rows = test instances, columns = class , confidence_matrix[i,j] have confidence of test instance= i and class=j
      confidence_matrix[i,j]<-sum(knn_distance_matrix[i,which(knn_class[i,]==j)]) / sum(knn_distance_matrix[i,])
    }
  }
  
  #for each row identify the max value, and the corresponding column for that max value is the predicted class
  predicted_classes<-max.col(confidence_matrix,ties.method = "first")
  
  return(as.factor(predicted_classes))
}

get_classes_from_row <- function (row){
  
  result <-y_train[row]
  
  return(result)
}


dtree <- function(x_train, y_train, x_test){
  set.seed(123)
  # You will build a CART decision tree, then use the tuned model to predict class values for a test dataset.
  
  # INPUT:
  # x_train: TF-IDF matrix with dimensions: (number_training_sentences x number_features)
  # y_train: Vector with length number_training_sentences of type factor - refers to the class labels
  # x_test: TF-IDF matrix with dimensions: (number_test_sentences x number_features)
  
  # OUTPUT:
  # A vector of predictions of length = number of sentences in y_test and of type factor.
  
  # Allowed packages: rpart, R Base, utils
  
  # HINT1: Make sure to read the documentation for the rpart package. Check out the 'rpart' and 'predict' functions.
  
  # HINT2: I've given you attributes and class labels as separate variables. Do you need to combine them 
  # into a data frame for rpart?
  
  #Using cbind to have training set with class
  train_attr_and_class <-cbind(x_train,Class=y_train)
  
  #training decision tree using gini index for splitting
  tree.gini.model <- rpart(Class~., data=train_attr_and_class, method = 'class')
 
  #classifing test set with previous model
  predicted_classes <- predict(tree.gini.model,newdata=x_test,type="class")
  
  #uname to get rid of attribute $names
  return(unname(predicted_classes))
}


dtree_cv <- function(x_train, y_train, x_test, n_folds){
  set.seed(123)
  # You will build a decision tree and tune its parameters using n-fold crossvalidation on the *training* dataset,
  # then use the tuned model to predict class values for a test dataset.
  
  # INPUT:
  # x_train: TF-IDF matrix with dimensions: (number_training_sentences x number_features)
  # y_train: Vector with length number_training_sentences of type factor - refers to the class labels
  # x_test: TF-IDF matrix with dimensions: (number_test_sentences x number_features)
  # n_folds: integer, refers to the number of folds for n-fold cross validation
  
  # OUTPUT:
  # A vector of predictions of length = number of sentences in y_test and of type factor.
  
  # Allowed packages: rpart, caret, R Base, utils
  
  # HINT1: Make sure to read the documentation for the caret package. Check out the 'train' and 'trainControl' functions.
  
  #CARET tune cp(complexity parameter) when it comes to RPART (CART by default - split by gini), with trainControl
  #here using (n_folds)-FOLDS CV (method=CrossValidation, number= number of folds)
  #trainControl.treee.gini.cv <- trainControl(method = "cv",number=n_folds)
  
  #CARET tune cp(complexity parameter) when it comes to RPART (CART by default - split by gini), with trainControl
  #here using (n_folds)-FOLDS CV (method=CrossValidation, number= number of folds)
  trainControl.treee.gini.cv <- trainControl(method = "cv",number=n_folds)
  
  #Caret uses train function to train based on trainControl ( to tune cp)
  tree.gini.model.cv <- train(x_train, y_train,method="rpart",trControl =trainControl.treee.gini.cv,tuneLength = 10)
  #testing model, type=raw to get predictions for predict.train
  predicted_classes <- predict(tree.gini.model.cv,newdata =x_test,type="raw")
  
  return(as.factor(predicted_classes))
  
}


calculate_accuracy <- function(y_pred, y_true){
  # Given the following:
  
  # INPUT:
  # y_pred: predicted class labels (vector, each value of type factor)
  # y_true: ground truth class labels (vector, each value of type factor)
  
  # OUTPUT:
  # a list in the following order: [confusion matrix, overall accuracy], where confusion matrix is of class "table"
  # (see Figure 2 in the PDF for an example Confusion Matrix)
  # and overall accuracy is on a scale of 0-1 of type double
  # overall class accuracy = accuracy of all the classes
  
  # confusion matrix should have Prediction to the left, and Reference on the top.
  
  #using caret confusion matrix
  confusion.matrix <- confusionMatrix(y_pred,y_true)
  
  #getting table and overall accuracy from confusionmatrix object
  result <- list("ConfusionMatrix"=confusion.matrix$table,"OverallAccuracy"=confusion.matrix$overall[["Accuracy"]])
  
  return(result)
  
}

