rm(list = ls(all = T))

# set working directory
#setwd('./')

library(titanic)
source('./hw2.R')


# function to load and process training and test data 
# Please note that TA may have a completely different dataset with the same dimensions as the one provided to you
load_train_and_test_data <- function(folder_path='./'){
  # Input: folder_path - points to the folder containing the hw2_training and hw2_test csv files
  # TA has different versions the same file, following the same properties of the data (# row, # columns and class values {1,2,3,4})
  tr_data <- read.csv(paste0(folder_path, 'hw2_training.csv'), stringsAsFactors= T)
  te_data <- read.csv(paste0(folder_path, 'hw2_test.csv'), stringsAsFactors= T)
  X_train <- tr_data[, 1:100]
  X_test <- te_data[, 1:100]
  y_train <- as.factor(tr_data[, 101])
  y_test <- as.factor(te_data[, 101])
 return(list(X_train, X_test, y_train, y_test)) 
}

# read data from disk, extract train test into separate variables 
all_data <- load_train_and_test_data('./')
X_train <- all_data[[1]]
X_test <- all_data[[2]]
y_train <- all_data[[3]]
y_test <- all_data[[4]]

# x_train<-X_train
# x_test<X_test
# n_holds<-5
#alternate dataset iris 
# iris.ds <- iris
# index <- createDataPartition(iris.ds$Species, p=.8, list=FALSE)
# 
# X_train<- iris.ds[index,][,1:4]
# y_train<- iris.ds[index,][,5]
# 
# X_test<- iris.ds[-index,][,1:4]
# y_test<- iris.ds[-index,][,5]


# calculate classification outcomes using KNN with euclidean distance
euclidean_classification <- knn_classifier(X_train, y_train, X_test, 'calculate_euclidean', 5)

# calculate classification outcomes using KNN with cosine distance
cosine_classification <- knn_classifier(X_train, y_train, X_test, 'calculate_cosine', 5)

# calculate classification outcomes using KNN_V2 with cosine distance
knn_conf_classification <- knn_classifier_confidence(X_train, y_train, X_test,  'calculate_cosine', 5)

# calculate classification outcomes using Decision Tree using rpart and gini index with default hyperparameters
dt_classification <- dtree(X_train, y_train, X_test)

# calculate classification outcomes using a tuned Decision Tree
dt_cv_classification <- dtree_cv(X_train, y_train, X_test, 5)

euclidean_result <- calculate_accuracy(euclidean_classification, y_test)
cosine_result <- calculate_accuracy(cosine_classification, y_test)
conf_result <- calculate_accuracy(knn_conf_classification, y_test)
dt_result <- calculate_accuracy(dt_classification, y_test)
dt_cv_result <- calculate_accuracy(dt_cv_classification, y_test)
print(dt_result$OverallAccuracy)
print(dt_cv_result$OverallAccuracy)

