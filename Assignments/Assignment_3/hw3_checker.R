#############
# hw3 checker file
# Do not submit this file
# 
#############
# clear workspace
rm(list=ls(all=T))
cat('\014')

# source hw3.R
source('./hw3.R')

# set your working directory
# setwd()

# install all necessary packages
required_packages = c('e1071', 'caret', 'glmnet')
for(package in required_packages){
  if(!(package %in% installed.packages())){
    install.packages(package, dependencies = T)
  }   
}


# load the packages
library('e1071') # for SVM
library('glmnet') # for linear, ridge and lasso regression

# set a seed for reproducibility
set.seed(100)
############################################################################################################
# Helper functions
# TA will use something similar to load data for his own system
# For regression data
load_data <- function(data_folder='./data/', learning_type){
  # this method will read data for regression/classification and return list containing two data frames:
  
  # for regression (specified by learning_type = "regression") 
  # first 19 columns (x1-x19) are attributes, last column (y) is your dependent variable (continuous))
  
  # for classification (specified by learning_type = "classification")
  # first 4 columns (x1-x4) are attributes, last column (class) is your dependent variable (continuous))
  
  # please note, TA WILL use the same training dataset, but a different test set 
  # TA's test set will have the same attributes, but may contain different number of data points
  train_df <- read.csv(paste0(data_folder, learning_type, '-train.csv'), header=T)
  test_df <- read.csv(paste0(data_folder, learning_type, '-test.csv'), header=T)
  
  # make sure dependent variable is of type factor if this is classification
  if(learning_type == 'classification'){
    train_df$class <- as.factor(train_df$class)
    test_df$class <- as.factor(test_df$class)
  }
  return(list(train_df, test_df))
}

##########################################################################################################
# Load data
# load data necessary for regression
reg_data <- load_data(data_folder='./data/', learning_type='regression')
reg_train_df <- reg_data[[1]]
reg_test_df <- reg_data[[2]]

# load data necessary for classification
clf_data <- load_data(data_folder='./data/', learning_type='classification')
clf_train_df <- clf_data[[1]]
clf_test_df <- clf_data[[2]]

############################################################################################################ 
# Learning and parameter tuning 

###############################################
# Regression

# slr
# simple_linear_regression_result: list, first argument is the model, second argument are the predicted values
simple_linear_regression_result <- alda_regression(x_train=as.matrix(reg_train_df[,-20]),x_test=as.matrix(reg_test_df[,-20]), 
                                      y_train=reg_train_df[,20], regression_type='linear') 

# ridge 
# ridge_regression_result: list, first argument is the model, second argument are the predicted values
ridge_regression_result <- alda_regression(x_train=as.matrix(reg_train_df[,-20]),x_test=as.matrix(reg_test_df[,-20]), 
                                              y_train=reg_train_df[,20], regression_type ='ridge')

# lasso
# lasso_regression_result: list, first argument is the model, second argument are the predicted values
lasso_regression_result <- alda_regression(x_train=as.matrix(reg_train_df[,-20]),x_test=as.matrix(reg_test_df[,-20]), 
                                              y_train=reg_train_df[,20], regression_type = 'lasso')

# compare rmse
all_regression_summary <- regression_compare_rmse(y_test=reg_test_df[,20], 
                                     linear_regression_prediction = simple_linear_regression_result[[2]], 
                                     ridge_prediction = ridge_regression_result[[2]], 
                                     lasso_prediction = lasso_regression_result[[2]])
print(paste('Best regression model =', all_regression_summary[[1]], 'RMSE =', all_regression_summary[[2]], 
            'RMSE vector=',all_regression_summary[3]))

########################################################
# Classification

# SVM

# linear kernel
linear_svm_result <- alda_svm(x_train = clf_train_df[,-5], x_test = clf_test_df[,-5], y_train = clf_train_df[,5], 
                              kernel_name = 'linear')

# radial kernel
radial_svm_result <- alda_svm(x_train = clf_train_df[,-5], x_test = clf_test_df[,-5], y_train = clf_train_df[,5], 
                              kernel_name = 'radial')

# sigmoid kernel
sigmoid_svm_result <- alda_svm(x_train = clf_train_df[,-5], x_test = clf_test_df[,-5], y_train = clf_train_df[,5], 
                              kernel_name = 'sigmoid')

# polynomial kernel
# linear svm
polynomial_svm_result <- alda_svm(x_train = clf_train_df[,-5], x_test = clf_test_df[,-5], y_train = clf_train_df[,5], 
                              kernel_name = 'polynomial')




# compare all classifiers
all_classifier_summary <- classification_compare_accuracy(y_test=clf_test_df[,5], 
                                                   linear_kernel_prediction = linear_svm_result[[2]], 
                                                   radial_kernel_prediction = radial_svm_result[[2]], 
                                                  polynomial_kernel_prediction = polynomial_svm_result[[2]], 
                                                  sigmoid_kernel_prediction = sigmoid_svm_result[[2]])

print(paste('Best classification model =', all_classifier_summary[[1]], 'Overall Accuracy =', all_classifier_summary[[2]],
            'Accuracy Vector=',all_classifier_summary[3]))






