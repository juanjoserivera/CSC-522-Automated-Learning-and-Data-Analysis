#############
# hw3 checker file
# Do not submit this file
# 
#############
# clear workspace
rm(list=ls(all=T))
cat('\014') # clear console

# source hw3.R
source('./hw4.R')

# set your working directory
# setwd()

# install all necessary packages
required_packages = c("caret", "nnet", "stats", "dplyr", "ggplot2")
for(package in required_packages){
  if(!(package %in% installed.packages())){
    install.packages(package, dependencies = T)
  }   
}


# load the packages
library('caret') # for cross validation
library('nnet') # for neural networks
library('stats') # for clustering
library('dplyr') # if needed
library('ggplot2') # for plotting elbow plot

# set seed
set.seed(100)
############################################################################################################
# Helper functions
# TA will use something similar to load data for his own system
# For regression data
load_data <- function(data_folder='./data/', learning_type){
  # this method will read data for clustering/classification and return list containing two data frames:
  
  # for clustering (specified by learning_type = "clustering") 
  # two columns (x, y) of continous data (of type double)
  
  # for classification (specified by learning_type = "classification")
  # first 4 columns (x1-x4) are attributes, last column (class) is your dependent variable (factor))
  
  # for classification, please note, TA WILL use the same training dataset, but a different test set 
    # TA's test set will have the same attributes (x1-x4, class), but may contain different number of data points
  
  # for clustering, please note, TA will use a different dataset
  
  # make sure dependent variable is of type factor if this is classification
  if(learning_type == 'classification'){
    train_df <- read.csv(paste0(data_folder, learning_type, '-train.csv'), header=T)
    test_df <- read.csv(paste0(data_folder, learning_type, '-test.csv'), header=T)
    train_df$class <- as.factor(train_df$class)
    test_df$class <- as.factor(test_df$class)
    return(list(train_df, test_df))
  }else{
    data_df <- read.csv(paste0(data_folder, learning_type, '_sample.csv'), header = T)
  }
  
}

##########################################################################################################
# Load data
# load data necessary for clustering
clustering_data <- load_data(data_folder='./data/', learning_type='clustering')

# load data necessary for classification
clf_data <- load_data(data_folder='./data/', learning_type='classification')
clf_train_df <- clf_data[[1]]
clf_test_df <- clf_data[[2]]

############################################################################################################ 
# Running clustering and classification  

###############################################
# Clustering

# KMeans
kmeans_result <- alda_cluster(data_df = clustering_data, n_clusters = 2, clustering_type = "kmeans")
kmeans_sse <- alda_calculate_sse(data_df = clustering_data, cluster_assignments = kmeans_result)

# Single link
single_link_result <- alda_cluster(data_df = clustering_data, n_clusters = 2, clustering_type = "single-link")
single_link_sse <- alda_calculate_sse(data_df = clustering_data, cluster_assignments = single_link_result)

# complete link
complete_link_result <- alda_cluster(data_df = clustering_data, n_clusters = 2, clustering_type = "complete-link")
complete_link_sse <- alda_calculate_sse(data_df = clustering_data, cluster_assignments = complete_link_result)


# Setup for analysis section in clustering
# generate the elbow plot for kmeans for c(1, 2, 3, 4, 5, 6, 7)
alda_kmeans_elbow_plot(data_df = clustering_data, k_values = c(1, 2,3,4,5,6,7))

# Code's already been written, no need to make any changes here
# First, lets evaluate the SSE values by printing them
print(paste("Kmeans SSE for given params = ", kmeans_sse))
print(paste("Single link SSE for given params = ", single_link_sse))
print(paste("Complete link SSE for given params = ", complete_link_sse))

# Next, lets evaluate visually by visualizing them
png("kmeans_plot.png")
plot(clustering_data$x, clustering_data$y, type='p', pch = '*', col=kmeans_result, main = "KMeans with 2 clusters", xlab = 'x', ylab = 'y')
dev.off()

png("singleLink_plot.png")
plot(clustering_data$x, clustering_data$y, type='p', pch = '*', col=single_link_result, main = "Single Link with 2 clusters", xlab = 'x', ylab = 'y')
dev.off()

png("completeLink_plot.png")
plot(clustering_data$x, clustering_data$y, type='p', pch = '*', col=complete_link_result, main = "Complete link with 2 clusters", xlab = 'x', ylab = 'y')
dev.off()
########################################################
# Classification

# Neural networks

# build different neural networks with different number of units in the hidden layer
# parameter choices for neural networks
# size - refers to # hidden units - we will set this to c(4, 8, 16) i.e., 4 hidden units, 8 hidden units and 16 hidden units in the hidden layer 
# decay - parameter used to avoid overfitting - we will set this to c(0.2, 0.5, 0.002, 0.005)
# there are several other parameters you could change, but in the interest of complexity, lets restrict ourselves to these parameters 
# specific details about more parameters are given under the function alda_nn

# create a parameter grid
parameter_grid <- expand.grid(size=c(4, 8, 16), decay = c(0.2, 0.5, 0.002, 0.005))

# perform grid search, predict on test set
nn_result <- alda_nn(x_train = clf_train_df[,-5], x_test = clf_test_df[, -5],
                     y_train = clf_train_df[,5],
                     parameter_grid = parameter_grid)

print(paste("Accuracy of my NN model = ", confusionMatrix(nn_result[[2]], clf_test_df[, 5])$overall['Accuracy']))
print("Confusion Matrix of my NN model: ")
print(confusionMatrix(nn_result[[2]], clf_test_df[, 5])$table)
print("Best Hyperparameters:")
print(nn_result[[1]]$bestTune)

