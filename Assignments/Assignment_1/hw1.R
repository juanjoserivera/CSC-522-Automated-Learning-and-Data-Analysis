###########
# HW1
############

# You may use the following libraries:
# If you get an error when running these lines, 
# make sure to install the respective libraries
require(ggplot2)
require(reshape2)
require(data.table)
require(philentropy)
require(plyr)
require(utils)

# read data matrix
read_data <- function(path = './hw1_word_frequency.csv') {
  # Note 1: DO NOT change the function arguments
  # Input: path: type: string, output: a matrix containing data from hw1_word_frequency.csv
  # Write code here to read the csv file as a matrix and return it.
  
  data.df<-read.csv(path, header=FALSE)
  return(data.df)
  
}

calculate_matrix <- function(data_matrix, method_name){
  # NOTE: This function has already been implemented for you.
  # DO NOT modifiy this function.
  # Input: data_matrix: type: matrix n_sentences x sentence_length,
  # where n_sentences is total # sentences (155 in the dataset supplied to you) and
  # sentence_length is the total length of each sentence (200 in the dataset supplied to you).
  # Input: method_name: type: string, can be one of the following values: ('calculate_euclidean', 'calculate_cosine', 'calculate_manhattan', 'calculate_chebyshev')
  # output: a 155 x 155 matrix of type double, containing the distance/similarity calculation using method_name between 
  # every pair of sentences 
  # This function has already been implemented for you. It takes the data matrix and method name, outputs the distance
  # matrix based on the method name.
  # Programming logic for selecting every pair wise rows from the data matrix has already been provided for you
  # for euclidean, cosine and manhattan. You are only required to write the logic to calculate the actual distances for a pair of vectors p and q
  # in the corresponding functions listed above.
  # for chebyshev, you have been tasked to use the library. You will not need to use loops here, since the 
  # library is already optimized. Read the documentation and figure out how to compute the distance matrix
  # without loops for chebyshev
  distance_matrix = matrix(0L, nrow = nrow(data_matrix), ncol = nrow(data_matrix))
  if(method_name %in% c("calculate_euclidean", "calculate_cosine", "calculate_manhattan")){
    # the looping logic for pairwise distances is already provided for you
    for(i in seq(1, nrow(data_matrix))){
      for(j in seq(i, nrow(data_matrix))){
        distance_matrix[i,j] <- do.call(method_name, list(unlist(data_matrix[i,]), unlist(data_matrix[j,])))
        distance_matrix[j,i] <- distance_matrix[i,j]
      }
    }
  }else if(method_name == "calculate_chebyshev"){
    # for chebyshev, you have been tasked to use the library. You will not need to use loops here, since the 
    # library is already optimized. Read the documentation and figure out how to compute the distance matrix
    # without loops for chebyshev
    distance_matrix <- calculate_chebyshev(data_matrix)
  }
  return(distance_matrix)
}

calculate_euclidean <- function(p, q) {
  # Input: p, q are vectors of size 1 x 200, each representing a row (i.e., a sentence) from the original dataset.
  # output: a single value of type double, containing the euclidean distance between the vectors p and q
  # Write code here to calculate the euclidean distance between pair of vectors p and q
  
  euclidean_distance <- sqrt(sum((p-q)^2))
  return(euclidean_distance)
}

calculate_cosine <- function(p, q) {
  # Input: p, q are vectors of size 1 x 200, each representing a row (i.e., a sentence) from the original dataset.
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

calculate_manhattan <- function(p, q) {
  # Input: p, q are vectors of size 1 x 200, each representing a row (i.e., a sentence) from the original dataset.
  # output: a single value of type double, containing the manhattan distance between the vectors p and q
  # Write code here to calculate the manhattan distance between pair of vectors p and q
  
  manhattan_distance <- sum(abs(p-q))
  return(manhattan_distance)
}

calculate_chebyshev <- function(data_matrix){
  # Input: data_matrix: type: matrix n_sentences x sentence_length, 
  # where n_sentences is total # sentences (155 in the dataset supplied to you) and
  # sentence_length is the total length of each sentence (200 in the dataset supplied to you).
  # output: a 155 x 155 matrix of type double, containing the chebyshev distance between every pair of sentences
  # Write code here to calculate chebyshev distance given an original data matrix of size 155 x 200
  
  chebyshev_total_distance <- distance(data_matrix, method ="chebyshev")
  return(chebyshev_total_distance)
}

normalize_data <- function(data_matrix){
  # Input: data_matrix: type: matrix n_sentences x sentence_length, 
  # where n_sentences is total # sentences (155 in the dataset supplied to you) and
  # sentence_length is the total length of each sentence (200 in the dataset supplied to you).
  # output: a 155 x 200 matrix of type double, containing the normalized values in [0, 1] range per row.
  # Write code here to normalize data_matrix
  
  normalized_data <-t(apply(data_matrix, 1, normalize_vector))
  return(normalized_data)
}

#util function to get vector norm
normalize_vector <- function(vector){
  
  normalized_vector <- (vector - min(vector))/(max(vector)-min(vector))
  return(normalized_vector)
}

analyze_normalization <- function(data_matrix, normalized_data_matrix){
  # Input: data_matrix, normalized_data_matrix: type: matrix n_sentences x sentence_length, 
  # where n_sentences is total # sentences (155 in the dataset supplied to you) and
  # sentence_length is the total length of each sentence (200 in the dataset supplied to you).
  # data_matrix refers to the original data_matrix, and normalized_data_matrix refers to the normalized version (i.e., output from normalize_data)
  # Output: a 155 x 155 matrix of type double containing the updated euclidean distance using the normalized_data_matrix
  # Also generate the plot(s) that were requested in the question and save them to the pdf.
  # Write code here to generate the output requested as well as any plots/analyses requested.
  
  euclidean_distance_norm_data<-calculate_matrix(normalized_data_matrix,"calculate_euclidean")

  plot_distance_euclidean_result<-plot_distance_matrix(euclidean_result)
  
  plot_hist_euclidean_result<-plot_distance_matrix_histogram(euclidean_result,"On Raw Data")
  
  plot_distance_norm_euclidean_result<-plot_distance_matrix(euclidean_distance_norm_data)
  
  plot_hist_norm_euclidean_result<-plot_distance_matrix_histogram(euclidean_distance_norm_data,"On Normalized Data")
  
  ggsave("G8_distance_matrix.pdf", plot=plot_distance_euclidean_result, path=getwd())
  ggsave("G8_hist_distance.pdf", plot=plot_hist_euclidean_result, path=getwd())
  ggsave("G8_distance_matrix_norm.pdf", plot=plot_distance_norm_euclidean_result, path=getwd())
  ggsave("G8_hist_distance_norm.pdf", plot=plot_hist_norm_euclidean_result, path=getwd())
  
  return(euclidean_distance_norm_data)
}

# This function visualizes a distance matrix, with color indicating distance
plot_distance_matrix <- function(distance_matrix) {
  ggplot(data = melt(distance_matrix), aes(x=Var1, y=Var2, fill=value)) + 
    geom_tile() +
    scale_x_discrete(name="Row Number") + scale_y_discrete(name="Row Number") +
    scale_fill_continuous(name="Distance")
}

plot_distance_matrix_histogram <-function(distance_matrix, title){
  ggplot(melt(distance_matrix), aes(x=value)) +
    geom_histogram(color="Black", fill="White") +xlab("Distance") + 
    ggtitle(title) +
    geom_vline(xintercept=mean(distance_matrix), color= "blue", lwd=1, lty="dashed") +
    scale_x_continuous(breaks=seq(min(distance_matrix),max(distance_matrix))) +
    stat_bin(aes(y=..count.., label=..count..), geom="text", vjust=-.5, angle=60, cex=3) 
  
}
