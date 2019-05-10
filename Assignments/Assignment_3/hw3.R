########
# HW3 
#########

# Write code for regression here
alda_regression <- function(x_train, x_test, y_train, regression_type){
  # Perform regression (linear/ridge/lasso)
  
  # Inputs:
    # x_train: training data frame(19 variables, x1-x19)
    # x_test: test data frame(19 variables, x1-x19)
    # y_train: dependent variable, training data (vector, continous type)
    # regression_type: specifies type of regression, string variable, can be of type 'linear', 'ridge' or 'lasso'
  
  # General Information:
    # Instructions for specific regression types:
      # linear: no cross validation
      # ridge: use 10-fold cross validation to determine optimal lambda
      # lasso: use 10-fold cross validation to determine optimal lambda
  
  # Output:
    # A list with two elements, first element = model generated, second element = predictions on test data (vector) 
  
  # allowed packages: R-base, glmnet
  
  # Function hints: Read the documentation for the functions glmnet, cv.glmnet, predict
  # Ridge and Lasso regression hints: Lambda is the hyperparameter
  if(regression_type == 'linear'){ 
    # ~ 2-3 lines of code
    # write code for building a linear regression model using x_train, y_train
    # can you use glmnet to do simple linear regression as well?
    # Explore away!  
    # Hint: Think of what the lambda value means for linear regression without regularization.

    #model<-lm( y_train ~ ., data=as.data.frame(cbind(x_train,y_train)))

    model<-glmnet(x_train,y_train, family = "gaussian",lambda=0)
    
    # predict using the model
    #predict <-as.vector(predict(model,newdata=as.data.frame(x_test)))
    predict <-as.vector(predict(model,newx=x_test))
    


  }else if(regression_type == 'ridge'){
    # ~ 2-3 lines of code
    # write code for ridge regression here
    # 10 fold cross validation, with mse (mean squared error) as the measure
    # the hyperparameter you are tuning here is lambda
    
    #we use alpha=0 for ridge
    cv.model <- cv.glmnet(x_train,y_train,alpha=0,nfolds = 10,type.measure = "mse")
    model <-cv.model$glmnet.fit
    # predict on x_test using the model that gives least MSE
    
    predict <-as.vector(predict(model,newx=x_test, s=cv.model$lambda.min))
    
    
  }else{
    # ~ 2-3 lines of code
    # write code for lasso regression here
    # 10 fold cross validation, with mse (mean squared error) as the measure
    # the hyperparameter you are tuning here is lambda
    
    #we use alpha=1 for lasso
    
    cv.model <- cv.glmnet(x_train,y_train,alpha=1,nfolds = 10,type.measure = "mse")
    model <-cv.model$glmnet.fit
    # predict on x_test using the model that gives least MSE
    
    predict <-as.vector(predict(model,newx=x_test, s=cv.model$lambda.min))
  }
  
  result <- list("Model"=model,"Predictions"=predict)
  
  return(result)
  
}

calculate_rmse <- function(y_true, y_pred){
  # DO NOT modify this code. TA has already given you code for this
  # You have already been provided this code to calculate RMSE
  
  # Inputs:
  # y_true: ground truth dependent variable values, of type vector
  # y_pred: prediction outcomes from any regression method, with the same length as y_true
  
  # Outputs:
  # a single value of type double, with the RMSE value
  return(sqrt(sum(y_true - y_pred)^2/length(y_true)))
}

regression_compare_rmse <- function(y_test, linear_regression_prediction, ridge_prediction, lasso_prediction){
  # ~ 8-10 lines of code
  # Calculate the rmse for each of the regression methods: 'linear', 'ridge', 'lasso'
  # Return the best method and its RMSE (i.e., method with least RMSE)
  
  # Inputs:
  # y_test: ground truth dependent variable from test data (vector)
  # linear_regression_prediction: predictions from linear regression (vector)
  # ridge_regression_prediction: predictions from ridge regression (vector)
  # lasso_regression_prediction: predictions from lasso regression (vector)
  
  # Returns:
  # list of three values:
  # First value, of type string, with the name of the best method
  #  'linear' if linear_regression_prediction is best
  #  'ridge' if ridge_prediction is best
  #  'lasso' if lasso_regression is best
  # Second value, of type double, with the corresponding RMSE of the best method (do not round off)
  # third value is a vector of RMSE values, in the following order: c(linear regression's RMSE, ridge regression's RMSE, lasso's RMSE)
  
  # Allowed packages: R-base
  # You are given the implementation for calculate_rmse (see above) 
  
  rmse.linear<-calculate_rmse(y_test,linear_regression_prediction)
  
  rmse.ridge<- calculate_rmse(y_test,ridge_prediction)
  
  rmse.lasso<- calculate_rmse(y_test,lasso_prediction)
  
  rmse.vec <- c(rmse.linear,rmse.ridge,rmse.lasso)
  names(rmse.vec) <- c("linear","ridge","lasso")
  
  best.method=names(rmse.vec)[which.min(rmse.vec)]
  best.rmse=rmse.vec[which.min(rmse.vec)][[1]]
  
  result <- list("BestMethod"=best.method,"RMSE"=best.rmse, "RmseValues"=rmse.vec)
  
  return(result)
}


alda_svm <- function(x_train, x_test, y_train, kernel_name){
  # Perform classification using support vector machines (linear/radial/sigmoid)
  
  # Inputs:
    # x_train: training data frame(4 variables, x1-x4)
    # x_test: test data frame(4 variables, x1-x4)
    # y_train: dependent variable, training data (factor)
    # kernel_name: specifies type of SVM kernel, string variable, can be of type 'linear', 'radial' or 'sigmoid' or 'polynomial'

  # General information
    # Both training data and test data have already been scaled - so you don't need to scale it once again.
  
  # Kernel specific information: using 10-fold cross-validation, perform hyperparameter tuning for each kernel as shown below:
    # Linear: 
      # 'cost' parameter: for the following values: c(0.01, 0.1, 1, 10)
    # radial: 
      # 'cost' parameter: for the following values: c(0.01, 0.1, 1, 10), 
      # 'gamma' parameter: for the following values: c(0.05, 0.5, 1, 2)
    # polynomial:
      # 'cost' parameter: for the following values: c(0.01, 0.1, 1, 10), 
      # 'gamma' parameter: for the following values: c(0.05, 0.5, 1, 2)
      # 'degree' parameter: for the following values: c(1,2,3)
    # sigmoid:
      # 'cost' parameter: for the following values: c(0.01, 0.1, 1, 10), 
      # 'gamma' parameter: for the following values: c(0.05, 0.5, 1, 2)
  
  # Output:
    # A list with two elements, first element = model generated, second element = predictions on test data (factor) 
  
  # Word of caution:
    # Make sure that you pick the best parameters after tuning
  
  # allowed packages: R-base, e1071
  
  # Hints: See if you can use the 'tune' function in e1071 for cross validation
   if(kernel_name == "radial"){
    # ~1-2 lines 
     model.tuned<-tune(svm,train.x=x_train,train.y=y_train,kernel ="radial",
                       ranges=list(cost=c(0.01, 0.1, 1, 10),gamma=c(0.05, 0.5, 1, 2)))
     
     model <-svm(x_train,y_train,kernel ="radial",cost=model.tuned$best.parameters$cost,
                 gamma = model.tuned$best.parameters$gamma, scale=FALSE)
     
     predict<-predict(model,x_test)
     
     
  }else if(kernel_name == 'polynomial'){
    #~1-2 lines
    model.tuned<-tune(svm,train.x=x_train,train.y=y_train,kernel ="polynomial",
                      ranges=list(cost=c(0.01, 0.1, 1, 10),gamma=c(0.05, 0.5, 1, 2),degree=c(1,2,3)))
    
    model <-svm(x_train,y_train,kernel ="polynomial",cost=model.tuned$best.parameters$cost,
                gamma = model.tuned$best.parameters$gamma, degree=model.tuned$best.parameters$degree, 
                scale=FALSE)
    
    predict<-predict(model,x_test)
    
    
  }else if(kernel_name == 'sigmoid'){
    #~1-2 lines
    model.tuned<-tune(svm,train.x=x_train,train.y=y_train,kernel ="sigmoid",
                      ranges=list(cost=c(0.01, 0.1, 1, 10),gamma=c(0.05, 0.5, 1, 2)))
    
    model <-svm(x_train,y_train,kernel ="sigmoid",cost=model.tuned$best.parameters$cost,
                gamma = model.tuned$best.parameters$gamma, scale=FALSE)
    
    predict<-predict(model,x_test)
    
    
  }else{ # default linear kernel
    #~1-2 lines

    model.tuned<-tune(svm,train.x=x_train,train.y=y_train,kernel ="linear",ranges=list(cost=c(0.01, 0.1, 1, 10)))
    
    model <-svm(x_train,y_train,kernel ="linear",cost=model.tuned$best.parameters$cost, scale=FALSE)
    
    predict<-predict(model,x_test)
    
    
  }
  
  result <- list("Model"=model,"Prediction"=as.factor(predict))
  
  return(result)
}


classification_compare_accuracy <- function(y_test, linear_kernel_prediction, radial_kernel_prediction, 
                                            polynomial_kernel_prediction, sigmoid_kernel_prediction){
  # ~ 6-10 lines of code
  # Calculate the accuracy for each of the classification methods: 
    # 'svm-linear': linear kernel SVM
    # 'svm-radial': radial kernel SVM
    # 'svm-poly': polynomial kernel SVM
    # 'svm-sigmoid': sigmoid kernel SVM 
  # Return the best method and its accuracy (i.e., method with highest accuracy)
  
  # Inputs:
    # y_test: ground truth dependent variable from test data (factor)
    # linear_kernel_prediction: predictions from linear kernel SVM (factor)
    # radial_kernel_prediction: predictions from radial kernel SVM (factor)
    # polynomial_kernel_prediction: predictions from polynomial kernel SVM (factor)
    # sigmoid_kernel_prediction: predictions from sigmoid kernel SVM (factor)
    
  # Returns:
  # list of three values:
    # First value, of type string, with the name of the best method, sould be:
      # 'svm-linear' if linear_kernel_prediction is best
      # 'svm-radial' if radial_kernel_prediction is best
      # 'svm-poly' if polynomial_kernel_prediction is best
      # 'svm-sigmoid' if sigmoid_kernel_prediction is best
    # Second value, of type double, with the corresponding overall accuracy of the best method (on a scale of 100, do not round off)
    # third value, a vector with the overall accuracies of all methods in this order: c(linear-svm's accuracy, radial-svm's accuracy, poly-svm's accuracy, sigmoid-svm's accuracy)
  # Allowed packages: R-base
  # Note that I asked you to implement accuracy calculation - do not use a library for this
  
  linear.accuracy <- calculate_accuracy(y_test,linear_kernel_prediction)
  radial.accuracy <-calculate_accuracy(y_test,radial_kernel_prediction)
  poly.accuracy<-calculate_accuracy(y_test,polynomial_kernel_prediction)
  sigmoid.accuracy<-calculate_accuracy(y_test,sigmoid_kernel_prediction)
  
  accuracy.vec <- c(linear.accuracy,radial.accuracy,poly.accuracy,sigmoid.accuracy)
  names(accuracy.vec) <- c("svm-linear","svm-radial","svm-poly","svm-sigmoid")
  
  best.method=names(accuracy.vec)[which.max(accuracy.vec)]
  best.accuracy=accuracy.vec[which.max(accuracy.vec)][[1]]
  
  result <- list("BestMethod"=best.method,"BestAccuracy"=best.accuracy,"AccuracyVector"=accuracy.vec)
}

calculate_accuracy <- function(y_pred, y_true){
  
  confusion.matrix<-table(y_pred, y_true)
  
  accuracy <- sum(diag(confusion.matrix))/sum(confusion.matrix)
  
  return(accuracy)
  
}

