# Implementation of Normal Equation for linear regression
# y : Output vector of m training examples
# X : feature vector of m training examples

#Linear Regression computation using normal Equation
LRegressionNEqn <- function(X,y){
  
  #compute the theta using the normal equation
  theta <- solve(t(X) %*% X) %*% t(X) %*% y
  
  return(theta)
}