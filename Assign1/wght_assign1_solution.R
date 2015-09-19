# Implementation of Weighted Linear Regression using Normal Equation
# y : Output vector of m training examples
# X : feature vector of m training examples
# x : The single data point where value of theta is required from normla equation.
# tau : tunning parameter for weight matrix calculation for m training examples.

#Weighted Normal Equation Function
LWghtLinearRegression <- function(X,y,x,tau=0.5){
    
    # m: no of training examples
    m <- NROW(X)
    
    # n: no of feature vectors including bias point
    n <- NCOL(X)
    
    #calculate the weight matrix for m training examples
    xdiff_vec <- X[,2] - as.matrix(rep(x,m))
    
    #generate the weight matrix of size mxm
    W <- diag(as.vector(exp(-xdiff_vec^2/(2*tau^2))),m,m)
    
    theta <- solve(t(X) %*% W %*% X, tol = 1e-25) %*% t(X) %*% W %*% y
    
    #return the calculated theta for given x
    return(theta)
}