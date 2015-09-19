# Implement Batch Gradient Descent and Stochastic Gradient Descent
# y : Output vector of m training examples
# X : feature vector of m training examples
# theta : The initial parameters for n+1 features vector.
# alpha : Learning Rate parameter
# tau : tunning parameter for learning rate in stochastic Linear Regression
# precision : 

#calculate the cost Function
CostFunction <- function(X,y,theta){
  
  # m: no of training examples
  m <- NROW(X)
  
  J <- (1/(2*m)) * t((X %*% theta - y)) %*% (X %*% theta - y)
  
  return(J)
}

#Batch Gradient Descent Function for Linear Regression
BatchGradientDescent <- function(X,y,theta,alpha=0.01,precision=0.00001){
  
  #Intialize the cost function for plotting
  J_history <- matrix(c(0),ncol = 1, nrow = 1)
  bg_itr <- 0
  
  # m: no of training examples
  m <- NROW(X)
  
  #start the gradient descend algorithm
  repeat{
    
    bg_itr <- bg_itr + 1
    
    # hypothesis - given output
    h_error <- as.matrix((X %*% theta) - y)

    #Batch gradient descent
    theta <- theta - ((alpha/m) * as.matrix(t(X) %*% h_error))
    
    #compute the cost for calculated the theta
    J <- CostFunction(X,y,theta)
    #cat("\nCost function : ",J)
    
    #update the Cost function for every iteration
    if(bg_itr == 1)
    {
      J_history[1,1] <- J
      next
    }else
    {
      J_history <- rbind(J_history,J)
    }
    
    CostDiff <- J_history[bg_itr-1,1] - J_history[bg_itr,1]
    #convergence point and loop gets terminated if the below condition satisfies.
    if((CostDiff < precision))
      return(theta)
  }
}

#Stochastic Gradient Descent Function for Linear Regression.
StochasticGradientDescent <- function(X,y,theta,tau=0.000001,precision=0.00001){
  
  #Intialize the cost function for plotting
  J_history <- matrix(c(0),ncol = 1, nrow = 1)
  sg_itr <- 0
  
  # m: no of training examples
  m <- NROW(X)
  
  #start the stochastic gradient descend algorithm
  repeat{
    
    sg_itr <- sg_itr + 1
    
    #set a temporary variable for theta 
    temptheta <- theta
    
    #Set the iterator for stochastic Gradient
    sg_itr2 <- 0
    
    #choose the batch size of 1 and process every training example.
    repeat{
      
      sg_itr2 <- sg_itr2 + 1
      
      #If stepsize alpha decays as O(1/sqrt(itr2)), then SGD asymptotically converge
      #to the true minimizer of J(w)
      alpha <- sqrt(tau/(tau+sg_itr2))
      
      theta <- theta - alpha * as.matrix(X[sg_itr2,]) %*% ((X[sg_itr2,] %*% theta) - y[sg_itr2,])
      
      if(sg_itr2 == m)
        break
    }
    
    #compute the cost for each computed theta
    J <- CostFunction(X,y,theta)
    
    #update the Cost function for every iteration
    if(sg_itr == 1)
    {
      J_history[1,1] <- J
      next
    }else
    {
      J_history <- rbind(J_history,J)
    }
    
    #Check the J(theta) for checking the convergence of the algorithm
    CostDiff <- J_history[sg_itr-1,1] - J_history[sg_itr,1]
    
    if(CostDiff < precision)
      #if(itr == 150000)
    {
      return(theta)
    }
  }
}