source("grad_assign1_solution.R")
source("neqn_assign1_solution.R")
source("wght_assign1_solution.R")

require(MASS)
data(Boston)

# load the RM and MEDV vector in the matrix data
data <- matrix(c(Boston$rm,Boston$medv), ncol = 2)

# plot scatter plot of the data 
X <- data[,1]
y <- data[,2]

# m: no of training examples
# n: no of feature vectors
m <- NROW(X)
n <- NCOL(X)

#Draw the scatter plot of the RM and MEDV
plot(X,y,main = "Boston Housing Data Regression Line Plot.", xlab="Avg No. of Rooms per dwelling",
     ylab="Median Value of house(in $1000's)",pch = "+")

# Add bias vector to the feature vector
X <- as.matrix(cbind(1,X))
y <- as.matrix(y)

# Initializes the fitting parameters
theta <- matrix(c(rep(0,n+1)), nrow = n+1, ncol = 1)

#learning rate parameter
alpha <- 0.01

#set precision
precision <- 0.0000001

####################################################################
###########For testing the weighted linear regression algorithm#####
#Set the min and max value of the feature varaible x1
xmin <- min(X[,2])
xmax <- max(X[,2])

#set intv intervals and calculate interval size
intv <- 500
intv_size <- (xmax - xmin)/intv

#set tau  and predicted output parameter
tau <- 1
h1 <- as.matrix(rep(0,intv))
h2 <- as.matrix(rep(0,intv))
h3 <- as.matrix(rep(0,intv))

#generate the x values in the intv intervals. 
x <- as.matrix(seq(xmin + intv_size/2,xmax, intv_size))

itr<- 0

repeat{
  itr <- itr+1
  wgt_theta1 <- LWghtLinearRegression(X,y,x[itr],0.1)
  wgt_theta2 <- LWghtLinearRegression(X,y,x[itr],0.5)
  wgt_theta3 <- LWghtLinearRegression(X,y,x[itr],1)
  
  h1[itr] <- matrix(c(1,x[itr]), ncol = 2) %*% wgt_theta1 
  h2[itr] <- matrix(c(1,x[itr]), ncol = 2) %*% wgt_theta2
  h3[itr] <- matrix(c(1,x[itr]), ncol = 2) %*% wgt_theta3
  
  #Weight calculation for all the x values completed
  if(itr == intv)
    break
}
#############################################################

BGD_theta <- BatchGradientDescent(X,y,theta,alpha,precision)

SGD_theta <- StochasticGradientDescent(X,y,theta,0.000001,precision)

NEqn_theta <- LRegressionNEqn(X,y)

cat("The Optimal Values of theta for BGD: ",BGD_theta, "Cost = ", 2*CostFunction(X,y,BGD_theta))
cat("The Optimal Values of theta for SGD: ",SGD_theta, "Cost = ", 2*CostFunction(X,y,SGD_theta))
cat("The Optimal Values of theta for NEqn: ",NEqn_theta, "Cost = ", 2*CostFunction(X,y,NEqn_theta))

reg <- lm(y~X[,2])
#abline(reg, col="Red", lwd=3)
abline(BGD_theta, col="Green", lwd=2)
abline(SGD_theta, col="pink", lwd=2)
abline(NEqn_theta, col="orange", lwd=2)
par(new = TRUE)
plot(x,h1, type = "l", ylim = range(c(y,h)), axes = FALSE, col = 2, lwd=2 )
par(new = TRUE)
plot(x,h2, type = "l", ylim = range(c(y,h)), axes = FALSE, col = 6, lwd=2 )
par(new = TRUE)
plot(x,h3, type = "l", ylim = range(c(y,h)), axes = FALSE, col = 6, lwd=2 )
legend("topleft", legend=c("Wgt tau=0.1","Wgt tau=0.5","Wgt tau=1.0","Batch", "Stochastic","Normal Equation"),
       col=c(2,4,6,"Green","pink","orange"),pch=20,cex=0.75)
