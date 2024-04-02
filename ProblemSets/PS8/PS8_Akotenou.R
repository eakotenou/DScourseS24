# Install necessary packages if not already installed
if (!requireNamespace("nloptr", quietly = TRUE)) {
  install.packages("nloptr")
}
if (!requireNamespace("modelsummary", quietly = TRUE)) {
  install.packages("modelsummary")
}
library(nloptr)
library(modelsummary)

#4-a Set seed for reproducibility
set.seed(100)

#4-b,c,d Generate data
N <- 100000
K <- 10
X <- matrix(rnorm(N * K), nrow = N)
X[,1] <- 1
eps <- rnorm(N, mean = 0, sd = 0.5)
beta <- c(1.5, -1, -0.25, 0.75, 3.5, -2, 0.5, 1, 1.25, 2)
Y <- X %*% beta + eps

#%5- Compute OLS estimate using closed-form solution
beta_ols_closed <- solve(t(X) %*% X) %*% t(X) %*% Y
print(beta_ols_closed)

#6- Compute OLS estimate using gradient descent
beta_ols_gd <- rep(0, K)
learning_rate <- 0.0000003
for(i in 1:100000) {
  grad <- -2 * t(X) %*% (Y - X %*% beta_ols_gd)
  beta_ols_gd <- beta_ols_gd - learning_rate * grad
}
print(beta_ols_gd)

# Define objective function
objective <- function(beta) {
  sum((Y - X %*% beta)^2)
}

#7- Compute OLS estimate using nloptr (L-BFGS)
# Define gradient function
gradient <- function(beta) {
  -2 * t(X) %*% (Y - X %*% beta)
}

# Compute OLS estimate using nloptr with L-BFGS algorithm
result_lbfgs <- nloptr(x0 = rep(0, K), 
                       eval_f = objective,
                       eval_grad_f = gradient,
                       opts = list("algorithm" = "NLOPT_LD_LBFGS", "xtol_rel" = 1.0e-6, "maxeval" = 1e3))
print(result_lbfgs$solution)

#7-b Compute OLS estimate using nloptr (Nelder-Mead)
result_nm <- nloptr(x0 = rep(0, K), 
                    eval_f = objective, 
                    opts = list("algorithm" = "NLOPT_LN_NELDERMEAD","xtol_rel"=1.0e-6,"maxeval"=1e3))
print(result_nm$solution)

#8 Compute MLE estimate using nloptr (L-BFGS)
objfun  <- function(theta,y,X) {
  beta    <- theta[1:(length(theta)-1)]
  sig     <- theta[length(theta)]
  loglike <- -sum( -.5*(log(2*pi*(sig^2)) + ((y-X%*%beta)/sig)^2) ) 
  return (loglike)
}
gradient <- function (theta,y,X) {
  grad     <- as.vector(rep(0,length(theta)))
  beta     <- theta [1:(length(theta)-1)]
  sig      <- theta [length(theta)]
  grad[1:(length(theta)-1)] <- -t(X)%*%(y - X%*%beta)/(sig^2)
  grad[length(theta)]       <- dim(X)[1]/sig-crossprod (y-X%*%beta)/(sig^3)
  return ( grad )
}
theta0 <- runif(dim(X)[2]+1) #start at uniform random numbers equal to number of coefficients
options <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-6,"maxeval"=1e4)
result <- nloptr( x0=theta0,eval_f=objfun,eval_grad_f=gradient,opts=options,y=Y,X=X)
print(result)
betahat  <- result$solution[1:(length(result$solution)-1)]
sigmahat <- result$solution[length(result$solution)]

# Check solution
print(summary(lm(Y ~ X - 1))) 

#9 Compute OLS estimate using lm()
ols_lm <- lm(Y ~ X - 1)
# Export regression output to .tex file using modelsummary
library(modelsummary)
modelsummary(ols_lm, output = "latex", file = "regression_output.tex")