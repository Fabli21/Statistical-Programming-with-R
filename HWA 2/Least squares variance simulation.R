olsFun <- function(data){
  y <- as.matrix(data[, 1]) # Dependent variable from the first column
  x <- data[, 2] # Chooses the independent variable from the second column
  X <- as.matrix(cbind(1, x)) # Adds column of ones to the matrix to estimate 
  #intercept
  
  #We calculate XtX
  
  XtX <- t(X) %*% X
  
  #We calculate inverse of XtX
  XtX_inv <- solve(XtX)
  
  #We calculate XtY
  XtY <- t(X) %*% y
  
  #We calculate beta_ols
  beta_ols <- XtX_inv %*% XtY
  
  return(beta_ols[2]) #We return the coefficient 
}

wlsFun <- function(data, lambda){
  y <- as.matrix(data[, 1]) # Dependent variable, same as last function
  x <- data[, 2] # Independent variable, same as last function
  X <- as.matrix(cbind(1, x)) # Intercept, same as last function
  
  #Lambda has to be numeric, else stop
  if(!is.numeric(lambda)){
    stop("lambda has to be a numeric value")
  }
  
  #Variance for each observation
  variance_i <- exp(x * lambda)
  
  #Create diagonal weight matrix W (Omega^-1) Since omega is the diagonal
  #the inverse is:
  W <- diag(1 / variance_i)
  
  #X' W X
  XtWX <- t(X) %*% W %*% X
  
  #Inverse of X' W X
  XtWX_inv <- solve(XtWX)
  
  #X' W y
  XtWy <- t(X) %*% W %*% y
  
  #Finally we calculate the WLS estimator
  beta_wls <- XtWX_inv %*% XtWy
  
  #Return the coefficient 
  return(beta_wls[2])
  
}

fwlsFun <- function(data, trueVar){
  y <- data[, 1] # Dependent variable
  x <- data[, 2] # Independent variable
  X <- as.matrix(cbind(1, x)) # Same as last function
  
  #OLS estimation
  XtX_inv <- solve(t(X) %*% X)
  beta_ols <- XtX_inv %*% t(X) %*% y
  
  #Residuals
  u_hat <- y - X %*% beta_ols
  u_hat_sq <- u_hat^2
  
  #Lambda estimation
  
  X_lambda <- as.matrix(cbind(1, x))
  ln_u_hat_sq <- log(u_hat_sq)
  
  #Beta lambda
  beta_lambda <- solve(t(X_lambda) %*% X_lambda) %*% t(X_lambda) %*% ln_u_hat_sq
  lambda_hat <- beta_lambda[2]
  
  #Lambda has to be numeric, else stop
  if(!is.numeric(lambda_hat)){
    stop("lambda_hat has to be a numeric value")
  }
  
  #Calculating variance with the correct and false form
  if(trueVar == TRUE){
    variance_i <- exp(x * lambda_hat)
  } else {
    variance_i <- (1 + x * lambda_hat)
  }
  #Omega inverse
  W <- diag(1 / variance_i)
  
  #FWLS estimator
  XtWX <- t(X) %*% W %*% X
  XtWX_inv <- solve(XtWX)
  XtWy <- t(X) %*% W %*% y
  
  #Beta FWLS
  beta_fwls <- XtWX_inv %*% XtWy
  
  #Return results
  return(beta_fwls[2])
}
  
#Test that we recive the same results as the function
testData <- cbind( c(0.62, 0.18, 3.92, 0.80, -5.15),
                   c(0.44, 1.49, 0.69, 0.13, 1.90) )

c(olsFun(data = testData),
  wlsFun(data = testData, lambda = 2),
  fwlsFun(data = testData, trueVar = TRUE),
  fwlsFun(data = testData, trueVar = FALSE), use.names = F) 
  
DataFun <- function(n, lambda) {
  #x_i ~ U(0, 2)
  x <- runif(n, min = 0, max = 2)
  #Heteroskedastic standard deviation
  sigma_i <- sqrt(exp(lambda * x)) 
  #Error term u_i
  epsilon <- rnorm(n, mean = 0, sd = sigma_i)
  #Beta set to 2
  beta <- 2
  #y_i
  y_i <- beta * x + epsilon
  #Combine y and x to data matrix
  data <- cbind(y_i, x)
  return(data)
}

SimFun <- function(n, sim_reps, seed, lambda) {
  # Set seed
  set.seed(seed)
 
  #Storage for estimates
  ols_est <- numeric(sim_reps)
  wls_est <- numeric(sim_reps)
  fwls_true_est <- numeric(sim_reps)
  fwls_false_est <- numeric(sim_reps)
  
  #Simulation loop that uses data from DataFun
  for (i in 1:sim_reps) {
    data <- DataFun(n, lambda)
    ols_est[i] <- olsFun(data = data)
    wls_est[i] <- wlsFun(data = data, lambda = lambda)
    fwls_true_est[i] <- fwlsFun(data = data, trueVar = TRUE)
    fwls_false_est[i] <- fwlsFun(data = data, trueVar = FALSE)
  }
  
  
  #We extract the variances of the estimates from the simulation
  var_results <- c(var_ols = var(ols_est),
     var_wls = var(wls_est),
     var_fwls_true = var(fwls_true_est),
     var_fwls_false = var(fwls_false_est)
  )
  
  return(var_results)
}

#Run the simulations and store the results
simresult_25 <- SimFun(n = 25, sim_reps = 1000, seed = 2024, lambda = 2)
simresult_50 <- SimFun(n = 50, sim_reps = 1000, seed = 2024, lambda = 2)
simresult_100 <- SimFun(n = 100, sim_reps = 1000, seed = 2024, lambda = 2)
simresult_200 <- SimFun(n = 200, sim_reps = 1000, seed = 2024, lambda = 2)
simresult_400 <- SimFun(n = 400, sim_reps = 1000, seed = 2024, lambda = 2)

#Combine the results from simulation in a dataframe for easier plotting
results_df <- data.frame(N = c(25, 50, 100, 200, 400), 
                         var_OLS = c(simresult_25[1], 
                                     simresult_50[1], 
                                     simresult_100[1], 
                                     simresult_200[1], 
                                     simresult_400[1]),
                         var_WLS = c(simresult_25[2], 
                                     simresult_50[2], 
                                     simresult_100[2], 
                                     simresult_200[2], 
                                     simresult_400[2]),
                         var_FWLS_True = c(simresult_25[3], 
                                           simresult_50[3], 
                                           simresult_100[3], 
                                           simresult_200[3], 
                                           simresult_400[3]),
                         var_FWLS_False = c(simresult_25[4], 
                                            simresult_50[4], 
                                            simresult_100[4], 
                                            simresult_200[4], 
                                            simresult_400[4]))

library(tidyr)
library(ggplot2)

# Reshape data for plotting a line plot with ggplot format
#we want a long format, all columns except N to be gathered
plot_data <- results_df %>%
  pivot_longer(cols = starts_with("var_"), #Select all 
               #columns starting with "var_"
               names_to = "Estimator", #New column for estimator names
               values_to = "Variance") #New column for variance values

#Finally we plot the data
ggplot(plot_data, aes(x = N, y = Variance, color = Estimator)) +
  geom_line() + # Draws line between the points
  geom_point() + #Dots at each variance estimate for each N
  scale_x_continuous(breaks = c(25, 50, 100, 200, 400)) + #Breaks for the 
  #simulation sizes
  labs(x = "Sample Size (N)",
       y = "Variance Estimate") +
  theme_minimal()



  
  
  
  
  

