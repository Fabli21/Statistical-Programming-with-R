library(ggplot2)
galaxies
#TASK 1########################################################################
#Create data frame for plotting the data
galaxies_df <- data.frame(Velocity = galaxies)
#Create density plot
ggplot(galaxies_df, aes(x = Velocity)) +
  geom_density(color = "blue", fill = "lightblue") +
  labs(x = "Velocity (km/sec / 1000)",
       y = "Density") + xlim(7,37) +
  theme_minimal()
#Task 2 a) ####################################################################
#Implement E-steps update function (Equation 7)
gammaUpdate <- function(x, mu, sigma, pi) {
#Initialize the length of x and mu
  N <- length(x)
  K <- length(mu)
  
  # Initialize matrices and vectors
  density_matrix <- matrix(0, nrow = N, ncol = K)
  numerator <- matrix(0, nrow = N, ncol = K)
  denominator_vector <- rep(0, N)
  gamma_matrix <- matrix(0, nrow = N, ncol = K)
  
  #Calculate the density values
  for (n in 1:N) {
    for (k in 1:K) {
      density_matrix[n, k] <- dnorm(x[n], mean = mu[k], sd = sigma[k])
    }
  }
  #Calculate the numarotor
  for (n in 1:N) {
    for (k in 1:K) {
      numerator[n, k] <- pi[k] * density_matrix[n, k]
    }
  }
  #Calculate the denominator
  for (n in 1:N) {
    denom_sum <- 0
    for (k in 1:K) {
      denom_sum <- denom_sum + numerator[n, k]
    }
    denominator_vector[n] <- denom_sum
  }
  #Compute gamma (i.e numarotor / denominator)
  for (n in 1:N) {
    for (k in 1:K) {
      gamma_matrix[n, k] <- numerator[n, k] / denominator_vector[n]
    }
  }
  #Return the gamma matrix
  return(gamma_matrix)
}
#Task 2 b) ####################################################################
#Implement M-steps update functions (i.e equation 8, 9 & 10)
#Function to calculate N_k to simplyfy the M-step functions
calculate_Nk <- function(gamma) {
  N_k <- rep(0, ncol(gamma)) 
  for (k in 1:ncol(gamma)) { 
    N_k[k] <- sum(gamma[, k])
  }
  return(N_k)
}
#Equation 8, muUpdate (Update the means))
muUpdate <- function(x, gamma, sigma) {
  N_k <- calculate_Nk(gamma)  
  K <- ncol(gamma)
  mu_new <- rep(0, K) 
  #Update mu for each component
  for (k in 1:K) {
    sum_weighted <- 0
    for (n in 1:length(x)) {
      sum_weighted <- sum_weighted + gamma[n, k] * x[n]
    }
    mu_new[k] <- sum_weighted / N_k[k]
  }
  #Return the updated means
  return(mu_new)
}
#Equation 9, sigmaUpdate (Update the standard deviations)
sigmaUpdate <- function(x, gamma, mu) {
  N_k <- calculate_Nk(gamma)
  K <- length(mu)
  N <- length(x)
  sigma_new <- rep(0, K)
  #Update sigma for each component
  for (k in 1:K) {
    sum_weighted_squared <- 0
    for (n in 1:N) {
      diff <- x[n] - mu[k]
      sum_weighted_squared <- sum_weighted_squared + gamma[n, k] * (diff^2)
    }
    variance_new <- sum_weighted_squared / N_k[k]
    sigma_new[k] <- sqrt(variance_new)
  }
  #Return the updated standard deviations
  return(sigma_new)
}
#Equation 10, piUpdate (Update the mixture proportions)
piUpdate <- function(gamma) {
  N <- nrow(gamma) #Number of data points 
  K <- ncol(gamma) #Number of components
  N_k <- calculate_Nk(gamma) #Calculate N_k
  pi_new <- rep(0, K) #Initialize new pi vector
  #Update pi for each component
  for (k in 1:K) {
    pi_new[k] <- N_k[k] / N
  }
  #Return the updated mixture proportions
  return(pi_new)
}
#Test the functions with initial values (This code is copied from the assignment)
mu = c(10, 20, 30)
sigma = c(2, 2, 2)
probs = c(1/3, 1/3, 1/3)
resp = gammaUpdate(galaxies, mu, sigma, probs)
mu = muUpdate(galaxies, resp, sigma)
sigma = sigmaUpdate(galaxies, resp, mu)
probs = piUpdate(resp)
cat("mu:", mu,
    "\nsigma:", sigma,
    "\nprobs:", probs,
    "\nresp[1,]", resp[1,])
#The code gives the exakt same output as in the assignment
#Task 3 #######################################################################
#Implement log-likelihood function (Equation 6)
loglik <- function(x, pi, mu, sigma) {
  n <- length(x)
  K <- length(pi)
  #Initialize log-likelihood
  log_likelihood <- 0
  #Calculate log-likelihood
  for (i in 1:n) {
    
    marginal_density <- 0
    for (j in 1:K) {
      density_value <- dnorm(x[i], mean = mu[j], sd = sigma[j])
      marginal_density <- marginal_density + pi[j] * density_value
    }
    log_likelihood <- log_likelihood + log(marginal_density)
  }#Return log-likelihood
  return(log_likelihood)
}
#Task 4  ######################################################################
#Initialize parameter estiamtes for EM algorithm (This code is copied from the assignment)
loglik(galaxies, probs, mu, sigma)
initialValues = function(x, K, reps = 100){
  mu = rnorm(K, mean(x), 5)
  sigma = sqrt(rgamma(10, 5))
  p = runif(K)
  p = p/sum(p)
  currentLogLik = loglik(x, p, mu, sigma)
  for(i in 1:reps){
    mu_temp = rnorm(K, mean(x), 10)
    sigma_temp = sqrt(rgamma(10, 5))
    p_temp = runif(K)
    p_temp = p_temp/sum(p_temp)
    tempLogLik = loglik(x, p_temp, mu_temp, sigma_temp)
    if(tempLogLik > currentLogLik){
      mu = mu_temp
      sigma = sigma_temp
      p = p_temp
      currentLogLik = tempLogLik
    }
  }
  return(list("mu" = mu, "sigma" = sigma, "p" = p))
}
#Implement the function that runs the EM algorithm (This code is copied from the assignment)
EM <- function(x, K, tol = 0.001){
  inits <- initialValues(x, K, reps = 1000)
  mu <- inits$mu
  sigma <- inits$sigma
  prob <- inits$p
  #Initial log-likelihood
  logLik_old <- loglik(x, prob, mu, sigma)
  logLik_new <- logLik_old #Initialize new log-likelihood for convergence check
  iter <- 0
  max_iter <- 1000 #Set the maximum number of iterations to prevent infinite loops
  
  # EM-loop
  repeat {
    iter <- iter + 1
    # E-step, calculate responsibilities
    gamma <- gammaUpdate(x, mu, sigma, prob)
    
    # M-step, update parameters
    mu_new <- muUpdate(x, gamma, sigma)
    sigma_new <- sigmaUpdate(x, gamma, mu_new)
    prob_new <- piUpdate(gamma)
    
    #Update each parameter
    mu <- mu_new
    sigma <- sigma_new
    prob <- prob_new
    
    #Calculate new log-likelihood
    logLik_new <- loglik(x, prob, mu, sigma)
    
    #Check for converegence if one of the conditions is met, break the loop
    if (abs(logLik_new - logLik_old) < tol || iter >= max_iter) {
      break
    }
    
    #Update old log-likelihood for next iteration
    logLik_old <- logLik_new
  }

  return(list('loglik' = logLik_new, 'mu' = mu, 'sigma' = sigma, 'prob' = prob))
}

# Task 5 ######################################################################
#The seed that was used in the assignment
set.seed(2024)
#Run the EM algorithm for K = 2 to 5
K_values <- 2:5
results <- list() #To store results for each K
for (K in K_values) {
  results[[as.character(K)]] <- EM(galaxies, K = K, tol = 0.001)
}
# Print log-likelihoods for each K
loglik_values <- rep(0, length(K_values))
for (i in 1:length(K_values)) {
  K <- K_values[i]
  loglik_values[i] <- results[[as.character(K)]]$loglik
}
# Create a data frame to compare log-likelihoods
comparison_df <- data.frame(
  K = K_values,
  LogLikelihood = loglik_values)
#Print the comparison data frame
print(comparison_df)

#Plots ########################################################################
task1_density <- density(galaxies, from = 7, to = 37)
task1_df <- data.frame(x = task1_density$x, y = task1_density$y, source = "Original Density")
#Create an empty list to store plot data for each K
plot_list <- list()
#Generate density data for each K and combine with original density
for (K in 2:5) {
  res <- results[[as.character(K)]]
  
  k_dens <- rep(0, length(task1_df$x))
  
  for (k in 1:K) {
    
    k_dens <- k_dens + res$prob[k] * dnorm(task1_df$x, res$mu[k], res$sigma[k])
  }
  #Create data frame for each density
  k_dens_df <- data.frame(
    x = task1_df$x,
    y = k_dens,
    source = paste("Mixture model, K =", K)
  )
  #Combine with original density
  combined_df <- rbind(task1_df, k_dens_df)
  combined_df$panel <- paste("K =", K)
  #Store in the list
  plot_list[[as.character(K)]] <- combined_df
}
#Combine all plot data into one data frame
all_plot_data <- do.call(rbind, plot_list)
ggplot(all_plot_data, aes(x = x, y = y, color = source)) +
  geom_line(size = 0.8) +
  facet_wrap(~ panel, ncol = 2) +
  labs(
    x = "Velocity (km/sec / 1000)",
    y = "Density"
  ) +
  xlim(7, 37) +
  ylim(0, 0.27) +
  theme_minimal() +
  scale_color_manual(values = c("Original Density" = "blue", 
                                "Mixture model, K = 2" = "red",
                                "Mixture model, K = 3" = "green",
                                "Mixture model, K = 4" = "purple",
                                "Mixture model, K = 5" = "orange"))