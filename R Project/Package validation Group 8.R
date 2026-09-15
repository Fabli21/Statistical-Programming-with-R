library(mvtnorm)
library(ggplot2)
library(scales)
library(patchwork)

#################################################################################
# Code for the package validation of mvtnrom more specifically function rmvt 
#################################################################################


returns_only <- dailylogreturns_df_dataset[, -1]

# We begin with identify the variables that will be used in the simulation 
# the df parameter is from the MCME 
# The choiche of n = 100 is the see the pattern of vol for different sizes n
# The choice of 1000 iterations was due to an trade off between relaibale results
# and computing time

nu <- 5.967593 
scaling_factor <- nu / (nu - 2) 
portfolio_sizes <- 1:100 
n_iterations <- 1000
n <- 100
avg_daily_vol <- mean(sqrt(diag(cov(returns_only)))) 
daily_var <- avg_daily_vol^2
avg_annual_vol <- avg_daily_vol * sqrt(252)

# We build a function to redo the simulation for different sizes of rho 
#(for this case rho = 1 or rho = 0, two extreme cases with known theoretical results) 
# without repeating in the code "DRY"
run_portfolio_sim <- function(rho, sizes, iterations, p_total, daily_var, nu, scaling_factor) {
  # Construct the covariance matrix under the known conditions for homogenus stocks
  mat <- matrix(rho * daily_var, nrow = p_total, ncol = p_total)
  diag(mat) <- daily_var
  
  # We ddjust the covariance matrix to the scale matrix for the input to match the 
  # function rmvt
  
  adj_sigma <- mat / scaling_factor
  results <- data.frame(N = sizes, average_vol = NA)
  
  # Start the loop, this loop creates a n*n scale matrix based on the input 
  # for the current size n and then simulate daily returns for one 
  # trading year (252 days) from rmvt and calculates the (annualized) portfolio standard devation 
  # (volatiltiy) and replicates this procedure 1000 times for each n up to 100
  # later the avarege is stored in the data frame  
  for(i in seq_along(sizes)) {
    
    current_n <- sizes[i]
    temp_vols <- replicate(iterations, {
      
      sub_sigma <- adj_sigma[1:current_n, 1:current_n, drop = FALSE]
      sim_returns <- mvtnorm::rmvt(n = 252, sigma = sub_sigma, df = nu)
      sd(rowMeans(sim_returns)) * sqrt(252)
    })
    
    results$average_vol[i] <- mean(temp_vols)
  }
  return(results)
}

# We know use the function with where rho is adjusted
# Note that this simulation took us around 5 minutes to compile
set.seed(2026)
res_rho1 <- run_portfolio_sim(1, portfolio_sizes, n_iterations, n, daily_var, nu, scaling_factor)
res_rho0 <- run_portfolio_sim(0, portfolio_sizes, n_iterations, n, daily_var, nu, scaling_factor)


# Know we want to visualize the pattern of the decay in portfolio size depending on 
# the number of stocks in portfolio under known conditions here scales is used
# to get an easier interpretaion of the volatility

p1 <- ggplot(res_rho1, aes(x = N, y = average_vol)) +
  geom_line(color = "red", linewidth = 1) +
  scale_y_continuous(labels = label_percent(), limits = c(0, max(res_rho1$average_vol) * 1.1)) +
  labs(title = "ρ = 1", x = "Stocks in portfolio (n)", y = "Volatility (σ)") + theme_minimal()

p2 <- ggplot(res_rho0, aes(x = N, y = average_vol)) +
  geom_line(color = "darkgreen", linewidth = 1) +
  scale_y_continuous(labels = label_percent(), limits = c(0, max(res_rho1$average_vol) * 1.1)) +
  labs(title = "ρ = 0", x = "Stocks in portfolio (n)", y = "Volatility (σ)") + theme_minimal()

# Now we compare the scenarios next to eachother
p1 + p2


# To not just make an visuall inspection of the result and to get a numerical result
# That we can use to validate package we also construct a table for different 
# portfolio sizes 

n_values <- c(1, 10, 50, 100) 

# We initilaize a data frame for the comparission. Note that the functions 
# that computes the numerical values of theo_rho1 and theo_rho0 is presented in the report
# in section 5.2.2 "Theoretical Benchmarks"

validation_table <- data.frame(
  n = n_values,
  sim_rho1  = numeric(length(n_values)),
  theo_rho1 = avg_annual_vol,
  sim_rho0  = numeric(length(n_values)),
  theo_rho0 = avg_annual_vol / sqrt(n_values)
)

# Fill the table for the simulated values
for(k in seq_along(n_values)) {
  n_curr <- n_values[k]
  validation_table$sim_rho1[k] <- res_rho1$average_vol[res_rho1$N == n_curr]
  validation_table$sim_rho0[k] <- res_rho0$average_vol[res_rho0$N == n_curr]
}

# The result is presented in the table. As can be seen is the simulated values
# are very close to the theoretical, which make us conclude that the
# package mvtnorm and function rmvt is robust and is suited for the objective 
# for this report

print(round(validation_table, 3))
