install.packages(c("mvtnorm", "ggplot2", "tidyquant", "ggridges", 
                   "patchwork", "readr", "fitHeavyTail", "zoo", "dplyr"))

library(mvtnorm)
library(ggplot2)
library(dplyr)
library(ggridges)
library(readr)
library(fitHeavyTail)
library(zoo)
library(MVT)
library(readr)

# -----------------------------------------------------------------------------

# Data loading and preparation

# Here the log returns data is loaded 
# and the date column is removed, however, since a time series will be used
# later in the results for the empirical analysis it is loaded in a vector. 
# This is the main dataset that is used for the mvt model and the empirical 
# evaluation later on. The covariance matrix is the main input for the mvt simulation

dailylogreturns <- read_csv("/Users/williamjarkander/Desktop/R programming/Project /dailylogreturns.csv")

returns_df_clean <- dailylogreturns[, -1]

cov_matrix <- cov(returns_df_clean)

dates_vector <- as.Date(dailylogreturns[[1]], format = "%m/%d/%y")

# We estimate the degrees of freedom used in the model with the package
# fitHeavyTail which uses maximum likelihood estimation (MLE) to find the best fit
# we will use the specific method "ECME" that is discussed in section 4.3
# As described in section 4.2 the adjusted cov matrix is used to 
# account for t-distribution scaling to match historical data,
# this is necessary input for mvt package  along with df and number of 
# trading days (252). For the simulation asset sizes 1 to 100 are used along 
# with 1000 iterations for each size

df_estimation <- fit_mvt(returns_df_clean, nu = "iterative", nu_iterative_method = "ECME")

nu <- df_estimation$nu

scaling_factor <- nu / (nu - 2)

adjusted_sigma <- cov_matrix / scaling_factor

# -----------------------------------------------------------------------------

# Degrees of freedom estimation

# To validate the fit of our data to the MVT model
# with our estimated degrees of freedom, we will create QQ-plots of
# the squared Mahalanobis distances against the theoretical quantiles of the F-distribution
# which is the theoretical benchmark for the multivariate t-distribution.
# The Mahalanobis distance is used since we are dealing with multivariate data.
# for chosen sampled asset sizes between 1 and 100.
# For each portfolio size N, we calculate the squared Mahalanobis distances d^2 from
# the center (location vector which is set to 0) which is then weighted by the scale matrix

# The F-distribution is defined in 
# theoretical = qf(ppoints(nrow(portfolio_data)), df1 = N, df2 = nu), this
# is our reference point for the QQ-plot. Here,
# ppoints generates n evenly spaced points between 0 and 1 based on the 
# number of observations (nrow(portfolio_data)). Then qf calculates the 
# corresponding quantiles from the F-distribution with degrees of freedom
# df1 = N (number of assets) and df2 = nu (estimated degrees of freedom from data).

# observed = sort(d2 / N) represents the empirical data, 
# we divide by n (number of stocks) to normalize the Mahalanobis distances.
#d^2/N will theoretically follow an F-distribution under the MVT assumption.
# Sort is needed since the QQ plot should compare the n-th smallest 
# against the n-th theoretical quantile

# Since we are analyzing asset sizes 1 to 100, we will create QQ-plots for sampled 
# portfolio sizes 15, 30, 50, and 100 to see how well these fit our model

chosen_asset_sizes <- c(15, 30, 50, 100)

data_matrix <- as.matrix(returns_df_clean)

results_qq <- data.frame()

set.seed(123)

# Loop over each chosen asset size
for (N in chosen_asset_sizes) {
  
  chosen_stocks <- sample(1:ncol(data_matrix), N)
  portfolio_data <- data_matrix[, chosen_stocks]
  scale_subset <- adjusted_sigma[chosen_stocks, chosen_stocks]
  
  d2 <- mahalanobis(portfolio_data, center = rep(0, N), cov = scale_subset)
  
  # Here the data frame is created with observed and theoretical
  # quantiles directly with rbind
  results_qq <- rbind(results_qq, data.frame(
    observed = sort(d2 / N),
    theoretical = qf(ppoints(nrow(portfolio_data)), df1 = N, df2 = nu),
    PortfolioSize = paste("N =", N)
  ))
}

# Since we had some issues with ggplot ordering the sizes in wrong
# order we created a factor variable with the correct order so it works

ordered_levels <- c("N = 15", "N = 30", "N = 50", "N = 100")

results_qq$PortfolioSize <- factor(results_qq$PortfolioSize, levels = ordered_levels)


ggplot(results_qq, aes(x = theoretical, y = observed)) +
  geom_point(alpha = 0.4, color = "steelblue", size = 0.8) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  scale_y_continuous(limits = c(0, 45)) + 
  scale_x_continuous(limits = c(0, 30)) +
  facet_wrap(~PortfolioSize) + # Facet by portfolio size
  labs(
    x = "Theoretical F-quantiles",
    y = "Observed distances (d²/p)")

# -----------------------------------------------------------------------------

# Parameters and settings

# Since the multivariate t distribution was chosen as the main model the 
# mvt package was used for this. Therefore, the estimated degrees of freedom
# from the exploratory part is needed along with the adjusted covariance matrix.
# We set the remaining parameters for the simulation

nu <- nu

sizes <- c(1:100) 

n_iterations <- 1000 

#------------------------------------------------------------------------------

# Main MVT model

# Now for the main simulation MVT model. The. simulation consists of two loops,
# one for switching between asset sizes and the other one for 
# creating the simulated daily returns for 1 year (252 trading days) and
# draw random returns from the multivariate t-distribution. These 252 trading days
# are drawn 1000 times for each asset size, ensuring stability and robustness. 
# The annualized volatility is then calculated for each iteration through the
# mean daily return of an equally weighted portfolio. 

# Store the final results from sim in a df
result_final <- data.frame(N = sizes, 
                           average_vol = NA) 
set.seed(2025)

all_sim_obs <- list() 

for(i in 1:length(sizes)) {
  current_portfolio <- sizes[i] 
  iteration_vol <- numeric(n_iterations)
  for(j in 1:n_iterations) {

    chosen_stocks <- sample(1:ncol(adjusted_sigma), 
                            size = current_portfolio)
    
    # Cut out the sub-covariance matrix that only contains the chosen stocks
    sub_cov_matrix <- adjusted_sigma[chosen_stocks, 
                                     chosen_stocks, 
                                     # drop = FALSE to keep matrix 
                                     # format even if aseet size 1, 
                                     # without this the simulation crashed
                                     drop = FALSE] 
    
    # Main simulation step, MVT draws for 252 trading days
    sim_ret <- rmvt(n= 252, sigma = sub_cov_matrix, df= nu) 
    
    portfolio_return <- rowMeans(sim_ret)
    
    # Annualized volatility calculation for the current iteration
    iteration_vol[j] <- sd(portfolio_return)*sqrt(252)
  }
  
  # Calculate mean average annualized volatility for the current portfolio size 
  result_final$average_vol[i] <- mean(iteration_vol)
  
  all_sim_obs[[i]] <- data.frame(
    N = current_portfolio,
    volatility = iteration_vol
  )
  
}

# Fix data for ridge plot later on
full_ridge_data <- bind_rows(all_sim_obs) 

# ------------------------------------------------------------------------------

# Theoretical Risk Floor Calculation

# After the simulation is done, we can calculate the theoretical risk floor
# based on the square root of the average covariance between assets. This risk floor represents
# the lowest possible risk that can be achieved through diversification

avg_cov <- mean(cov_matrix[upper.tri(cov_matrix, 
                                     diag = FALSE)]) # exclude the diagonal (variances)

sys_risk_floor <- sqrt(avg_cov) * sqrt(252)

# --------------------------------------------------------------------------------

# Results and visualization

# Ridge plot

# Since the mvt simulation draws 252 trading days for each asset size 1000 
# times we can illustrate the variability in the main annualized volatility 
# estimate. This was done through a ridge plot using the ggridges package in R.
# It was done for sizes 1, 5, 25, 50 and 100. The data was then extracted
# from the full simulation results and filtered for the chosen asset sizes.

chosen_n <- c(1, 5, 10, 20, 50, 100) 

ridge_filtered <- full_ridge_data[full_ridge_data$N %in% chosen_n, ]

# turn into a factor variable to ensure correct ordering in the plot
ridge_filtered$factor_levels <- factor(ridge_filtered$N, levels = chosen_n)

ridge_plot <- ggplot(ridge_filtered, 
                     aes(x = volatility,
                         y = factor_levels,  
                         fill = factor_levels)) + # fill is also N to get different colors for each N
  
  # We use geom_density_ridges to create multiple density plots
  geom_density_ridges( 
    scale = 1,  # determines the height of the peaks          
    alpha = 0.6, # determines the transparency   
    quantiles = 2, # median
    quantile_lines = TRUE, # draws out the line for the median
    rel_min_height = 0.01 # we chose a value that cuts off outliers at a fair
    # point for the data
  ) + 
  
  # Adjust scaling to better fit volatility
  scale_x_continuous(
    labels = scales::percent, 
    limits = c(0, 1) 
  ) +
  
  theme_minimal() + 
  labs(x = "Annualized Portfolio Volatility (%)", 
       y = "Number of assets (n)"
  )

print(ridge_plot)
# ----------------------------------------------------------------------------------

# Convergence point for 95% risk reduction and achieved diversification

# Now, for the analysis we want fo find a threshold where 95% of the unsystematic risk (diversifiable)
# has been eliminated. This was done to illustrate a point where the marginal 
# benefit of adding even more stocks became negligible in terms of risk reduction.
# After that, we also wanted to illustrate the rate of decrease in unsystematic risk
# towards the systematic risk as the number of assets increased
# this was plotted through a line plot with the theoretical risk floor plotted
# together with number of assets and mean annualized volatility for each asset size


# To find the convergence point, the following calculations were made:

# First, find the average volatility for portfolio with asset size N=1
# this represents represents the total volatility, both unsystematic and systematic, 
# before any diversification has occurred. 

average_vol_n1 <- result_final$average_vol[1]

# By taking this volatility minus the systematic risk floor we
# can get the total diversifiable risk
total_divers_risk <- average_vol_n1 - sys_risk_floor

# Then we can define the convergence threshold 
# which we choose to be 95% reduction in diversifiable risk
convergence_threshold <- sys_risk_floor + (0.05 * total_divers_risk)

all_achieved <- which(result_final$average_vol <= convergence_threshold)

# Take the first asset size that achieves the threshold of 95% risk reduction
n_conv_95 <- result_final$N[all_achieved[1]]

print(n_conv_95)

# Line plot

ggplot(result_final, aes(x = N, y = average_vol)) +
  
  # Draw the average volatility line as the blue line
  geom_line(color = "blue", linewidth = 1.2) +
  
  # Draw the theoretical risk floor as the red line 
  geom_hline(yintercept = sys_risk_floor, 
             linetype = "dashed", 
             color = "red") +
  
  # Draw a horizontal line for the convergence threshold 
  # to mark the volatility level
  geom_hline(yintercept = convergence_threshold, 
             linetype = "dotted", 
             color = "darkgreen") +
  
  # Draw vertical line at convergence point
  geom_vline(xintercept = n_conv_95, 
             color = "darkgreen", 
             alpha = 0.5, 
             linewidth = 1) + 
  
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, 0.35) # we cut off at 35% to focus on the relevant area
  ) +
  
  scale_x_continuous(limits = c(0, 100), 
                     expand = c(0, 2)) + # makes the plot start exactly at 0
  # and add some space at the end
  
  labs(x = "Number of assets (n)", y = "Annualized Volatility (%)") +
  theme_minimal() 

# Now, we want to show achieved diversification for chosen asset sizes.
# The sizes, 1, 5, 25, 50 and 100 were chosen to illustrate the 
# achieved eliminated unsystematic risk, along with their 
# average annualized volatility from the simulation. The results
# were then illustrated in a table

divers_table <- result_final[result_final$N %in% c(1, 5, 25, 50, 100), ]

divers_table$achieved_diversification <- (average_vol_n1 - divers_table$average_vol) / total_divers_risk * 100

print(divers_table)

# ----------------------------------------------------------------------------------------------

# Emphirical analysis of N=25 vs N=1

# Finally, after the found threshold where 95% of the unsystematic was had been
# eliminated was found at n = 25, we wanted to further analyze this asset size
# through a more empirical approach. This was done through a Monte Carlo simulation 
# from the original data with rolling volatility and rolling correlation 
# calculations using a rolling window of 120 days for a good balance between 
# responsiveness and stability. One stock was then drawn from the 25 sampled 
# assets to compare, and get a stock from the same "universe". The rolling volatility 
# for both N=25 and N=1 was then plotted in a time series plot, comparing the diversification 
# benefit through the chosen 10 year period. To illustrate the uncertainty in 
# having only one stock, the 10th and 90th percentiles were calculated for 
# N=1 and shown as a shaded area around the average line. The rolling correlation 
# was used for a regime classification which will be explained further down below.

# The rolling pairwise correlation part proved to be a bit difficult since the
# standard rollapply function only sees one column at a time. However,
# for the correlation, we need the whole 25-column matrix for each rolling window
# Therefore, by forcing the function to look at the entire 25x25 matrix
# through function(i) { cor_matrix <- cor(sub_25[i, ]), we could extract the
# unique pairwise correlations for the current window and take the mean. 

# To calculate the rolling volatility and correlation, the package 
# zoo was used for its rollapply function. 

num_iterations <- 500 
num_of_stocks <- 25 
window_size <- 120  

# Prepare matrices to store results from each iteration
vol_25 <- matrix(NA, nrow = nrow(returns_df_clean), ncol = num_iterations)
vol_1  <- matrix(NA, nrow = nrow(returns_df_clean), ncol = num_iterations)
cor_25 <- matrix(NA, nrow = nrow(returns_df_clean), ncol = num_iterations)


set.seed(2025)

for(s in 1:num_iterations) {
  
  chosen_assets <- sample(ncol(returns_df_clean), 
                          num_of_stocks) 
  sub_25 <- returns_df_clean[, chosen_assets]
  
  port_ret <- rowMeans(sub_25)

  single_ret <- sub_25[, 1] 
  
  # Calculate rolling volatility for both N=25 and N=1
  vol_25[, s] <- rollapply(port_ret, 
                           window_size,
                           sd, 
                           fill = NA,  # fill = NA means that the first 119 days 
                           # will be NA since we don't have enough data
                           # due to the rolling window
                           align = "right") * sqrt(252) 
  
  vol_1[, s] <- rollapply(single_ret, 
                          window_size, 
                          sd,
                          fill = NA,  
                          align = "right") * sqrt(252)  
  
  # Calculate the rolling pairwise correlation for n=25
  # for regime comparison later on
  cor_25[, s] <- rollapply(1:nrow(sub_25), 
                           # 1:nrow(sub_25) to provide the 25 column matrix
                           window_size, 
                           function(i) { 
                             cor_matrix <- cor(sub_25[i, ]) # Calculate cor 
                             #for current window (indices i)
                             mean(cor_matrix[upper.tri(cor_matrix)]) # Extract 
                             #the unique pariwise corrleation (upper triangle) 
                             #for those dates and take the mean
                           }, fill = NA, 
                           align = "right")
}

# Double check for na values 
sum(is.na(vol_25)) 
sum(is.na(vol_1))
sum(is.na(cor_25))

# Check if this matches expected number of NA values
num_iterations * (window_size - 1) # 119 first rows should be NA cause of rolling
# window, it matches! No unexpected NA values 


# Results are compiled into a final data frame for plotting and calculations
time_series_df <- na.omit(data.frame( # remove rows with NA values from the initial rolling window
  # this will be the first 119 rows with no values 
  Date = dates_vector, # align rolling result with original dates vector we created earlier
  
  # Then we calculate the rolling volatility and correlation
  avg_vol_25 = rowMeans(vol_25, na.rm = TRUE), 
  avg_vol_1 = rowMeans(vol_1, na.rm = TRUE), 
  avg_cor_25 = rowMeans(cor_25, na.rm = TRUE), 
  
  tenth_percentile = apply(vol_1, 1, # apply function across rows
                           quantile, probs = 0.10, na.rm = TRUE), 
  ninty_percentile = apply(vol_1, 1, quantile, probs = 0.90, na.rm = TRUE)  
)) 

# Finally a plot visualizing the results is created
# between N=1 and N=25 including spread for N=1 that we calculated earlier

ggplot(time_series_df, aes(x = Date)) +
  # Create the shaded area for the 10-90th percentile spread of N=1
  geom_ribbon(aes(ymin = tenth_percentile, 
                  ymax = ninty_percentile, 
                  fill = "Single Asset Spread (10-90th)"), 
              alpha = 0.2) +
  
  geom_line(aes(y = avg_vol_1, color = "Average Single Asset (N=1)"), size = 0.7, linetype = "dashed") +
  
  geom_line(aes(y = avg_vol_25, color = "Average Diversified Portfolio (N=25)"), size = 1.2) +
  
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 1)) +
  
  scale_color_manual(values = c("Average Single Asset (N=1)" = "gray40", 
                                "Average Diversified Portfolio (N=25)" = "blue")) +
  scale_fill_manual(values = c("Single Asset Spread (10-90th)" = "gray70")) +
  
  labs(x = "", 
       y = "Annualized Volatility (%)", 
       color = "Strategy", 
       fill = "Uncertainty") +
  theme_minimal() + 
  theme(legend.position = "bottom", legend.box = "vertical")

# ----------------------------------------------------------------------------------------------

# Regime analysis for low, medium and high correlation levels

# A regime analysis was then performed based on the rolling correlation levels
# This was done to see how the diversification benefit changed across different market regimes
# defined by correlation levels. Three regimes were defined: low, medium, and high correlation.
# based on the lower 25th and upper 75th percentiles, along with the inbetween levels.
# This was shown in a table together with volatility ratio for each regime

# Volatility Ratio
time_series_df$volatility_ratio <- time_series_df$avg_vol_25 / time_series_df$avg_vol_1 

lower_bound <- quantile(cor_25, probs = 0.25, na.rm = TRUE)

# The lower bound is found at around 0.205, it is rounded down to 0.2

low_thresh <- 0.2

higher_bound <- quantile(cor_25, probs = 0.75, na.rm = TRUE)

# The higher bound is found at around 0.36, it is rounded to 0.35

high_thresh <- 0.35

time_series_df$Regime <- ifelse(time_series_df$avg_cor_25 < 0.20, "1. Low (Stable)",
                                ifelse(time_series_df$avg_cor_25 <= 0.35, "2. Medium (Neutral)", 
                                       "3. High (Crisis)"))


# create a summary table showing average metrics for each regime
summary_table <- aggregate(cbind(volatility_ratio) ~ Regime, 
                           data = time_series_df, 
                           FUN = mean)

# Include the number of observed days for each regime
summary_table$Days <- table(time_series_df$Regime)

print(summary_table)

# ------------------------------------------------------------------------------




