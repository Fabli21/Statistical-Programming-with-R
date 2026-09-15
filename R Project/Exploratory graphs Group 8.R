library(ggplot2)
library(patchwork)
library(igraph)
library(plotly)
library(mvtnorm)

returns_only <- dailylogreturns_df_dataset[, -1]

################################################################################
# Code for exploratory data analysis 
################################################################################


################################################################################
# Explaratory graphs
################################################################################


# Univariate Densities


# For the univariate densities we plot how the distribution of a stock 
# from the data is compared to the normal distribution. We create a function 
# (DRY) to to this for any stock from the data. The function estimates the mean
# and standard deviation from the stock to create a normal density based on 
# the estimated parameters and then in the same plot include the density 
# from the actual data

create_univariate <- function(data, col_name) {
  mu <- mean(data[[col_name]])
  sigma <- sd(data[[col_name]])
  
  ggplot(data, aes(x = .data[[col_name]])) +
    geom_density(linewidth = 0.8, color = "steelblue") +

    stat_function(fun = dnorm, args = list(mean = mu, sd = sigma), 
                  linetype = "dashed", linewidth = 0.8) +
    
    labs(title = col_name, x = " ", y = " ")
}

# Now we randomize from the sample to pick four random stocks and then uses the 
# function for each random stock
set.seed(2026)
cols <- sample(names(returns_only), 4)
p1 <- create_univariate(returns_only, cols[1])
p2 <- create_univariate(returns_only, cols[2])
p3 <- create_univariate(returns_only, cols[3])
p4 <- create_univariate(returns_only, cols[4])

# Plot the four graphs like a grid for an overall comparission 
(p1 + p2)/(p3 + p4)


# Bivariate Densities

# Now for the bivariate case. This function works in a similair way like the first one
# We use again a function for "DRY". The function first estimates a mean vector
# and a covariance matrix to estimate the joint distribution for two stocks.
# Then it creates a cordinate grid for the theoretical normal dsitribution
# Lastly a plot is created adding the actual data in the plot along with 
# the multivariate normal distribution contours representing the black elipses

create_bivariate <- function(data, x_col, y_col) {
  
  df_subset <- data[, c(x_col, y_col)]
  mu <- colMeans(df_subset)
  sigma <- cov(df_subset)
  
  x_seq <- seq(min(df_subset[[1]]) * 1.2, max(df_subset[[1]]) * 1.2, length.out = 100)
  y_seq <- seq(min(df_subset[[2]]) * 1.2, max(df_subset[[2]]) * 1.2, length.out = 100)
  grid <- expand.grid(x = x_seq, y = y_seq)

  grid$z <- dmvnorm(as.matrix(grid), mean = mu, sigma = sigma)
  
  ggplot() +
    geom_point(data = df_subset, aes(x = .data[[x_col]], y = .data[[y_col]]), 
               alpha = 0.4, color = "steelblue", size = 1) +
    
    geom_contour(data = grid, aes(x = x, y = y, z = z), 
                 color = "black", linewidth = 0.5) +
    
    labs(x = x_col, y = y_col)
}

# Now we use the function, first we create a matrix where we sample 8 random 
# stocks two create pairs, we later use lapply to iterate through the function for 
# the different pairs and then plot them in a grid

set.seed(2026)
cols <- names(returns_only)
pairs <- matrix(sample(cols, 8), ncol = 2)
b_plots <- lapply(1:4, function(i) create_bivariate(returns_only, pairs[i,1], pairs[i,2]))

wrap_plots(b_plots, ncol = 2)



#NETWORK PLOT


# To illustrate the network between different sectors in the data
# for the network plot the first the correlation matrix is computed.
# The threshold is set to 0.6 which is approximatley the 98 quantile in the 
# dataset


correlations <- cor(returns_only)
threshold <- 0.6  

# We create a network based on the correlation matrix from the package 
# igraph if the correlations between two stocks is greater than the threshold. 
# The layout is determined by the #Fruchterman-Reingold from the same package
# This makes stocks more correlated to eachother to appear closer to eachother 
# In the plot for easier visualization and iterpretation. 

network <- graph_from_adjacency_matrix(
  ifelse(correlations > threshold & 
           row(correlations) != col(correlations), 1, 0),
  mode = "undirected",  
  diag = FALSE      
)

layout <- layout_with_fr(network)

# We construct the graph which only constits of the layout from the network

network_plot <- ggplot(data.frame(x = layout[, 1], y = layout[, 2]), 
               aes(x = x, y = y))

# We extract all the connections (rho > 0.6) between from the network and construct
# the nodes with using cordinates based on the position between the stocks
# We then add this lines to the plot to present the correlations between stocks
edges <- as_edgelist(network, names = FALSE)

edges_data <- data.frame(
  x_start = layout[edges[, 1], 1],
  y_start = layout[edges[, 1], 2],
  x_end   = layout[edges[, 2], 1],
  y_end   = layout[edges[, 2], 2]
)

network_plot <- network_plot + 
  geom_segment(data = edges_data, aes(x = x_start, y = y_start, xend = x_end, yend = y_end),
               color = "gray", alpha = 0.5, linewidth = 0.8)


# We then add all the stocks that belong to each position in the network and

network_plot <- network_plot + 
  geom_point(size = 4, color = "steelblue", alpha = 0.8)

network_plot <- network_plot + 
  labs(
    title = paste(" "), 
    subtitle = paste("Each point = 1 stock, Lines = correlation > 0.6")
  ) +
  theme_void() +
  theme(
    plot.subtitle = element_text(hjust = 0.5)  # Centrerar subtiteln
  )


print(network_plot)


# 3D Rolling window timeseries, vol & cor


# To get a representative rolling wondow of the whole market we take the rowmeans
# of the returns data frame. This is equal to an equally weighted portfolio of
# all stocks that are included in the data
# Since one trading year is appriximatley 252, for the timeline to go from 2015
# to 2025 we need to adjust the time sequence by 365/252. This will result in
# the exact dater are not matched but it will still be a useful approximation
# for the time line. A data frame is constructed for the statistics computed later

market_returns <- rowMeans(returns_only)
scale_factor <- 365/252
date_seq <- seq.Date(from = as.Date("2015-11-13"), 
                     by = scale_factor, 
                     length.out = nrow(returns_only))


market_data <- data.frame(
  date = date_seq,
  market_return = market_returns
)

# We use a window size of 120 days (apporximatley 6 months in a trading year)
#to get an overall tradeoff between the noise in data and still capture the 
# medium term trends and initiialize the vectors used for 

window_size <- 120

n <- nrow(returns_only)
rolling_corr <- rep(NA, n) 
rolling_vol <- rep(NA, n) 

# This loop computes the annual volatility and mean corelation for each 
# window size. i.e the it takes the current date and the 119 past days to compute
# the statistic for each window and 


for (i in window_size:n) {
  window_idx <- (i - window_size + 1):i 
  
  rolling_vol[i] <- sd(market_returns[window_idx]) * sqrt(252)
  
  cor_matrix <- cor(returns_only[window_idx, ])
  rolling_corr[i] <- mean(cor_matrix[upper.tri(cor_matrix)])
}

# The results from the loop is stored in the data frame, since the first 119 
# observations will include NA's, they are removed when revealing the plot


market_data$rolling_correlation <- rolling_corr
market_data$rolling_volatility <- rolling_vol
plot_data <- market_data[complete.cases(market_data), ]

# We construct the plot using plot_ly from the package plotly. This package 
# let us construct an interactive 3D plot. We use the 3 axis as the 
# rolling correlation, rolling volatiltiy and the date. The timeline is also
# colorcoded based on the volatility

plot_3d <- plot_ly(
  data = plot_data, 
  x = ~date, 
  y = ~rolling_correlation, 
  z = ~rolling_volatility,
  type = "scatter3d",
  mode = "lines+markers", 
  marker = list(
    size = 3,
    color = ~rolling_volatility, 
    colorscale = "Viridis",
    showscale = TRUE, 
    colorbar = list(title = "Volatility",
                    tickformat = ".0%") 
  )
)

# Here we add the labels for each axis and can adjust so the volatitliy  
# is represented in percent for easier interpretation

plot_3d <- layout(
  plot_3d,
  scene = list(
    xaxis = list(title = "Date"),
    yaxis = list(
      title = "ρ",
      range = c(0, max(plot_data$rolling_correlation) * 1.1)
    ),
    zaxis = list(
      title = "σ",
      range = c(0, max(plot_data$rolling_volatility) * 1.1),
      tickformat = ".0%"
    )
  )
)

print(plot_3d)
