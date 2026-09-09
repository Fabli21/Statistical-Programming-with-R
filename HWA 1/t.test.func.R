Welch_t_test <- function(data, group, names, alpha = 0.05, bonferroni = FALSE) {
  #Check that data is a tibble or data frame
  if (!is.data.frame(data)) stop("data must be a data.frame.")
  #Check that group vector contains only A and B
  if (!all(group %in% c("A","B"))) stop("group must contain only 'A' and 'B'.")
  #Check that names match the number of columns in data
  if (length(names) != ncol(data)) stop("names must match number of columns.")
  #Split the data into two groups
  A_idx <- which(group == "A")
  B_idx <- which(group == "B")
  #Intitialize the t-vector and p-vector
  t_stats <- numeric(ncol(data))
  p_vals  <- numeric(ncol(data))
  #Begin the loop to compute the test for each column
  for (i in seq_along(names)) {
    x <- data[[i]][A_idx]
    y <- data[[i]][B_idx]
    #Obtain the sample size
    x_n <- length(x)
    y_n <- length(y)
    #Compute the mean
    x_bar <- sum(x)/x_n
    y_bar <- sum(y)/y_n
    #Compute the sample variance
    s2x <- 1/(x_n-1)*sum((x-x_bar)^2)
    s2y <- 1/(y_n-1)*sum((y-y_bar)^2)
    #Compute t-value
    t_val <- (x_bar - y_bar) / (s2x/x_n + s2y/y_n)^0.5
    #compute defrees of freedom
    num <- (s2x/x_n + s2y/y_n)^2
    den <- (s2x/x_n)^2 / (x_n - 1) + (s2y/y_n)^2 / (y_n - 1)
    df  <- num / den
    #Obtain the two-tailed p-value
    p_val <- 2 * pt(-abs(t_val), df = df)
    #Store the values
    t_stats[i] <- t_val
    p_vals[i]  <- p_val
  }
  #Bonferroni corrected p-vals
  if (bonferroni) {
    p_vals <- pmin(1, p_vals * length(p_vals))
  }
  #Output of the function
  out <- list(
    t = t_stats,       
    p = p_vals,            
    names = names,
    alpha = alpha,
    data = data,         
    group = group       
  )
  #Intialize the class of the output
  class(out) <- "Welch_t_test"
  return(out)
}
#Method 1 for the print
print.Welch_t_test <- function(obj) {
  cat("Welch Two-Sample t-test results:\n\n")
  res <- data.frame(
    Variable = obj$names,#Print the variable names
    p_value = round(obj$p, 3), #Print the p-value with three decimals
    t_value = round(obj$t, 4) #Print the t-value with four decimals
  )
  print(res)
  cat("\nSignificance level:", obj$alpha)
}
#Metohd 2 for the plot
plot.Welch_t_test <- function(obj) {
  library(ggplot2)
  #Rearange the data for the plot
  plot_df <- stack(obj$data) 
  plot_df$group <- obj$group 
  #Build the plot and set the colors for the different groups and separate them
  ggplot(plot_df, aes(x = ind, y = values, color = group)) +
    geom_point(position = position_dodge(width = 0.5), size = 1.5) + 
    scale_color_manual(values = c("A" = "blue", "B" = "red")) +
    labs(#Add title and label
      title = "Welch Two-Sample t-tests",
      x = "Variable", y = "Value", color = "Group"
    )
}