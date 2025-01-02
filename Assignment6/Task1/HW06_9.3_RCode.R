# Define the critical region bounds
lower_bound <- 40
upper_bound <- 60

# Function to calculate alpha (significance level) under H0: p = 0.5
find_alpha <- function() {
  mean_H0 <- 50
  sd_H0 <- sqrt(100 * 0.5 * 0.5) # SD under H0
  
  # find alpha , probability of rejecting H0 under p = 0.5 
  alpha <- pnorm(lower_bound, mean = mean_H0, sd = sd_H0) + 
    (1 - pnorm(upper_bound, mean = mean_H0, sd = sd_H0))
  
  return(alpha)
}

# Function to calculate the power for a given p
find_power <- function(p) {
  mean_p <- 100 * p
  sd_p <- sqrt(100 * p * (1 - p))
  
  # Calculate probability of rejection (power) for this p
  power <- pnorm(lower_bound, mean = mean_p, sd = sd_p) + 
    (1 - pnorm(upper_bound, mean = mean_p, sd = sd_p))
  return(power)
}

# Plotting the critical region, alpha, and power as a function of p
plot_critical_region_and_power <- function() {

  mean_H0 <- 50
  sd_H0 <- sqrt(100 * 0.5 * 0.5) # SD under H0
  
  # Generate x values for the normal distribution curve
  x_vals <- seq(20, 80, by = 0.1)
  y_vals <- dnorm(x_vals, mean = mean_H0, sd = sd_H0)
  
  # Plot normal distribution for null hypothesis
  plot(x_vals, y_vals, type = "l", col = "black", lwd = 2,
       xlab = "X values", ylab = "Density", main = "Critical Region and Alpha")
  
  # Shade critical regions or rejection regions
  polygon(c(x_vals[x_vals < lower_bound], lower_bound), 
          c(y_vals[x_vals < lower_bound], 0), col = "red", border = NA)
  polygon(c(x_vals[x_vals > upper_bound], upper_bound), 
          c(y_vals[x_vals > upper_bound], 0), col = "red", border = NA)
  
  alpha <- find_alpha()
  text(60, 0.1, paste("Alpha =", round(alpha, 3)), col = "green")

  abline(v = 50, col = "blue", lty = 2)
  
  # Now plot the power function for a range of p values
  p_values <- seq(0, 1, by = 0.01)
  power_values <- sapply(p_values, find_power)

  plot(p_values, power_values, type = "l", col = "blue", lwd = 2,
       xlab = "p", ylab = "Power", main = "Power Function for Test of p")
  
  # Add horizontal line for alpha level
  abline(h = alpha, col = "red", lty = 2) 
  text(0.5, alpha, labels = paste("alpha =", round(alpha, 3)), pos = 4, col = "red")
}

# Calculate and display alpha
alpha <- find_alpha()
cat("Alpha (significance level) =", round(alpha, 3), "\n")

# Call the function to plot both critical region and power function
plot_critical_region_and_power()

