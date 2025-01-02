################################################
#Author:    Saurabh Gupta                                                                              
#Collaborators:                                                                        
#Program Purpose: ST502 Final Project 
#Date: 12-10-2024                                                                      
################################################


# Observed concordant/discordant pairs data
dataset <- matrix(c(85, 15, 40, 110), nrow = 2, byrow = TRUE,
                  dimnames = list("Drug A" = c("Success", "Failure"),
                                  "Drug B" = c("Success", "Failure")))
print(dataset)
# Extract discordant counts
n12 <- dataset[1, 2] # Drug A Success, Drug B Failure
n21 <- dataset[2, 1] # Drug A Failure, Drug B Success

# Calculate test statistic
X2_value <- (n12 - n21)^2 / (n12 + n21)

# p-value
p_value <- pchisq(X2_value, df = 1, lower.tail = FALSE)

# Critical value or rejection region for alpha = 0.05
alpha <- 0.05
critical_value <- qchisq(1 - alpha, df = 1)

# Decision
decision <- ifelse(X2_value > critical_value, "Reject H0", "Fail to Reject H0")

# Results
list(test_statistic = X2_value, p_value = p_value, critical_value = critical_value, decision = decision)


# Perform McNemar's test without continuity correction
mcnemar_result <- mcnemar.test(dataset, correct = FALSE)
print(mcnemar_result)


# Install MultiRNG package if not already installed
if (!require("MultiRNG")) install.packages("MultiRNG")
library(MultiRNG)

# Define parameters
pi1_values <- c(0.1, 0.4, 0.8)         # π1 values
pi2_offsets <- c(0, 0.02, 0.05, 0.1)   # Offsets for π2
n_values <- c(25, 50, 80, 200)         # Sample sizes
rho_values <- c(0, 0.2, 0.5)          # Correlations

# Initialize an empty data frame to store the results
final_dataset <- data.frame()

# Create the base grid for n, rho, and pi1
sample_dataset <- expand.grid(
  n = n_values,
  rho = rho_values,
  pi1 = pi1_values
)

# Loop over each row in the base grid
for (i in seq_len(nrow(sample_dataset))) {
  # Extract the current row
  row <- sample_dataset[i, ]
  
  # Compute the π2 values for the current π1 using offsets
  pi2_values <- row$pi1 + pi2_offsets
  
  # Create rows for each π2 value
  temp <- data.frame(
    n = row$n,
    rho = row$rho,
    pi1 = row$pi1,
    pi2 = pi2_values
  )
  
  # Append to the final results
  final_dataset <- rbind(final_dataset, temp)
}

# View the final results
print(final_dataset)

# Function to run a single McNemar's test simulation
McNemarSimulation <- function(n, pi1, pi2, rho) {
  # Generate correlated binary data
  data <- draw.correlated.binary(
    no.row = n,
    d = 2,
    prop.vec = c(pi1, pi2),
    corr.mat = matrix(c(1, rho, rho, 1), nrow = 2)
  )
  
  # Create 2x2 contingency table
  n11 <- sum(data[, 1] == 1 & data[, 2] == 1) # Concordant
  n12 <- sum(data[, 1] == 1 & data[, 2] == 0) # Discordant
  n21 <- sum(data[, 1] == 0 & data[, 2] == 1) # Discordant
  n22 <- sum(data[, 1] == 0 & data[, 2] == 0) # Concordant
  
  # Calculate McNemar's test statistic
  if (n12 + n21 == 0) return(FALSE) # Prevent division by zero
  X2 <- (n12 - n21)^2 / (n12 + n21)
  p_value <- 1 - pchisq(X2, df = 1)
  
  # Determine whether to reject the null hypothesis (H0)
  reject_null <- p_value < 0.05
  
  # Return the result (TRUE if H0 is rejected, FALSE otherwise)
  reject_null
}

# Function to run multiple McNemar's test simulations and compute rejection proportion
RunMcNemarSimulation <- function(n, pi1, pi2, rho, N = 1000) {
  results <- replicate(N, McNemarSimulation(n, pi1, pi2, rho))
  rejection_proportion <- mean(results) # Compute proportion of rejections
  return(rejection_proportion)
}

# Add a column to store rejection rates
final_dataset$rejection_rate <- NA

# Run simulations
for (i in seq_len(nrow(final_dataset))) {
  n <- final_dataset$n[i]
  pi1 <- final_dataset$pi1[i]
  pi2 <- final_dataset$pi2[i]
  rho <- final_dataset$rho[i]
  
  final_dataset$rejection_rate[i] <- RunMcNemarSimulation(n, pi1, pi2, rho)
  #print(final_dataset$rejection_rate[i])
}

# Check summary of rejection rates
summary(final_dataset$rejection_rate)


library(ggplot2)

# Loop over each pi1 value
for (pi_1 in pi1_values) {
  
  # Filter the data for the current pi1 value
  filtered_df <- final_dataset[final_dataset$pi1 == pi_1, ]
  
  # Create the plot
  pt <- ggplot(filtered_df, aes(x = pi2 - pi1, y = rejection_rate, color = factor(rho), group = rho)) +
    geom_line() +
    facet_grid(. ~ n) +  # Facets for different sample sizes
    labs(
      title = sprintf("Rejection Rate for different sample sizes 
      and correlations with pi1 = %.2f", pi_1),
      x = expression(pi[2] - pi[1]),
      y = "Rejection Rate",
      color = "Correlation (ρ)"
    ) +
    theme_minimal() 
  
  # Print the plot
  print(pt)
}
