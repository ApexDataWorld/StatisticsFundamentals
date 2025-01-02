################################################;
#Author:    Saurabh Gupta                                                                              ;
#Collaborators:                                                                        ;
#Program Purpose: ST502 (Homework 3) question 8.16
#Date: 09/11/24                                                                                    ;
################################################;

# Install nimble package if not already installed
install.packages("nimble")

library(nimble)

# Given data
data <- c(-0.04, -6.01, 1.05, 2.10, -2.76, -2.60, 4.02, 10.50, 2.75, 3.31, -1.21, 3.26)

# Step 1: Calculate the MLE for sigma (mean of absolute values of data)
sigma_hat <- mean(abs(data))

print (sigma_hat)

#########################################################

# Step 2: Set up parametric bootstrap
B <- 10000  # Number of bootstrap samples
n <- length(data)  # Sample size

# Step 3: Generate bootstrap estimates
set.seed(42)  # For reproducibility
bootstrap_mles <- replicate(B, {
  # Generate bootstrap sample from the  distribution 
  sim_data <- rdexp(n, location = 0, scale = sigma_hat)
    # Calculate the MLE for this bootstrap sample
  sigma_hat_boot <- mean(abs(sim_data))
    return(sigma_hat_boot)
})

# Step 4: Estimate the standard error of the MLE
bootstrap_se <- sd(bootstrap_mles)

# Print results
cat("MLE for sigma: ", sigma_hat, "\n")
cat("Standard Error from bootstrap: ", bootstrap_se, "\n")

# Step 5: Plot the distribution of bootstrap estimates
hist(bootstrap_mles, main = "Bootstrap Distribution of MLE for sigma", xlab = "MLE for sigma", breaks = 30)

#########################################################
n <- length(data)

# Calculate the asymptotic variance estimate
asymptotic_variance <- sigma_hat^2 / n
# Print the result
cat("Asymptotic Variance Estimate: ", asymptotic_variance, "\n")


