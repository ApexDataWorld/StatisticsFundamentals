################################################;
#Author:    Saurabh Gupta                                                                              ;
#Collaborators:                                                                        ;
#Program Purpose: ST502 (Homework 3)  question 8.43 
#Date: 09/11/24                                                                                    ;
################################################;

# Load the data (assuming the file is in CSV format or plain text)
data <- readr::read_csv("https://www4.stat.ncsu.edu/online/datasets/gamma-arrivals.csv",
                          col_names = "data")
arrival_times <- data$data
head(arrival_times)

##############################################################################
#a. Histogram of the Arrival Times
# Create the histogram
hist(arrival_times, breaks = 50, probability = TRUE, 
     main = "Histogram of Interarrival Times", 
     xlab = "Interarrival Time (seconds)", col = "lightblue", freq = FALSE)

# Overlay a Gamma distribution for comparison
# Placeholder parameters for visualization (to be refined in part b)
curve(dgamma(x, shape = 2, rate = 0.5), from = 0, to = 5, add = TRUE, col = "red")

## The histogram graph looks like a gamma distribution as gamma is usually a right skewed distribution 

##############################################################################
#b. Fit Gamma Parameters using Method of Moments and Maximum Likelihood
# Method of Moments
sample_mean <- mean(arrival_times)
sample_var <- var(arrival_times)

ybar <- mean(arrival_times)
sbsq <- mean(arrival_times^2)-ybar^2
c(ybar,sbsq)

alpha_hat <- ybar^2/sbsq
lambda_hat <- ybar/sbsq
c(alpha_hat, lambda_hat)

# Method of moments estimates
shape_moment <- sample_mean^2 / sample_var
rate_moment <- sample_mean / sample_var
cat("Method of Moments - Shape:", shape_moment, "Rate:", rate_moment, "\n")
cat("parameters - alpha_hat:", alpha_hat, "lambda_hat:", lambda_hat, "\n")



library(MASS)

# Maximum Likelihood Estimation
mle_fit <- fitdistr(arrival_times, densfun = "gamma")
shape_mle <- mle_fit$estimate[1]
rate_mle <- mle_fit$estimate[2]
cat("Maximum Likelihood - Shape:", shape_mle, "Rate:", rate_mle, "\n")



##############################################################################
#c. Plotting Fitted Gamma Densities on Top of the Histogram
# Re-create the histogram
hist(arrival_times, breaks = 50, probability = TRUE, 
     main = "Fitted Gamma Densities on Histogram", 
     xlab = "Interarrival Time (seconds)", col = "lightblue")

# Overlay Method of Moments Gamma fit
curve(dgamma(x, shape = alpha_hat, rate = lambda_hat), 
      col = "blue", lwd = 2, add = TRUE)

# Overlay Maximum Likelihood Gamma fit
curve(dgamma(x, shape = shape_mle, rate = rate_mle), 
      col = "red", lwd = 2, add = TRUE)

legend("topright", legend = c("Method of Moments", "Maximum Likelihood"), 
       col = c("blue", "red"), lwd = 2)



##############################################################################
#d. Bootstrap to Estimate Standard Errors
# Function to perform bootstrap for method of moments
ybar <- mean(arrival_times)
sbsq <- mean(arrival_times^2)-ybar^2
c(ybar,sbsq)

alpha_hat <- ybar^2/sbsq
lambda_hat <- ybar/sbsq
c(alpha_hat, lambda_hat)

B <- 10000
n <- length(arrival_times)

boot_estimates <- replicate(B, {
  sim_data <- rgamma(n,shape = alpha_hat, rate = lambda_hat)
  ybar <- mean(sim_data)
  sbsq <- mean(sim_data^2)-ybar^2
  alpha_hat_boot <- ybar^2/sbsq
  lambda_hat_boot <- ybar/sbsq
  return(c("alpha_hat_boot" = alpha_hat_boot, "lambda_hat_boot" = lambda_hat_boot))
})


MLE_boots <- replicate(B, {
  MLE_data <- rgamma(n, shape = shape_mle, rate = rate_mle)
  MLE_boot <- fitdistr(MLE_data, "gamma")
  return(c("mle_boot" = MLE_boot))
})


c(sd(boot_estimates[1,]), sd(boot_estimates[2,]))
par(mfrow = c(1,2))
hist(boot_estimates[1,], main = "Exact Distribution of \nalpha_hat", xlab="alpha_hat")
hist(boot_estimates[2,], main = "Exact Distribution of \nlambda_hat", xlab="lambda_hat")
c(sd(MLE_boot[1,]), sd(MLE_boot[2,]))
par(mfrow = c(1,2))


print ("part d is complete")

# Number of bootstrap samples
#n_bootstrap <- 1000

# Perform bootstrap for method of moments
#bootstrap_mom_results <- replicate(n_bootstrap, boot_estimates(interarrival_times))
#mom_se <- apply(bootstrap_mom_results, 1, sd)
#cat("Method of Moments - SE for Shape:", mom_se[1], "SE for Rate:", mom_se[2], "\n")

# Perform bootstrap for MLE
#bootstrap_mle_results <- replicate(n_bootstrap, MLE_boots(interarrival_times))
#mle_se <- apply(bootstrap_mle_results, 1, sd)
#cat("Maximum Likelihood - SE for Shape:", mle_se[1], "SE for Rate:", mle_se[2], "\n")



##############################################################################
#e. form approximate confidence intervals for the parameters.
# Bootstrap confidence intervals for Method of Moments
mom_ci_alpha <- quantile(boot_estimates[1,], probs = c(0.025, 0.975))
mom_ci_lambda <- quantile(boot_estimates[2,], probs = c(0.025, 0.975))
cat("Method of Moments - CI for MON Aloha:", mom_ci_alpha, "CI for MOM Lambda:", mom_ci_lambda, "\n")

# Bootstrap confidence intervals for MLE
mle_ci_alpha <- quantile(MLE_boots[1,], probs = c(0.025, 0.975))
mle_ci_lambda <- quantile(MLE_boots[2,], probs = c(0.025, 0.975))
cat("Maximum Likelihood - CI for MLE Alpha:", mle_ci_alpha, "CI for MLE Lambda:", mle_ci_lambda, "\n")

print ("part e is complete")


##############################################################################
##############################################################################



