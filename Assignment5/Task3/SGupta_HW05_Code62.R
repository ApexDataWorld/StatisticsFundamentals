
# Sample size
n <- 20
# Average waiting time per customer (in minutes)
avg_time <- 5.1
# Total observed waiting time
total_time <- n * avg_time

# Case 1: Prior mean = 0.5, SD = 1
mean1 <- 0.5
sd1 <- 1
alpha1 <- (mean1 / sd1)^2
beta1 <- mean1 / (sd1^2)

# Case 2: Prior mean = 10, SD = 20
mean2 <- 10
sd2 <- 20
alpha2 <- (mean2 / sd2)^2
beta2 <- mean2 / (sd2^2)

# Posterior parameters
# Posterior alpha = alpha_prior + n
# Posterior beta = beta_prior + total_time

# Case 1 Posterior
alpha_post1 <- alpha1 + n
beta_post1 <- beta1 + total_time

# Case 2 Posterior
alpha_post2 <- alpha2 + n
beta_post2 <- beta2 + total_time

# Posterior means
lambda_post_mean1 <- alpha_post1 / beta_post1
lambda_post_mean2 <- alpha_post2 / beta_post2

# Plot the posterior distributions
par(mfrow = c(1, 2))  # two plots side by side

# Case 1 posterior distribution
curve(dgamma(x, shape = alpha_post1, rate = beta_post1), 
      from = 0, to = 0.5, 
      main = "Posterior (Mean = 0.5, SD = 1)", 
      xlab = "Lambda", 
      ylab = "Density")

# Add prior mean, sample mean, and posterior mean lines
abline(v = mean1, col = "green", lwd = 2)  # prior mean
abline(v = 1 / avg_time, col = "blue", lwd = 2)  # sample mean
abline(v = lambda_post_mean1, col = "black", lwd = 2)  # posterior mean

legend("topright", legend = c("Prior Mean", "Sample Mean", "Posterior Mean"), 
       col = c("green", "blue", "black"), lwd = 2)

# Case 2 posterior distribution
curve(dgamma(x, shape = alpha_post2, rate = beta_post2), 
      from = 0, to = 0.5, 
      main = "Posterior (Mean = 10, SD = 20)", 
      xlab = "Lambda", 
      ylab = "Density")

# Add prior mean, sample mean, and posterior mean lines
abline(v = mean2, col = "green", lwd = 2)  # prior mean
abline(v = 1 / avg_time, col = "blue", lwd = 2)  # sample mean
abline(v = lambda_post_mean2, col = "black", lwd = 2)  # posterior mean

legend("topright", legend = c("Prior Mean", "Sample Mean", "Posterior Mean"), 
       col = c("green", "blue", "black"), lwd = 2)

# Posterior means
cat("Case 1 Posterior Mean (Mean = 0.5, SD = 1):", lambda_post_mean1, "\n")
cat("Case 2 Posterior Mean (Mean = 10, SD = 20):", lambda_post_mean2, "\n")


#Conclusion : 
#The posterior distribution for Case 1 (with prior mean = 0.5, SD = 1) 
#will be more concentrated around smaller values of λ, reflecting the 
#more informative prior.

#The posterior distribution for Case 2 (with prior mean = 10, SD = 20) 
#is broader and less concentrated, indicating a less informative prior.
