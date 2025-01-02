# c
counts <- c(10, 68, 112)  # Hp1-1, Hp1-2, Hp2-2 counts
n <- sum(counts)          # Total number of observations

# MLE of theta
theta_hat <- (counts[2] + 2 * counts[3]) / (2 * n)
theta_hat

var_theta_hat <- 0.0004684

# Standard error from the asymptotic variance
se_theta_hat <- sqrt(var_theta_hat)

# 99% confidence interval
alpha <- 0.01
z <- qnorm(1 - alpha / 2)  # z-value for 99% CI

ci_lower <- theta_hat - z * se_theta_hat
ci_upper <- theta_hat + z * se_theta_hat

c(ci_lower, ci_upper)

###############################################
###############################################
#d.
# Define the observed counts for the categories
observed_vector <- c(10, 68, 112)  # Hp1-1, Hp1-2, Hp2-2 counts
n <- sum(observed_vector)          # Total number of observations (190)
n
# Probability for each genotype
theta <- 0.7684
AA <- (1 - theta) ^ 2
Aa <- 2 * theta * (1 - theta)
aa <- (theta) ^ 2
p_mle <- c(AA, Aa, aa)
p_mle
# Perform multinomial sampling with B bootstraps
B <- 10000
bootstrap_samples <- rmultinom(B, n, p_mle)

# Each column in bootstrap_samples is a resampled count for Hp1-1, Hp1-2, Hp2-2
# Example of the first bootstrap sample
bootstrap_samples[,1]

# Define the category labels
categories <- c("Hp1-1", "Hp1-2", "Hp2-2")

# Perform sampling with replacement using the observed probabilities
bootstrap_samples <- replicate(B, {
  sample(categories, size = n, replace = TRUE, prob = p_mle)
})

bootstrap_samples[, 1]

# Standard deviation of bootstrap estimates
sd_bootstrap <- sd(bootstrap_samples)
sd_bootstrap


###############################################
###############################################

#e. 
# Bootstrap confidence intervals
boot_ci_lower <- quantile(bootstrap_samples, probs = 0.005)
boot_ci_upper <- quantile(bootstrap_samples, probs = 0.995)

c(boot_ci_lower, boot_ci_upper)






