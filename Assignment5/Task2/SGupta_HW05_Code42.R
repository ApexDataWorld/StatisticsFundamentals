# Load the data
gamma_data <- read.csv("gamma-ray.csv")

# Extract the total count of gamma rays and total time duration
total_counts <- sum(gamma_data$count)
total_time <- sum(gamma_data$seconds)
print(total_counts)
print(total_time)
# Specify parameters for the improper Gamma prior
alpha_prior <- 0  # improper prior
beta_prior <- 0   # improper prior

# Posterior parameters for Gamma distribution (since the conjugate prior for Poisson is Gamma)
alpha_post <- alpha_prior + total_counts
beta_post <- beta_prior + total_time
print(alpha_post)
print(beta_post)
# Posterior mean for lambda
lambda_post_mean <- alpha_post / beta_post

# Posterior standard error for lambdawd()
lambda_post_se <- sqrt(alpha_post) / beta_post

# Posterior distribution: Plot
curve(dgamma(x, shape = alpha_post, rate = beta_post), 
      from = 0, to = 0.1, 
      main = "Posterior Distribution of Lambda",
      xlab = "Lambda", 
      ylab = "Density")

# Posterior mean and standard error
cat("Posterior Mean:", lambda_post_mean, "\n")
cat("Posterior Standard Error:", lambda_post_se, "\n")
