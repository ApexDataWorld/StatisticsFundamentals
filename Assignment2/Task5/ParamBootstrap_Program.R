#First, let's assume we know the true values of alpha and lambda (these values chosen arbitrarily)
whales <- readr::read_csv("https://www4.stat.ncsu.edu/~online/datasets/whales.txt", 
                          col_names = "time")
alpha0 <- 0.3
lambda0 <- 4

#Number of samples (estimators) to create
N <- 10000
#sample size
n <- length(whales$time)
#for reproducibility
set.seed(42)

estimates <- replicate(N,{
  sim_data <- rgamma(n, shape = alpha0, rate = lambda0)
  ybar <- mean(sim_data)
  sbsq <- mean(sim_data^2)-ybar^2
  alpha_hat <- ybar^2/sbsq
  lambda_hat <- ybar/sbsq
  return(c("alpha_hat" = alpha_hat, "lambda_hat" = lambda_hat))
})

par(mfrow = c(1, 2))
hist(estimates[1, ], main = "Exact Distribution of \nalpha_hat", xlab = "alpha_hat")
hist(estimates[2, ], main = "Exact Distribution of \nlambda_hat", xlab = "lambda_hat")

#SE(alpha_hat), SE(lambda_hat)
c(sd(estimates[1, ]), sd(estimates[2, ]))

#Get estimates from the data
ybar <- mean(whales$time)
sbsq <- mean(whales$time^2)-ybar^2
alpha_hat <- ybar^2/sbsq
lambda_hat <- ybar/sbsq

#Number of bootstrap datasets (estimators) to create
B <- 10000
#sample size
n <- length(whales$time)

boot_estimates <- replicate(B,{
  sim_data <- rgamma(n, shape = alpha_hat, rate = lambda_hat)
  ybar <- mean(sim_data)
  sbsq <- mean(sim_data^2)-ybar^2
  alpha_hat_boot <- ybar^2/sbsq
  lambda_hat_boot <- ybar/sbsq
  return(c("alpha_hat_boot" = alpha_hat_boot, "lambda_hat_boot" = lambda_hat_boot))
})

#bootstrap standard errors
#SE(alpha_hat), SE(lambda_hat)
c(sd(boot_estimates[1, ]), sd(boot_estimates[2, ]))

par(mfrow = c(1, 2))
hist(boot_estimates[1, ], main = "Bootstrap Distribution of \nalpha_hat", xlab = "alpha_hat")
hist(boot_estimates[2, ], main = "Bootstrap Distribution of \nlambda_hat", xlab = "lambda_hat")

#Number of bootstrap datasets (estimators) to create
B <- 10000
#sample size
n <- length(whales$time)

boot_estimates <- replicate(B,{
  sim_data <- sample(whales$time, size = n, replace = TRUE)
  ybar <- mean(sim_data)
  sbsq <- mean(sim_data^2)-ybar^2
  alpha_hat_boot <- ybar^2/sbsq
  lambda_hat_boot <- ybar/sbsq
  return(c("alpha_hat_boot" = alpha_hat_boot, "lambda_hat_boot" = lambda_hat_boot))
})

#bootstrap standard errors
#SE(alpha_hat), SE(lambda_hat)
c(sd(boot_estimates[1, ]), sd(boot_estimates[2, ]))

par(mfrow = c(1, 2))
hist(boot_estimates[1, ], main = "Bootstrap Distribution of \nalpha_hat", xlab = "alpha_hat")
hist(boot_estimates[2, ], main = "Bootstrap Distribution of \nlambda_hat", xlab = "lambda_hat")