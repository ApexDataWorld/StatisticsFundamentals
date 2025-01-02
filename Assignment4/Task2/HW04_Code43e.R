#read in the data
arrivals <- readr::read_csv("https://www4.stat.ncsu.edu/online/datasets/gamma-arrivals.csv",
                        col_names = "arrival_time")


#a
hist(arrivals$arrival_time,
     xlab = "Arrival times",
     main = "Histogram of Arrival Times")
#b fit the parameters
mean_arrival <- mean(arrivals$arrival_time)
sd_arrival <- mean(arrivals$arrival_time^2)-(mean_arrival)^2
alpha_hat_MOM<- mean_arrival^2/sd_arrival

lambda_hat_MOM <- mean_arrival/sd_arrival
MOMs <- c(alpha_hat_MOM, lambda_hat_MOM)
MOMs
#mle fits
MLEs <- MASS::fitdistr(arrivals$arrival_time, densfun = "gamma")
MLEs
#c
hist(arrivals$arrival_time,
     xlab = "Arrival times",
     main = "Histogram of Arrival Times",
     freq = FALSE)
curve(dgamma(x, shape = MOMs[1], rate = MOMs[2]),
      col = "Red", from = 0, to = 500, add = TRUE)
curve(dgamma(x, shape = MLEs$estimate[1], rate = MLEs$estimate[2]),
      col = "Blue", from = 0, to = 500, add = TRUE)
legend("topright", legend = c("MOM", "MLE"),
       col = c("Red", "Blue"), lwd = 1)
#d
B <- 10000
n <- length(arrivals$arrival_time)
MOM_boots <- replicate(B, {
  MOM_data <- rgamma(n, shape = alpha_hat_MOM, rate = lambda_hat_MOM)
  mean_MOM_data <- mean(MOM_data)
  sd_MOM_data <- mean(MOM_data^2)-(mean(MOM_data))^2
  alpha_hat_MOM_boot <- mean_MOM_data^2/sd_MOM_data
  lambda_hat_MOM_boot <- mean_MOM_data/sd_MOM_data
  c(alpha_hat_MOM_boot, lambda_hat_MOM_boot)
})
MLE_boots <- replicate(B, {
  MLE_data <- rgamma(n, shape = MLEs$estimate[1], rate = MLEs$estimate[2])
  MLE_boot <- MASS::fitdistr(MLE_data, "gamma")
  c(MLE_boot$estimate[1], MLE_boot$estimate[2])
})
c(sd(MOM_boots[1,]), sd(MOM_boots[2,]))
c(sd(MLE_boots[1,]), sd(MLE_boots[2,]))


##############################################################################
#e. form approximate confidence intervals for the parameters.
# Bootstrap confidence intervals for Method of Moments
mom_ci_alpha <- quantile(MOM_boots[1,], probs = c(0.025, 0.975))
mom_ci_lambda <- quantile(MOM_boots[2,], probs = c(0.025, 0.975))
cat("Method of Moments - CI for MON Aloha:", mom_ci_alpha, "CI for MOM Lambda:", mom_ci_lambda, "\n")

# Bootstrap confidence intervals for MLE
mle_ci_alpha <- quantile(MLE_boots[1,], probs = c(0.025, 0.975))
mle_ci_lambda <- quantile(MLE_boots[2,], probs = c(0.025, 0.975))
cat("Maximum Likelihood - CI for MLE Alpha:", mle_ci_alpha, "CI for MLE Lambda:", mle_ci_lambda, "\n")

#the true value of α is likely between 0.951 and 1.076.
#the true value of λ is likely between 0.0118 and 0.0136.
#the true value of α is likely between 0.987 and 1.068.
#the true value of λ is likely between 0.0122 and 0.0135.

#Both the Method of Moments and MLE produce relatively similar confidence intervals for α and λ, 
#which indicates that both methods are giving consistent estimates for these parameters based on data.
#MOM uses sample moments (mean, variance), while MLE maximizes the likelihood of the data given the model.

##############################################################################
library(tidyverse)
library(plotrix)

N <- 10000
n <- length(arrivals$arrival_time)
head(arrivals$arrival_time,10)
mu <- mean(arrivals$arrival_time)
mu

observed_CIs <- replicate(N, {
  sample_data <- sample(arrivals$arrival_time, size = n, replace = FALSE)
  c(mean(sample_data)-qnorm(0.975)*sd(sample_data)/sqrt(n),
    mean(sample_data)+qnorm(0.975)*sd(sample_data)/sqrt(n))
})
observed_CIs[, 1:5]

#check how many contained the trut value
mean((observed_CIs[1, ] < mu) & (observed_CIs[2, ] > mu))

#quick function to color our intervals based on how they hit or miss
mycolor <- function(endpoints, par) {
  if (par < endpoints[1]) 
    "Red"  # if the mean is below the left endpoint of the confidence interval
  else if (par > endpoints[2]) 
    "Orange"  # if the mean is above the right endpoint of the confidence interval
  else "Black"  # if the mean lies between the endpoints
}

#Load the plotrix package, which contains the plotCI function.
require(plotrix)
plotCI(x = 1:N, 
       y = colMeans(observed_CIs), 
       li = observed_CIs[1, ], 
       ui = observed_CIs[2, ],
       col = apply(FUN = mycolor, X = observed_CIs, MARGIN = 2, par = mu), 
       ylab = "BMI",
       xlab = "Sampled Data Set",
       main = paste0("Visualization of 100 CIs\nProportion containing mu = ", 
                     mean((observed_CIs[1, ] < mu) & (observed_CIs[2, ] > mu)))
)
#draw a line for true mean
abline(h = mu, lwd = 2) 



##############################################################################

