library(tidyverse)
#read the data
CHIS_data <- read_csv("/Users/saurabhgupta/projects/github/StatisticsFundamentals/Assignment2/Task1/CHIS.csv") %>% select(-1)
CHIS_data

hist(CHIS_data$BMI, main = "BMI Data", xlab = "BMI", breaks = 20)

mu <- mean(CHIS_data$BMI)
mu
set.seed(3)
n <- 25
sample_data <- sample(CHIS_data$BMI, size = n, replace = FALSE)
sample_data
mean(sample_data)
sd(sample_data)/sqrt(n)
c(mean(sample_data)-qnorm(0.975)*sd(sample_data)/sqrt(n),
  mean(sample_data)+qnorm(0.975)*sd(sample_data)/sqrt(n))


N <- 100
observed_CIs <- replicate(N, {
  sample_data <- sample(CHIS_data$BMI, size = n, replace = FALSE)
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


N <- 100
observed_CIs <- replicate(N, {
  sample_data <- sample(CHIS_data$BMI, size = n, replace = FALSE)
  c(mean(sample_data)-qnorm(0.975)*sd(sample_data)/sqrt(n),
    mean(sample_data)+qnorm(0.975)*sd(sample_data)/sqrt(n))
})

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

N <- 10000
observed_CIs <- replicate(N, {
  sample_data <- sample(CHIS_data$BMI, size = n, replace = FALSE)
  c(mean(sample_data)-qnorm(0.975)*sd(sample_data)/sqrt(n),
    mean(sample_data)+qnorm(0.975)*sd(sample_data)/sqrt(n))
})
#check how many contained the true value
mean((observed_CIs[1, ] < mu) & (observed_CIs[2, ] > mu))


N <- 100
n <- 100
observed_CIs <- replicate(N, {
  sample_data <- sample(CHIS_data$BMI, size = n, replace = FALSE)
  c(mean(sample_data)-qnorm(0.975)*sd(sample_data)/sqrt(n),
    mean(sample_data)+qnorm(0.975)*sd(sample_data)/sqrt(n))
})

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


N <- 10000
n <- 100
observed_CIs <- replicate(N, {
  sample_data <- sample(CHIS_data$BMI, size = n, replace = FALSE)
  c(mean(sample_data)-qnorm(0.975)*sd(sample_data)/sqrt(n),
    mean(sample_data)+qnorm(0.975)*sd(sample_data)/sqrt(n))
})
#check how many contained the true value
mean((observed_CIs[1, ] < mu) & (observed_CIs[2, ] > mu))