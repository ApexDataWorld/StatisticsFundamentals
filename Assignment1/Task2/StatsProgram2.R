#changing sample size, same code

n <- 30
#set our true lambda value
lambda <- 1

#now repeat the process!
N <- 10000 #number of estimates to find
results <- replicate(N, {
  sim_data <- rpois(n, lambda)
  list(lambda1 = mean(sim_data), lambda2 = mean(sim_data^2) - (mean(sim_data))^2)
})

lambda1 <- unlist(results[1,])
lambda2 <- unlist(results[2,])

#plot the values on a histogram to compare
par(mfrow = c(1, 2))
hist(lambda1, main = bquote(atop(~ lambda[1] ~ " values with ", "true " ~ lambda ~ "=" ~ .(lambda) ~ " with n = " ~ .(n))))
abline(v = lambda, col = "blue", lwd = 2)
abline(v = mean(lambda1), col = "red", lwd = 2, lty = "dashed")
text(paste0("SE = ", round(sd(lambda1), 3)), x = mean(lambda1) +3*sd(lambda1), y = 1500, cex = 0.8)
hist(lambda2, main = bquote(atop(~ lambda[2] ~ " values with ", "true " ~ lambda ~ "=" ~ .(lambda) ~ " with n = " ~ .(n))))
abline(v = lambda, col = "blue", lwd = 2)
abline(v = mean(lambda2), col = "red", lwd = 2, lty = "dashed")
text(paste0("SE = ", round(sd(lambda2), 3)), x = mean(lambda2) + 3*sd(lambda2), y = 1500, cex = 0.8)

#####################
##Changing sample size again

n <- 100
#set our true lambda value
lambda <- 1

#now repeat the process!
N <- 10000 #number of estimates to find
results <- replicate(N, {
  sim_data <- rpois(n, lambda)
  list(lambda1 = mean(sim_data), lambda2 = mean(sim_data^2) - (mean(sim_data))^2)
})

lambda1 <- unlist(results[1,])
lambda2 <- unlist(results[2,])

#plot the values on a histogram to compare
par(mfrow = c(1, 2))
hist(lambda1, main = bquote(atop(~ lambda[1] ~ " values with ", "true " ~ lambda ~ "=" ~ .(lambda) ~ " with n = " ~ .(n))))
abline(v = lambda, col = "blue", lwd = 2)
abline(v = mean(lambda1), col = "red", lwd = 2, lty = "dashed")
text(paste0("SE = ", round(sd(lambda1), 3)), x = mean(lambda1) +3*sd(lambda1), y = 1500, cex = 0.8)
hist(lambda2, main = bquote(atop(~ lambda[2] ~ " values with ", "true " ~ lambda ~ "=" ~ .(lambda) ~ " with n = " ~ .(n))))
abline(v = lambda, col = "blue", lwd = 2)
abline(v = mean(lambda2), col = "red", lwd = 2, lty = "dashed")
text(paste0("SE = ", round(sd(lambda2), 3)), x = mean(lambda2) + 3*sd(lambda2), y = 1500, cex = 0.8)


#######################
##Change lambda value

n <- 10
#set our true lambda value
lambda <- 20

#now repeat the process!
N <- 10000 #number of estimates to find
results <- replicate(N, {
  sim_data <- rpois(n, lambda)
  list(lambda1 = mean(sim_data), lambda2 = mean(sim_data^2) - (mean(sim_data))^2)
})

lambda1 <- unlist(results[1,])
lambda2 <- unlist(results[2,])

par(mfrow = c(1, 2))
hist(lambda1, main = bquote(atop(~ lambda[1] ~ " values with ", "true " ~ lambda ~ "=" ~ .(lambda) ~ " with n = " ~ .(n))))
abline(v = lambda, col = "blue", lwd = 2)
abline(v = mean(lambda1), col = "red", lwd = 2, lty = "dashed")
text(paste0("SE = ", round(sd(lambda1), 3)), x = mean(lambda1) +3*sd(lambda1), y = 1500, cex = 0.8)
hist(lambda2, main = bquote(atop(~ lambda[2] ~ " values with ", "true " ~ lambda ~ "=" ~ .(lambda) ~ " with n = " ~ .(n))))
abline(v = lambda, col = "blue", lwd = 2)
abline(v = mean(lambda2), col = "red", lwd = 2, lty = "dashed")
text(paste0("SE = ", round(sd(lambda2), 3)), x = mean(lambda2) + 3*sd(lambda2), y = 1500, cex = 0.8)


########################
##Change lambda value

n <- 50
#set our true lambda value
lambda <- 20

#now repeat the process!
N <- 10000 #number of estimates to find
results <- replicate(N, {
  sim_data <- rpois(n, lambda)
  list(lambda1 = mean(sim_data), lambda2 = mean(sim_data^2) - (mean(sim_data))^2)
})

lambda1 <- unlist(results[1,])
lambda2 <- unlist(results[2,])

#plot the values on a histogram to compare
par(mfrow = c(1, 2))
hist(lambda1, main = bquote(atop(~ lambda[1] ~ " values with ", "true " ~ lambda ~ "=" ~ .(lambda) ~ " with n = " ~ .(n))))
abline(v = lambda, col = "blue", lwd = 2)
abline(v = mean(lambda1), col = "red", lwd = 2, lty = "dashed")
text(paste0("SE = ", round(sd(lambda1), 3)), x = mean(lambda1) +3*sd(lambda1), y = 1500, cex = 0.8)
hist(lambda2, main = bquote(atop(~ lambda[2] ~ " values with ", "true " ~ lambda ~ "=" ~ .(lambda) ~ " with n = " ~ .(n))))
abline(v = lambda, col = "blue", lwd = 2)
abline(v = mean(lambda2), col = "red", lwd = 2, lty = "dashed")
text(paste0("SE = ", round(sd(lambda2), 3)), x = mean(lambda2) + 3*sd(lambda2), y = 1500, cex = 0.8)
