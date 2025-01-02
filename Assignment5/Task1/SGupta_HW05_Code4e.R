# Posterior parameters
alpha_post <- 6
beta_post <- 6

posterior_mode <- (alpha_post - 1) / (alpha_post + beta_post - 2)

# Plot posterior distribution
curve(dbeta(x, shape1 = alpha_post, shape2 = beta_post), 
      from = 0, to = 1, 
      main = "Posterior Distribution of Theta", 
      xlab = "Theta", ylab = "Density", col = "blue")

abline(v = posterior_mode, col = "green", lwd = 2, lty = 2)

# Add legend
legend("topright", legend = c("Posterior Mean", "Posterior Mode"), 
       col = c("red", "green"), lty = 2, lwd = 2)



