set.seed(123)

# Generate original sample
n <- 1000
lambda <- 1/12
x <- rexp(n, rate = lambda)
x_bar <- mean(x)

# Number of bootstrap samples
nb <- 100000
# Take boostrap samples
bootstrap_samples <- sample(x, n * nb, replace = TRUE) %>%
  matrix(nrow = n, ncol = nb)

# Get means of columns 
means <- colMeans(bootstrap_samples)
# Get deltas (x* - x)
deltas <- means - x_bar
deltas <- sort(deltas)
# Calculate CIs
ci <- x_bar - quantile(deltas, probs = c(0.025, 0.975))
# Print
print(paste0("Xbar = ", round(x_bar, 2), 
             " (95% CI: ", 
             round(ci[2], 2), " - ", 
             round(ci[1], 2), ")"))
