set.seed(123)

# Generate original sample
n <- 1000
x <- rexp(n, rate = (1/12))
x_bar <- mean(samp)

# Number of bootstrap samples
nb <- 10000
# Boostraps
boot_straps <- sample(x, n * nb, replace = TRUE) %>%
  matrix(nrow = n, ncol = nb)

# Get means of columns 
means <- colMeans(boot_straps)
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
