# Imports
library(tidyverse)

# Confidence interval functions
bootstrap_ci <- function(N, rate, alpha = 0.05){
  # Function to calculate bootstrap CI
  x <- rexp(N, rate = rate)
  x_bar <- mean(x)
  # Number of bootstrap samples
  nb <- 1000
  # Take boostrap samples
  bootstrap_samples <- sample(x, N * nb, replace = TRUE) %>%
    matrix(nrow = N, ncol = nb)
  # Get means of columns 
  means <- colMeans(bootstrap_samples)
  # Get deltas (x* - x)
  deltas <- means - x_bar
  deltas <- sort(deltas)
  # Calculate CIs
  ci <- x_bar - quantile(deltas, probs = c(alpha / 2, 1 - (alpha / 2)))
  return(c(ci[2], ci[1]))
}
gamma_ci <- function(N, rate, alpha = 0.05){
  # Inspiration: https://math.stackexchange.com/questions/1288139/calculate-the-confidence-interval-of-parameter-of-exponential-distribution
  x <- rexp(N, rate = rate)
  x_bar <- mean(x)
  ci_rate <- qgamma(c(alpha / 2, 1 - (alpha / 2)), N, N) / x_bar
  ci_mean <- 1 / ci_rate
  return(c(ci_mean[2], ci_mean[1]))
}
wald_ci <- function(N, rate, alpha = 0.05){
  x <- rexp(N, rate = rate)
  x_bar <- mean(x)
  se <- sd(x)/sqrt(N)
  ci <- x_bar + c(-1, 1)*qnorm(1 - (alpha / 2))
  return(ci)
}

# Other functions
coverage_probability <- function(N, rate, ci_fun, alpha = 0.05, B = 10000){
  # Match input function to actual function
  fun <- tryCatch(match.fun(ci_fun), 
                  error = function(e) print(paste0("ci_fun: '", ci_fun, "' does not exist")))
  # Expected value
  exp_val <- 1 / rate
  # Calculate B confidence intervals and put in dataframe
  conf_ints <- replicate(B, fun(N, rate)) %>%
    t() %>% data.frame()
  names(conf_ints) <- c("lower", "upper")
  # Calculate hit rates (expected value inside confidence interval)
  conf_ints <- mutate(conf_ints, hit = (exp_val >= lower & exp_val <= upper))
  coverage <- conf_ints$hit %>% mean()
  return(coverage)
}

#avg_coverage_probability <- function(){}

# Work
set.seed(123)

lambda <- 1 / 12
beta <- 1 / lambda
n <- 60

t1 <- Sys.time()
coverage_probability(n, lambda, "bootstrap_ci") # Takes a while
coverage_probability(n, lambda, "gamma_ci")
coverage_probability(n, lambda, "wald_ci")
print(difftime(Sys.time(), t1, units = "secs"))
