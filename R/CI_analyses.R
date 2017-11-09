library(tidyverse)

bootstrap_ci <- function(N, rate, alpha = 0.05){
  x <- rexp(n, rate = rate)
  x_bar <- mean(x)
  # Number of bootstrap samples
  nb <- 10000
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
  return(c(ci[2], ci[1]))
}

coverage_probability <- function(N, rate, ci_fun = "bootstrap_ci", alpha = 0.05, B = 10000){
  # Match input function to actual function
  fun <- tryCatch(match.fun(ci_fun), error = function(e) print(paste0("ci_fun: '", ci_fun, "' does not exist")))
  exp_val <- 1/rate
  conf_ints <- replicate(B, fun(N, rate)) %>%
    t() %>% data.frame()
  names(conf_ints) <- c("lower", "upper")
  conf_ints <- mutate(conf_ints, hit = (exp_val >= lower & exp_val <= upper))
  coverage <- conf_ints$hit %>% mean()
  return(coverage)
}

avg_coverage_probability <- function(){}


set.seed(123)
n <- 16
lambda <- 1/12
beta <- 1/lambda

bootstrap_ci(n, lambda)

t1 <- Sys.time()
coverage_probability(n, lambda, "bootstrap_ci")
print(difftime(Sys.time(), t1, units = "secs"))
