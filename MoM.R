
#Constant n,u. Varying k
library(gridExtra)
library(tidyverse)

momest_exp <- function(v,k){
  return((factorial(k)/(sum(v^k)/length(v)))^(1/k))
}

k <- 10
moment_number <- seq(1,k,by=1)
sample_size <- c(10,100,1000,10000,100000)
par(mfrow = c(2,3))
g <- list()
c <- 1
plotlabel <- data.frame(text = c("MLE","MLE","MLE","MLE","MLE","Unbiased Correction for MLE", 
                                 "Unbiased Correction for MLE","Unbiased Correction for MLE",
                                 "Unbiased Correction for MLE","Unbiased Correction for MLE"), 
                        x = c(10,10,10,10,10,8,5,8,8,8), 
                        y = c(0.08,0.0475,0.048,0.05,0.0503,0.0775,0.047,0.048,0.0495,0.05))
for(i in sample_size){
  set.seed(123)
  exp_sample <- rexp(i, rate = 0.05)
  moment_estimations <- data.frame(degree = 1:k, moments = matrix(unlist(lapply(moment_number,momest_exp,v=exp_sample))))
  g[[c]] <- ggplot(data = moment_estimations, aes(x=degree,y=moments)) + 
              geom_point(col = "darkblue") + 
              geom_line(col = "blue") +
              ggtitle(paste0("sample size = ",i)) + 
              geom_hline(yintercept = 1/mean(exp_sample), lty = 2, col="darkred") +
              geom_hline(yintercept = (length(exp_sample) - 1)/sum(exp_sample), lty = 2, col="darkgreen") +
              geom_text(data=plotlabel[c,], aes(x,y,label=text), size = 3, col="darkred") +
              geom_text(data=plotlabel[c + 5,], aes(x,y,label=text), size = 3, col="darkgreen") +
              theme_bw()
              c <- c + 1
}

grid.arrange(g[[1]], g[[2]], g[[3]], g[[4]], g[[5]], top = "Moment Estimations V/S Moment degree (Actual rate = 0.05)")

#Constant u. Varying n,k.
k <- 10
moment_number <- seq(1,k,by=1)
cols <- c(paste0("k",1:k))
sample_size <- seq(1000,100000,by=1000)
moment_estimations <- data.frame(matrix(0, nrow = length(sample_size), ncol = k + 1))
moment_estimations[,1] <- sample_size
colnames(moment_estimations) <- c("n",cols)
library(dplyr)
c <- 1
for (i in sample_size) {
  exp_sample <- rexp(i, rate = 0.05)
  for (j in moment_number) {
    moment_estimations[c,j+1] <- momest_exp(exp_sample,j)
  }
  c <- c + 1
}

plotlabel <- data.frame(text = c("1","2","3","4","5","6","7","8","9","10"), 
                        x = c(rep(3,10)), 
                        y = c(0.05,0.051,0.052,0.0535,0.0555,0.057,0.0595,0.0615,0.0635,0.066))

moment_estimations %>% ggplot(aes(x = n)) + 
  geom_line(aes(y=k1),col="violet") + 
  geom_line(aes(y=k2),col="blue") +
  geom_line(aes(y=k3),col="green") + 
  geom_line(aes(y=k4),col="yellow") + 
  geom_line(aes(y=k5),col="orange") + 
  geom_line(aes(y=k6),col="red") + 
  geom_line(aes(y=k7),col="cyan") + 
  geom_line(aes(y=k8),col="magenta") +
  geom_line(aes(y=k9),col="black") + 
  geom_line(aes(y=k10),col="grey50") + 
  xlab("Sample size") + 
  ylab("Estimator value") + 
  ggtitle("Variation of first 10 moment estimators with sample size(Actual rate = 0.05)") + 
  theme_bw() + 
  geom_text(data=plotlabel, aes(x,y,label=text), size = 3, col="darkred")

#Constant n. Varying u,k.
#k <- 10
#moment_number <- seq(1,k,by=1)
#cols <- c(paste0("k",1:k))
#rates <- seq(1,10,by=0.1)
#moment_estimations <- data.frame(matrix(0, nrow = length(rates), ncol = k + 1))
#moment_estimations[,1] <- rates
#colnames(moment_estimations) <- c("rates",cols)
#library(dplyr)
#c <- 1
#for (i in rates) {
#  exp_sample <- rexp(100, rate = rates[i])
#  for (j in moment_number) {
#    moment_estimations[c,j+1] <- momest_exp(exp_sample,j)
#  }
#  c <- c + 1
#}

#moment_estimations %>% ggplot(aes(x = rates)) + geom_line(aes(y=k1),col="violet") + geom_line(aes(y=k2),col="blue") +
#  geom_line(aes(y=k3),col="green") + geom_line(aes(y=k4),col="yellow") + geom_line(aes(y=k5),col="orange") + 
#  geom_line(aes(y=k6),col="red") + geom_line(aes(y=k7),col="cyan") + geom_line(aes(y=k8),col="magenta") +
#  geom_line(aes(y=k9),col="black") + geom_line(aes(y=k10),col="white") + xlab("Rate") + ylab("Estimator value") + 
#  ggtitle("Variation of first 10 moment estimators with rate(n=100)")
