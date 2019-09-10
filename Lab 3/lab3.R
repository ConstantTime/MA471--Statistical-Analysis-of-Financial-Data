n <- 200

c = rnorm(n , 0 , 1)
for ( i in 1 : n ) {
  a = rnorm(1 , 0 , 1)
  
  if(a < 0.4) {
    c[i] = a
  }
  else {
    c[i] = rnorm(1 , 0 , 5)
  }
}

write.table(c , "data.txt")

hist(c, breaks = 100, main = "Mixture of Distributions", 
     xlab = "Values obtained")



#PART B with own built in function


library(mixtools)
gm<-normalmixEM(c,k=2,lambda=c(0.4 , 0.6),mu=c(0 , 0),sigma=c(1 , 5))



#PART B with written function

# modified sum only considers finite values
sum.finite <- function(x) {
  sum(x[is.finite(x)])
}


x = c

Q <- 0
k <- 2

mu1 <- 0
mu2 <- 0

sigma1 <- 1
sigma2 <- 5

pi1 <- 0.4
pi2 <- 0.6
# starting value of expected value of the log likelihood
Q[2] <- sum.finite(log(pi1)+log(dnorm(x, mu1, sigma1))) + sum.finite(log(pi2)+log(dnorm(x, mu2, sigma2)))


while (Q[k] != Inf && Q[k - 1] != Inf && abs(Q[k]-Q[k-1]) >= 1e-6) {
  cat(Q[k])
  cat(" ")
  cat(Q[k - 1])
  cat("\n")
  # E step
  comp1 <- pi1 * dnorm(x, mu1, sigma1)
  comp2 <- pi2 * dnorm(x, mu2, sigma2)
  comp.sum <- comp1 + comp2
  
  p1 <- comp1/comp.sum
  p2 <- comp2/comp.sum
  
  # M step
  pi1 <- sum.finite(p1) / length(x)
  pi2 <- sum.finite(p2) / length(x)
  
  mu1 <- sum.finite(p1 * x) / sum.finite(p1)
  mu2 <- sum.finite(p2 * x) / sum.finite(p2)
  
  sigma1 <- sqrt(sum.finite(p1 * (x-mu1)^2) / sum.finite(p1))
  sigma2 <- sqrt(sum.finite(p2 * (x-mu2)^2) / sum.finite(p2))
  
  p1 <- pi1 
  p2 <- pi2
  
  k <- k + 1
  Q[k] <- sum(log(comp.sum))
}

#histogram of the distribution we received
hist(x, prob=T, breaks=32, xlim=c(range(x)[1], range(x)[2]), main='')
lines(density(x), col="green", lwd=2)
x1 <- seq(from=range(x)[1], to=range(x)[2], length.out=1000)
y <- pi1 * dnorm(x = x1, mean=mu1, sd=sigma1) + pi2 * dnorm(x1, mean=mu2, sd=sigma2)
lines(x1, y, col="red", lwd=2)
legend('topright', col=c("green", 'red'), lwd=2, legend=c("kernal", "fitted"))