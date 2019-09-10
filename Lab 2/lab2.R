library(metRology)
library(smoothmest)
options(scipen=999)
df <- read.table('data.txt' , header = TRUE)

mean_c = mean(df[,'C'])
var_c = var(df[,'C'])
mean_sp = mean(df[,"SP"])
var_sp = var(df[,"SP"])


h <- hist(df[, 'C'], xlab = 'C returns', main = 'Returns of C with normal curve')
x <- seq(-0.4 , 0.4 , length.out = 801)
y <- dnorm(x , mean = mean_c , sd = sqrt(var_c))
y <- y * diff(h$mids[1:2]) * length(df[, 'C']) 
lines(x , y, col = "black", lwd = 2)

q1 <- c(1:50)
j = -0;
for (i in c(1:50)) {
  if(i == 1) {
    q1[i] = 0;
  }
  else {
    q1[i] = q1[i - 1];
  }
  for (elem in df[, 'C']) {
    if(elem < j) {
      q1[i] = q1[i] + 1;
    }
  }
  j = j + 0.001;
}
q2 <- c(1:50)
j = 1;
for (i in c(1:50)) {
  if(i == 1) {
    q2[i] = 0;
  }
  else {
    q2[i] = q2[i - 1];
  }
  for (elem in y) {
    if(elem < j) {
      q2[i] = q2[i] + 1;
    }
  }
  j = j + 1;
}
plot(q1 , q2 , xlab = "Empricial Q" , ylab = "Normal Q", main = "QQ plot")


hist(df[, 'SP'], xlab = 'SP returns', main = 'Returns of SP with normal curve')
x <- seq(-0.15 , 0.15 , by = 0.001)
y <- dnorm(x , mean = mean_sp , sd = sqrt(var_sp))
y <- y * diff(h$mids[1:2]) * length(df[, 'SP']) 
lines(x , y, col = "black", lwd = 2)

q1 <- c(1:50)
j = -0;
for (i in c(1:50)) {
  if(i == 1) {
    q1[i] = 0;
  }
  else {
    q1[i] = q1[i - 1];
  }
  for (elem in df[, 'SP']) {
    if(elem < j) {
      q1[i] = q1[i] + 1;
    }
  }
  j = j + 0.001;
}
q2 <- c(1:50)
j = 1;
for (i in c(1:50)) {
  if(i == 1) {
    q2[i] = 0;
  }
  else {
    q2[i] = q2[i - 1];
  }
  for (elem in y) {
    if(elem < j) {
      q2[i] = q2[i] + 1;
    }
  }
  j = j + 1;
}
plot(q1 , q2 , xlab = "Empricial Q for SP" , ylab = "Normal Q for SP", main = "QQ plot")



plot(df[, 'C'], xlab = ' Date', ylab = 'C returns', main = 'Returns of C with t distribution')
plot(df[, 'SP'], xlab = ' Date', ylab = 'SP returns', main = 'Returns of SP with t distribution')


# For C - t distribution
h <- hist(df[, 'C'], xlab = 'Returns of C', main = 'C returns')
x <- seq(-0.4 , 0.4 , by = 0.001)
y <- dt.scaled(x , df = 1 , mean = mean_c , sd = sqrt(var_c))
y <- y * diff(h$mids[1:2]) * length(df[, 'C']) 
lines(x , y , col = "red" , lwd = 2)



q1 <- c(1:50)
j = -0;
for (i in c(1:50)) {
  if(i == 1) {
    q1[i] = 0;
  }
  else {
    q1[i] = q1[i - 1];
  }
  for (elem in df[, 'C']) {
    if(elem < j) {
      q1[i] = q1[i] + 1;
    }
  }
  j = j + 0.001;
}
q2 <- c(1:50)
j = 1;
for (i in c(1:50)) {
  if(i == 1) {
    q2[i] = 0;
  }
  else {
    q2[i] = q2[i - 1];
  }
  for (elem in y) {
    if(elem < j) {
      q2[i] = q2[i] + 1;
    }
  }
  j = j + 1;
}
plot(q1 , q2 , xlab = "Empricial Q for C" , ylab = "t dist Q for C", main = "QQ plot")



# for SP - t distribution

hist(df[, 'SP'], xlab = 'Returns of SP', main = 'SP returns')
x <- seq(-0.15 , 0.15 , by = 0.001)
y <- dt.scaled(x , df = 1 , mean = mean_sp , sd = sqrt(var_sp))
y <- y * diff(h$mids[1:2]) * length(df[, 'SP']) 
lines(x , y, col = "red", lwd = 2)


q1 <- c(1:50)
j = -0;
for (i in c(1:50)) {
  if(i == 1) {
    q1[i] = 0;
  }
  else {
    q1[i] = q1[i - 1];
  }
  for (elem in df[, 'SP']) {
    if(elem < j) {
      q1[i] = q1[i] + 1;
    }
  }
  j = j + 0.001;
}
q2 <- c(1:50)
j = 1;
for (i in c(1:50)) {
  if(i == 1) {
    q2[i] = 0;
  }
  else {
    q2[i] = q2[i - 1];
  }
  for (elem in y) {
    if(elem < j) {
      q2[i] = q2[i] + 1;
    }
  }
  j = j + 1;
}
plot(q1 , q2 , xlab = "Empricial Q for SP" , ylab = "t dist Q for SP", main = "QQ plot")



# for C - Cauchy distribution
h <- hist(df[, 'C'], xlab = 'C returns', main = 'Returns of C with cauchy distribution')
x <- seq(-0.4 , 0.4 , by = 0.001)
y <- dcauchy(x , location = mean_c , scale = sqrt(var_c))
y <- y * diff(h$mids[1:2]) * length(df[, 'C']) 
lines(x , y, col = "green", lwd = 2)


q1 <- c(1:50)
j = -0;
for (i in c(1:50)) {
  if(i == 1) {
    q1[i] = 0;
  }
  else {
    q1[i] = q1[i - 1];
  }
  for (elem in df[, 'C']) {
    if(elem < j) {
      q1[i] = q1[i] + 1;
    }
  }
  j = j + 0.001;
}
q2 <- c(1:50)
j = 1;
for (i in c(1:50)) {
  if(i == 1) {
    q2[i] = 0;
  }
  else {
    q2[i] = q2[i - 1];
  }
  for (elem in y) {
    if(elem < j) {
      q2[i] = q2[i] + 1;
    }
  }
  j = j + 1;
}
plot(q1 , q2 , xlab = "Empricial Q for C" , ylab = "cauchy Q for C", main = "QQ plot")



# for SP - cauchy distribution

hist(df[, 'SP'], xlab = 'SP returns', main = 'Returns of SP with cauchy distribution')
x <- seq(-0.15 , 0.15 , by = 0.001)
y <- dnorm(x , mean = mean_sp , sd = sqrt(var_sp))
y <- y * diff(h$mids[1:2]) * length(df[, 'SP']) 
lines(x , y, col = "green", lwd = 2)



q1 <- c(1:50)
j = -0;
for (i in c(1:50)) {
  if(i == 1) {
    q1[i] = 0;
  }
  else {
    q1[i] = q1[i - 1];
  }
  for (elem in df[, 'SP']) {
    if(elem < j) {
      q1[i] = q1[i] + 1;
    }
  }
  j = j + 0.001;
}
q2 <- c(1:50)
j = 1;
for (i in c(1:50)) {
  if(i == 1) {
    q2[i] = 0;
  }
  else {
    q2[i] = q2[i - 1];
  }
  for (elem in y) {
    if(elem < j) {
      q2[i] = q2[i] + 1;
    }
  }
  j = j + 1;
}
plot(q1 , q2 , xlab = "Empricial Q for SP" , ylab = "Cauchy Q for SP", main = "QQ plot")



#For C - double exponential return
h <- hist(df[, 'C'], xlab = 'Returns of C', main = 'Returns of C with double exponential')
x <- seq(-0.4 , 0.4 , by = 0.001)
y <- ddoublex(x, mu=mean_c, lambda=sqrt(var_c))
y <- y * diff(h$mids[1:2]) * length(df[, 'C']) 
lines(x , y , col = "blue" , lwd = 2)


q1 <- c(1:50)
j = -0;
for (i in c(1:50)) {
  if(i == 1) {
    q1[i] = 0;
  }
  else {
    q1[i] = q1[i - 1];
  }
  for (elem in df[, 'C']) {
    if(elem < j) {
      q1[i] = q1[i] + 1;
    }
  }
  j = j + 0.001;
}
q2 <- c(1:50)
j = 1;
for (i in c(1:50)) {
  if(i == 1) {
    q2[i] = 0;
  }
  else {
    q2[i] = q2[i - 1];
  }
  for (elem in y) {
    if(elem < j) {
      q2[i] = q2[i] + 1;
    }
  }
  j = j + 1;
}
plot(q1 , q2 , xlab = "Empricial Q for C" , ylab = "Double exponential Q for C", main = "QQ plot")



#For SP - double exponential return
hist(df[, 'SP'], xlab = 'Returns of SP', main = 'Returns of SP with double exponential')
x <- seq(-0.15 , 0.15 , by = 0.001)
y <- ddoublex(x, mu=mean_sp, lambda=sqrt(var_sp))
y <- y * diff(h$mids[1:2]) * length(df[, 'SP'])
lines(x , y, col = "blue", lwd = 2)


q1 <- c(1:50)
j = -0;
for (i in c(1:50)) {
  if(i == 1) {
    q1[i] = 0;
  }
  else {
    q1[i] = q1[i - 1];
  }
  for (elem in df[, 'SP']) {
    if(elem < j) {
      q1[i] = q1[i] + 1;
    }
  }
  j = j + 0.001;
}
q2 <- c(1:50)
j = 1;
for (i in c(1:50)) {
  if(i == 1) {
    q2[i] = 0;
  }
  else {
    q2[i] = q2[i - 1];
  }
  for (elem in y) {
    if(elem < j) {
      q2[i] = q2[i] + 1;
    }
  }
  j = j + 1;
}
plot(q1 , q2 , xlab = "Empricial Q for SP" , ylab = "Double exponential Q for SP", main = "QQ plot")




# for mixture of 2 normal distributions

h <- hist(df[, 'C'], xlab = 'C returns', main = 'Returns of C with 2 normal curve')
x <- seq(-0.4 , 0.4 , by = 0.001)
y1 <- dnorm(x , mean = mean_c , sd = sqrt(var_c))
y2 <- dnorm(x , mean = mean_c , sd = 0.01)
y <- 0.9 * y1 + 0.1 * y2
y <- y * diff(h$mids[1:2]) * length(df[, 'C']) 
lines(x , y, col = "red", lwd = 2)


q1 <- c(1:50)
j = -0;
for (i in c(1:50)) {
  if(i == 1) {
    q1[i] = 0;
  }
  else {
    q1[i] = q1[i - 1];
  }
  for (elem in df[, 'C']) {
    if(elem < j) {
      q1[i] = q1[i] + 1;
    }
  }
  j = j + 0.001;
}
q2 <- c(1:50)
j = 1;
for (i in c(1:50)) {
  if(i == 1) {
    q2[i] = 0;
  }
  else {
    q2[i] = q2[i - 1];
  }
  for (elem in y) {
    if(elem < j) {
      q2[i] = q2[i] + 1;
    }
  }
  j = j + 1;
}
plot(q1 , q2 , xlab = "Empricial Q for C" , ylab = " 2 Normals Q for C", main = "QQ plot")



hist(df[, 'SP'], xlab = 'SP returns', main = 'Returns of SP with normal curve')
x <- seq(-0.15 , 0.15 , by = 0.001)
y1 <- dnorm(x , mean = mean_sp , sd = sqrt(var_sp))
y2 <- dnorm(x , mean = mean_sp , sd = 0.01)
y <- 0.9 * y1 + 0.1 * y2
y <- y * diff(h$mids[1:2]) * length(df[, 'SP']) 
lines(x , y, col = "red", lwd = 2)


q1 <- c(1:50)
j = -0;
for (i in c(1:50)) {
  if(i == 1) {
    q1[i] = 0;
  }
  else {
    q1[i] = q1[i - 1];
  }
  for (elem in df[, 'SP']) {
    if(elem < j) {
      q1[i] = q1[i] + 1;
    }
  }
  j = j + 0.001;
}
q2 <- c(1:50)
j = 1;
for (i in c(1:50)) {
  if(i == 1) {
    q2[i] = 0;
  }
  else {
    q2[i] = q2[i - 1];
  }
  for (elem in y) {
    if(elem < j) {
      q2[i] = q2[i] + 1;
    }
  }
  j = j + 1;
}
plot(q1 , q2 , xlab = "Empricial Q for SP" , ylab = "2 Normals Q for SP", main = "QQ plot")
