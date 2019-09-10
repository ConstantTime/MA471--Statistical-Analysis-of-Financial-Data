total_boxes = 3

al <- function(n) {
  x <- matrix(nrow = n , ncol = total_boxes)
  for (i in 1 : n) {
    for(j in 1 : total_boxes) {
      x[i, j] = 1;
    }
  }
  p <- c(0.5 , 0.3 , 0.2)
  for (i in 1 : total_boxes) {
    if(i != 1) {
      p[i] = p[i] + p[i - 1]
    }
  }
  for (i in 1 : n) {
    u = runif(total_boxes , 0 , 1)
    for (j in 1 : total_boxes) {
      for(k in 1 : total_boxes) {
        if(u[j] <= p[k]) {
          x[i, j] = k;
          break;
        }
      }
    }
  }
  
  return(x);
}

count = c(10 , 100 , 200 , 500 , 1000)

for (i in count) {
  cat("The final array for ")
  cat(i)
  cat(" values is \n")
  if(i == 10) {
    sprint(al(i))
  }
}

cat("Checking the correctness of these values\n")

for (i in count) {
  x = al(i);
  error <- matrix(nrow = 1 , ncol = total_boxes)
  mean <- matrix(nrow = 1 , ncol = total_boxes)
  mean[1 , 1] = 0
  mean[1 , 2] = 0
  mean[1 , total_boxes] = 0
  for (j in 1 : i) {
    mean[1 , 1] = mean[1 , 1] + x[j , 1]
    mean[1 , 2] = mean[1 , 2] + x[j , 2]
    mean[1 , total_boxes] = mean[1 , total_boxes] + x[j , total_boxes]
  }
  mean = mean / i
  error[1 , 1] = abs(mean[1 , 1] - total_boxes * 0.5) / mean[1 , 1]
  error[1 , 2] = abs(mean[1 , 2] - total_boxes * 0.3) / mean[1 , 2]
  error[1 , 3] = abs(mean[1 , 3] - total_boxes * 0.2) / mean[1 , 3]
  cat("The final array for error in ")
  cat(i)
  cat(" values is \n")
  print(error)
}

cat("\n")
cat("One can notice the error that has been decreasing with more and more values")





