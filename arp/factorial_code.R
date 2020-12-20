#Advanced R Programming Peer Graded Assignment: Part 1, Factorial Function

#utility functions

int_checker <- function(x) {
  #checks if the input is an integer or a whole number numeric. otherwise returns an error.
  if (class(x) == "integer") {
  }
  else if (class(x) == "numeric") {
    if(x == as.integer(x)){
    }
    else {
      stop("x must be an integer or of class numeric and a whole number.")
    }
  }
  else {
    stop("x must be an integer or of class numeric and a whole number.")
  }
}

purrr_checker <- function() {
  #checks if purrr is installed. if not, installs it and loads it
  if (require(purrr) == FALSE) {
    message("installing the purrr package.")
    install.packages(purrr)
    library(purrr)
  }
  #checks if an installed package is loaded. if it isn't, it loads it
  else if (!"purrr" %in% (.packages())) {
    message("loading the purrr package.")
    library(purrr)
  }
}

#1 Factorial_loop: a version that computes the factorial of an integer using looping (such as a for loop)

Factorial_loop <- function(n) {
  int_checker(n)
  if (n == 0) 1
  else {
    factorial <- n
    for (i in 1:n) {
      if (!n == 1) {
        n <- n - 1
        factorial <- factorial * n 
      }
      }
    factorial
  }
}

#2 Factorial_reduce: a version that computes the factorial using the reduce() function in the purrr package. Alternatively, you can use the Reduce() function in the base package.

library(purrr)

Factorial_reduce <- function(n) {
  int_checker(n)
  purrr_checker()
  
  reduce(seq_len(n), function(x, y) {
    x * y
  })
}

#3 Factorial_func: a version that uses recursion to compute the factorial.

Factorial_func <- function(n) {
  int_checker(n)
  if(n %in% c(0,1)) 1
  else n*Factorial_func(n-1)

}

#4 Factorial_mem: a version that uses memoization to compute the factorial.

mems = 1

Factorial_mem <- function(n) {
  int_checker(n)
  if (n %in% c(0,1)) 1
  if (length(mems) < n) mems <<- `length<-`(mems, n)
  if (!is.na(mems[n])) return (mems[n])
  x <- n*Factorial_mem(n-1)
  mems[n] <<- x
  x
}

#benchmarking and outputting the results
library(microbenchmark)

n <- 10
results <- microbenchmark(Factorial_loop(n),
                          Factorial_reduce(n),
                          Factorial_func(n),
                          Factorial_mem(n))
sink("factorial_output.txt")
cat("Results of different functions for calculating a factorial:", "\n")
results
sink()
