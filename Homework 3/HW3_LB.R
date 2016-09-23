#Homework3_LB
#Question 1

#' The first entry containing a letter or word.
#' @param pattern String: a pattern to search for
#' @param word String: a string in which to search
#' @return The index to the first occurence of pattern in word.
#' @seealso \code{\link{grep}} which this function wraps
#' @export
whereis <- function(pattern, word){
  return(grep(pattern, word))
}
whereis('I', 'Im lost')

#' @param x object that will be replicated
#' @param y number of times it will be replicated
repetition <- function(x, y){
  rep(x, y)
}
rep(1:90, 5)
print(x)

#' @param l A random number between 0 and 1.
#' @param b A random number between 0 and 1.
#' @return The sum of {l} and {b}.
#' add(1, 1)
#' add(10, 1)
 add<- function(l, b) {
   l<-runif(1)
   b<-runif(1)
   l+b
}
add(l,b)

#' @param f A random number between 0 and 1.
#' @param g A random number between 0 and 1.
#' @return The maximum of {f} and {g}.
max(f, g)<- function(x, y) 

#' @param  A number.
#' @param b A number.
#' @return The sum of {l} and {b}.
our_sum <- function(x){
  summation <- 0
  for(i in x){
    summation <- summation + i
  }
  return(summation)
}



#Question 2

rand_sample <- function(n, F_inv=function(x) x){
  unif_sample <- runif(n, 0, 10)
  return(F_inv(unif_sample))
}

rand_sample(5)

F_inv_unif <- function(x, a, b) x*(b - a) + a
rand_sample(5, F_inv=function(x) F_inv_unif(x, 0, 10))

rand_sample(n=5)

rand_sample(5, function(x) F_inv_unif(x, 0, 10))

rand_sample <- function(n, F_inv=function(x) x){
  unif_sample <- runif(n)
  return(F_inv(unif_sample))
}

mc_mean <- function(n, F_inv=function(x) x){
  sample <- rand_sample(n, F_inv=function(x) F_inv(x))
  return(sum(sample)/n)
}

plot_results <- function(n_vec, F_inv=function(x) x){
  results <- rep(0, length(n_vec))
  i <- 1
  for(n in n_vec){
    results[i] <- mc_mean(n)
    i <- i + 1
  }
  plot(n_vec, results, type="l")
}

N <- seq(0, 66438, 333)

plot_results(N)