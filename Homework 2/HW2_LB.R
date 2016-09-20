#Question 1

w0 <- 100
T <- 10000

sprintf("You started with $%s", w0)

w <- rep(0, T+1)
x <- rep(0, T)

w[1]=w0

for(t in 1:T){
  p <- runif(1,0,1)
  u <- runif(1,0,1)
  if(u <= p)  {
    print("you loose")
    x[t] <- 0
    w[t+1] <- w[t]+1
  } else {
    print ("you win")
    x[t] <- 1
    w[t+1] <- w[t]-1
  }
  print(sprintf("you now have $%s", w[t+1]))
}
time = 0:T
plot(time, w, type = "l")


#Question 2
w0 <- 100
T <- 10000

sprintf("You started with $%s", w0)

wB <- rep(0, T+1)
x <- rep(0, T)
u <- runif(1,0,1)

wB[1]=w0

for(t in 1:T){
  p <- runif(1,0,1)
  if(u <= p)  {
    print("you loose")
    x[t] <- 0
    wB[t+1] <- wB[t]+1
  } else {
    print ("you win")
    x[t] <- 1
    wB[t+1] <- wB[t]-1
  }
  print(sprintf("you now have $%s", w[t+1]))
}
time = 0:T
plot(time, wB, type = "l")

#Question 3
minimum <- minval(w, wB)
maximum <- maxval(w, wB)

time = 0:T
plot(time, w, type = "l", col="pink", ylim = c(-2000,1000), xlim = c(0,10000))
     lines(time, wB)

#Question 4

     w0 <- 100
     T <- 10000
     R <- 200
     sprintf("You started with $%s", w0)
     
     wD <- rep(0, T+1)
     p <- runif(1,0,1)
     
     K<- c(0, R)
     
     wD[1]=w0
     for(j in 1:R){
       wD<-rep(0, T+1) 
       wD[1]<-w0     
       
       for(t in 1:T){
       u <- runif(1,0,1)
       if(u <= p)  {
         print("you loose")
         wD[t+1] <- wD[t]+1
       } else {
         print ("you win")
         wD[t+1] <- wD[t]-1
       }
       print(sprintf("you now have $%s", w[t+1]))
       }
     }
     K[j]<-wD[T+1]
     ave<- mean(K)

     #Question 5
     
     w0 <- 100
     T <- 10000
     R <- 200
     sprintf("You started with $%s", w0)
     
     K<- c(0, R)
     
     wE[1]=w0
     for(j in 1:R){
       wE <- rep(0, T+1)
       p <- runif(1,0,1)
       wE[1]<-w0     
       
       for(t in 1:T){
         u <- runif(1,0,1)
         if(u <= p)  {
           print("you loose")
           wE[t+1] <- wE[t]+1
         } else {
           print ("you win")
           wE[t+1] <- wE[t]-1
         }
         print(sprintf("you now have $%s", w[t+1]))
       }
     }
     K[j]<-wE[T+1]
     ave<- mean(K)
     
