#HW3_LB
#Question 1
# R incorporates a constant, whereas the line of code didn't.

#Question 2
# Define the vector as a matrix where one colum is contained of 1s and
# the other one with xs

# Load the package
# NOTE: Uncomment this line if need to install package
#install.packages("MASS", repos="http://cran.rstudio.com/")
require(MASS)

# Set the correlation parameter and mean
beta = 0.5
SIGMA = matrix(c(1,beta,beta,1), ncol=2)
MU = c(2.0, 1.0)

# Set the sample size
N = 50

# Draw the sample
out <- mvrnorm(N, mu = MU, Sigma = SIGMA)

plot(out)
abline(lm(out[,2]~out[,1]), col="red")


# Our data set is named `out`, which we split into y and X
y <- out[, 2]
X <- cbind(rep(1, N),out[, 1])  ##The only significant modification has to be 
#made at this point: a column of "1" is 
#added to X 

# Now carry out intermediate calculations
XT = t(X)
XTX = XT%*%X
invXTX = solve(XTX)
XTy = XT%*%y
beta = invXTX %*% XTy


plot(out)
abline(lm(out[,2]~out[,1]), col="red") # regression line (y~x) 
abline(a=beta[1], b=beta[2], col="blue")

#Question 3


MU <- c(4, 2, 1)

c12<-0.3
c13<-0.6
c23<-0.9

SIGMA = matrix(c(1, c12, c13, c12, 1, c23, c13, c23, 1), ncol = 3)

N <- 30


S<- mvrnorm(N, mu = MU, Sigma = SIGMA)



Y<-S[, 1]
X<- cbind(rep(1, N), S[, c(2, 3)])
X1<-S[, 2]
X2<-S[, 3]


model<-lm(Y~X1+X2)
summary(model)

model$coefficients


XT = t(X)
XTX = XT%*%X
invXTX = solve(XTX)
XTy = XT%*%Y
beta = invXTX %*% XTy
beta


mat<-cbind(model$coefficients, beta)
print(mat)
 
mat[,1]==mat[,2]




#Question 4




general_OLS<-function(nvar, 
                      MU,   
                      SIGMA,
                      N    
){
  
  
  
  if(is.na(nvar)){
    nvar<-floor(runif(1,0,1)*100+2)
  }
  
  if(is.na(MU)){
    MU<-runif(nvar, 5, 25)
  }
  
  
  if(is.na(SIGMA)){
    coeff_sigma<- runif(nvar*nvar, 0, 1)
    SIG<- matrix(coeff_sigma, ncol = nvar, nrow = nvar)
    SIGMA<-SIG%*%t(SIG)
  }
  if(is.na(N)){
    N<-floor(runif(1,0,1)*1000+5*nvar)
  } 
  
  sample<- mvrnorm(N, mu = MU, Sigma = SIGMA)
  
  Y<- sample[, 1]
  X<- cbind(rep(1, N), sample[, c(2:(nvar))])
  XT = t(X)
  XTX = XT%*%X
  invXTX = solve(XTX)
  XTy = XT%*%Y
  beta = invXTX %*% XTy
  return(beta)
  
  if(nvar == 2){
    graph<-plot(sample)
    abline(lm(sample[,2]~sample[,1]), col="red") 
    abline(a=beta[1], b=beta[2], col="blue")
  }
  if(nvar == 3){
    library(plot3D)
    graph<-persp3D(x = sample[, 2], y = sample[, 3], z = sample[, 1], colvar = z)
  }
  return(graph)
}

general_OLS(nvar = 2, MU = NA, SIGMA = NA, N = NA)




