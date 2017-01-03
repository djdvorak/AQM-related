
dat.adv <- read.csv("advertising.csv")
Sales <- dat.adv$Sales
TV <- dat.adv$TV
Radio <- dat.adv$Radio
Newspaper <- dat.adv$Newspaper

X <- as.matrix(dat.adv[,2:4]) # design matrix
Y <- as.matrix(dat.adv[,5]) # response variable
#X <- cbind(1,dat.adv$TV,dat.adv$Radio,dat.adv$Newspaper)
X <- cbind(1, X) #rep(1,length(Y))
beta <- solve(t(X) %*% X)  %*% t(X) %*% Y
Y_hat <- X %*% beta
res_manual <- (Y - (X %*% beta))

data <- dat.adv

ssquares <- function(x){ # x is beta, vector of coefficients 
  n <- nrow(data) #200
  sum((data[,4] - cbind(1,data[,1])) %*% x)^2 / n
}

derivative <- function(x){ # x is a point
  n <- nrow(data) # 200
  c(sum(-2*(data[,4] = cbind(1, data[,1]) %*% x)), sum(-2*(data[,1])*(data[,4] - cbind(1,data[,1]) %*% x))) /n
}

gradient_descent <- function(func, derv, start, step=0.00001, tol=1e-8){
  pt1 <- start
  grdnt <- derv(pt1)
  pt2 <- c(pt1[1] - step*grdnt[1], pt1[2] - step*grdnt[2])
  while (abs(func(pt1)-func(pt2)) > tol)
  {
    pt1 <- pt2
    grdnt <- derv(pt1)
    pt2 <- c(pt1[1] - step*grdnt[1], pt1[2] - step*grdnt[2])
    print(func(pt2)) #print progress
  }
}
  
  
result <- gradient_descent(
  ssquares, # the function to optimize
  derivative, # the gradient of the funcion
  c(0,0), # start point of theplot_loss(simple_ex) search
  0.00001, # step size (alpha)
  1e-8) # relative tolerance for one step

print(result) # coordinate of function minimum
print(ssquares(result)) # response of function minimum