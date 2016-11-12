# Input the data on Sales vs. spending on TV, Newpaper and Radio advertising
dat.adv <- read.csv("advertising.csv")
Sales <- dat.adv$Sales
TV <- dat.adv$TV
Radio <- dat.adv$Radio
Newspaper <- dat.adv$Newspaper

# Linear least squares regression for each of the possible explanatory variables
mod <- lm(Sales ~ ., dat.adv)

# Residuals for linear least squares fitting
res <- residuals(mod)

plot(Sales~TV) # tilda usage: Sales "depends on" TV. Equivalent to plot(TV,S)
plot(Sales~Radio)
plot(Sales~Newspaper)
plot(res ~ predict(mod),
     main = "Residual Plot for Sales vs. TV, Radio and Newspaper Advertising",
     xlab = "Model Prediction",
     ylab = "Residuals")

# Note: run a single line using alt+enter

# Matric multiplication check of linear fitting, matrix mult X %*% Y

X <- as.matrix(dat.adv[,2:4]) # design matrix
Y <- as.matrix(dat.adv[,5]) # response variable
#X <- cbind(1,dat.adv$TV,dat.adv$Radio,dat.adv$Newspaper)
X <- cbind(1, X) #rep(1,length(Y))
beta <- solve(t(X) %*% X)  %*% t(X) %*% Y
beta
Y_hat <- X %*% beta
res_manual <- (Y - (X %*% beta))
plot(Y_hat,res_manual,
    main = "Manual Residual Plot for Sales vs. TV, Radio and Newspaper Advertising",
    xlab = "Model Prediction",
    ylab = "Residuals")

