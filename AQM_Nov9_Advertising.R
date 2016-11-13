#library(ggplot2)

# Input the data on Sales vs. spending on TV, Newpaper and Radio advertising
dat.adv <- read.csv("advertising.csv")
Sales <- dat.adv$Sales
TV <- dat.adv$TV
Radio <- dat.adv$Radio
Newspaper <- dat.adv$Newspaper

# Linear least squares regression for each of the possible explanatory variables
mod <- lm(Sales ~ TV+Radio+Newspaper, dat.adv)

# Residuals for linear least squares fitting
res <- residuals(mod)

plot(Sales~TV) # tilda usage: Sales "depends on" TV. Equivalent to plot(TV,S)
plot(Sales~Radio)
plot(Sales~Newspaper)
plot(res ~ predict(mod),
     main = "lm () Residual Plot for Sales vs. TV, Radio and Newspaper Advertising",
     xlab = "Model Prediction",
     ylab = "Residuals")

# Note: run a single line using alt+enter

# Matric multiplication check of linear fitting, matrix mult X %*% Y

X <- as.matrix(dat.adv[,2:4]) # design matrix
Y <- as.matrix(dat.adv[,5]) # response variable
#X <- cbind(1,dat.adv$TV,dat.adv$Radio,dat.adv$Newspaper)
X <- cbind(1, X) #rep(1,length(Y))
beta <- solve(t(X) %*% X)  %*% t(X) %*% Y
Y_hat <- X %*% beta
res_manual <- (Y - (X %*% beta))

plot(Y_hat,res_manual,
    main = "Manual Residual Plot for Sales vs. TV, Radio and Newspaper Advertising",
    xlab = "Model Prediction",
    ylab = "Residuals")

mod
beta

qplot(TV,res_manual,
           main = "Residuals vs. TV Advertising",
           xlab = "TV",
           ylab = "Residuals")
qplot(Radio,res_manual,
           main = "Residuals vs. Radio Advertising",
           xlab = "Radio",
           ylab = "Residuals")
qplot(Newspaper,res_manual,
           main = "Residuals vs. Newspaper Advertising",
           xlab = "Newspaper",
           ylab = "Residuals")
# It looks like the linear model fully explains the impact of Newspaper advertising (randomly distributed residul)
# but the residuals for TV and Radio show some structure with wider variance at smaller and larger advertising values.
# Looks like an offset quadratic term needs to be accounted for.
