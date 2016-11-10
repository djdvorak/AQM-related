dat.adv <- read.csv("advertising.csv")

mod <- lm(Sales ~ ., dat.adv)

res <- residuals(mod)
plot(log(dat.adv$Sales) ~ dat.adv$TV)

plot(res ~ predict(mod),
     main = "Residual Plot",
     xlab = "observation",
     ylab = "Residuals")



#run a single line using alt+enter

#Next task: model where noise is a normal distribution where the variance increases linearly with x
x <- c(0:1000)
error <- rnorm(x,0,0.001*x) #rnorm generates len(x) random deviates based on x value in question
y <- 0.25*x+error
plot(x,y)
model_norm <- lm(y~x)
res_norm <- resid(model_norm)
plot(res_norm ~ predict(model_norm))
#plot(resid(model_norm) ~ predict(model_norm))

# Matric multiplication check of linear fitting, matrix mult X %*% Y

X <- as.matrix(dat.adv[,2:4]) # design matrix
Y <- as.matrix(dat.adv[,5]) # response variable
#X <- cbind(1,dat.adv$TV,dat.adv$Radio,dat.adv$Newspaper)
X <- cbind(1, X) #rep(1,length(Y))
beta <- solve(t(X) %*% X)  %*% t(X) %*% Y
res_manual <- t(Y - (X %*% beta)) %*% (Y - (X %*% beta))
plot(Y,res_manual)
