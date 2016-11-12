#####
# Next task: consider a model where noise is a normal distribution where the variance increases 
#             linearly with x
x <- c(0:1000)
error <- rnorm(x,0,0.001*x) #rnorm generates len(x) random deviates based on x value in question
y <- 0.25*x+error
plot(x,y)
model_norm <- lm(y~x)
res_norm <- resid(model_norm)
plot(res_norm ~ predict(model_norm),
     main = "lm() Residual Plot where noise is a normal distribution where the variance increases linearly with x",
     xlab = "Model Prediction",
     ylab = "Residuals")
# More compact: plot(resid(model_norm) ~ predict(model_norm))
#####