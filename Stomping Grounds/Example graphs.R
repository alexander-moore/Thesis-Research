stem_length <- seq(from = 0, to = 5, by = .1)
flower_diam <- 1.6^x  + rnorm(51, 0, .4)

df_orig <- cbind(stem_length,flower_diam)
df_dropped <- df_orig[-c(40:51), ]

plot(df_orig)
plot(df_dropped)

x <- df_dropped[,1]
y <- df_dropped[,2]

lin_relation <- lm(y~x)

plot(x,y,col = "blue", 
     main = "Dropped Linear Regression",
     abline(lin_relation, col = "red"), 
     cex = .7,pch = 16, 
     xlab = "Stem Length", 
     ylab = "Flower Diameter")

exp_relation <- lm(log(df_orig[,2]) ~ df_orig[,1])

plot(df_orig[,1],df_orig[,2],col = "blue", 
     main = "True Exponential Data",
     abline(exp_relation, col = "red"), 
     cex = .7,pch = 16, 
     xlab = "Stem Length", 
     ylab = "Flower Diameter")




x <- seq(from = 0, to = 20, by = .05)
y <- 300 + .5*(x-8)^3 
plot(x,y)

noise <- rnorm(length(x), mean = 8, sd = 100)
noise.y <- y+noise
plot(x,noise.y)

plot(x, noise.y, col = 'deepskyblue4', xlab = 'x', ylab = 'noise.y', main = "Observed Data")
lines(x, y, col='firebrick1', lwd = 3)

model <- lm(noise.y ~ poly(x, 25))

predicted.intervals <- predict(model,data.frame(x=x),interval='confidence',
                               level=0.99)

lines(x,predicted.intervals[,1],col='green',lwd=3)
lines(x,predicted.intervals[,2],col='black',lwd=1.5)
lines(x,predicted.intervals[,3],col='black',lwd=1.5)

legend("bottomright",c("Observ.","Signal","Predicted"), 
       col=c("deepskyblue4","red","green"), lwd=3)


x <- 1:15
y <- -.5*(x-7.5)^2 + 50
plot(x,y)
true_model <- lm(y~poly(x,2))

plot(x,y)
lines(x,y)

noise <- rnorm(length(x), 0, 3)
noise_y <- y+noise
plot(x,noise_y)

under_model <- lm(noise_y ~ x)
plot(x,noise_y)
under_pred <- predict(under_model, data.frame(x=x))
lines(x,under_pred,col = 'red', lwd = 2)

over_model <- lm(noise_y ~ poly(x,14))
predicted <- predict(over_model, data.frame(x=x))
plot(x,noise_y)
lines(x,predicted, col = 'red', lwd = 2)
lines(x,y, col = 'green', lwd = 2)

new_noise <- rnorm(length(x), 0, 2)
new_noise_y <- y + new_noise

plot(x,new_noise_y)
lines(x,predicted, col = 'red', lwd = 2)
lines(x,y, col = 'green', lwd = 2)
