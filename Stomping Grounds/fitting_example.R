set.seed(1)

x <- 1:15
y <- -.5*(x-7.5)^2 + 50
plot(x,y)
#true_model <- lm(y~poly(x,2))

#plot(x,y)
#lines(x,y)

noise <- rnorm(length(x), 0, 3)
noise_y <- y+noise
plot(x,noise_y)

under_model <- lm(noise_y ~ x)
plot(x,noise_y)
under_pred <- predict(under_model, data.frame(x=x))
lines(x,under_pred,col = 'red', lwd = 2) #Underfit model

over_model <- lm(noise_y ~ poly(x,14))
predicted <- predict(over_model, data.frame(x=x))
plot(x,noise_y)
lines(x,predicted, col = 'red', lwd = 2) #overfit model
lines(x,y, col = 'green', lwd = 2) # goodfit (+ overfit lingering, can remove)

new_noise <- rnorm(length(x), 0, 2)
new_noise_y <- y + new_noise

plot(x,new_noise_y)
lines(x,predicted, col = 'red', lwd = 2) #(overfit on new data)
lines(x,y, col = 'green', lwd = 2) # adds stayGood fit
