y <- melt(HCHO_Monthly[85:127,39:57,loop,1])
x <- melt(TS_Monthly[85:127,39:57,loop,1])

# plot of x and y :
plot(x$value,y$value,col=rgb(0.3,0.3,0.5,0.4),pch=16 , cex=1.3) 

# Can we find a polynome that fit this function ?
model <- lm(y$value ~ x$value + I(x$value^2))
#model <- lm(y$value ~ x$value + I(x$value^2) + I(x$value^3))

# I can get the features of this model :
#summary(model)
#model$coefficients
#summary(model)$adj.r.squared

# For each value of x, I can get the value of y estimated by the model, and add it to the current plot !
myPredict <- predict( model ) 
ix <- sort(x$value,index.return=T)$ix
lines(x$value[ix], myPredict[ix], col=2, lwd=2 )  

# I add the features of the model to the plot
coeff <- round(model$coefficients , 2)
text(3, -70 , paste("Model : ",coeff[1] , " + " , coeff[2] , "*x"  , "+" , coeff[3] , "*x^2" , "+" , coeff[4] , "*x^3" , "\n\n" , "P-value adjusted = ",round(summary(model)$adj.r.squared,2)))
