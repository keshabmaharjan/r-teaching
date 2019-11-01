##Polynomial regression
library(reshape2)
dev.off()
for (i in 1:10 )  
{
  loop = i
  print(loop)
  a = 2004
  y1 <- melt(HCHO_Monthly[85:127,39:57,loop,])
  x1 <- melt(TS_Monthly[85:127,39:57,loop,])
  
  data.all <- data.frame(x1,y1)
  head(data.all)
  colnames(data.all)
  
  #assigning the column name
  colnames(data.all) = c("sn1","sn2","sn3", "temp","sn4", "sn5","sn6", "hcho")
  
  # Build the model
  #model <- lm(data.all$hcho ~ poly(data.all$temp, 2, raw = TRUE), data = Data)
  # Make predictions
  #summary(model)
  #Data %>%
  #  add_predictions(model) %>%
  #  summarise(
  #    R2 = cor(Data$temp, predictions)^2,
  #    MSE = mean((Data$temp - predictions)^2),
  #    RMSE = sqrt(MSE),
  #    MAE = mean(abs(Data$temp - predictions))
  #  )
  #for output in the png format
  
  ggplot(data.all, aes(x= data.all$temp, y = data.all$hcho) ) +
    geom_point(cex = .1) +
    stat_smooth(method = lm, formula = y ~ poly(x, 2, raw = TRUE)) +
    labs(title = "Scatter Plot of Us-SE",subtitle = paste("HCHO con. v/s Temp [K] - ", a + loop),x = "Temperature",y = "HCHO(molec/cm^2)") 
}  
  ggplot(data.all, aes(x= data.all$temp, y = data.all$hcho) , color = data.all$temp > 250) +
    geom_point(cex = .1) +
    stat_smooth(method = lm, formula = y ~ poly(x, 2, raw = TRUE)) +
    stat_ellipse()+
    labs(title = "Scatter Plot of Us-SE",subtitle = paste("HCHO con. v/s Temp [K] - ", a + loop),x = "Temperature",y = "HCHO(molec/cm^2)") 
  
  
  ggplot(data.all, aes(x= data.all$temp, y = data.all$hcho) , color = data.all$hcho > 10.0e+16) +
    geom_point(cex = .1) +
    stat_smooth(method = lm, formula = y ~ poly(x, 2, raw = TRUE)) +
    stat_ellipse(type = "norm", linetype = 2) +
    labs(title = "Scatter Plot of Us-SE",subtitle = paste("HCHO con. v/s Temp [K] - ", a + loop),x = "Temperature",y = "HCHO(molec/cm^2)") 
  
  ggplot(data.all, aes(x= data.all$temp, y = data.all$hcho) , color = data.all$hcho > 10.0e+16) +
    geom_point(cex = .1) +
    stat_smooth(method = lm, formula = y ~ poly(x, 2, raw = TRUE)) +
    stat_ellipse(type = "norm", linetype = 2) +
    labs(title = "Scatter Plot of Us-SE",subtitle = paste("HCHO con. v/s Temp [K] - ", a + loop),x = "Temperature",y = "HCHO(molec/cm^2)") 
  
  ggplot(data.all, aes(x= data.all$temp, y = data.all$hcho, z=data.all$hcho ) , color = data.all$hcho > 10.0e+16) +
  stat_summary_2d()
    
  ggplot(data.all, aes(x= data.all$temp, y = data.all$hcho, z=data.all$hcho ) , 
         color = data.all$hcho > 10.0e+16) + 
    stat_summary_2d(fun = function(x) sum(x^2))+
    labs(title = "Scatter Plot of Us-SE",subtitle = paste("HCHO con. v/s Temp [K] - ", a + loop),x = "Temperature",y = "HCHO(molec/cm^2)") 
  
  ggplot(data.all, aes(x= data.all$temp, y = data.all$hcho, z=data.all$hcho ) , 
  color = data.all$hcho > 10.0e+16) +
  stat_summary_2d(fun = var)
  
  ggplot(data.all, aes(x= data.all$temp, y = data.all$hcho, z=data.all$hcho ) , 
  color = data.all$hcho > 10.0e+16)+
  stat_summary_2d(fun = "quantile", fun.args = list(probs = 0.1))
  
  ggplot(data.all, aes(x= data.all$temp, y = data.all$hcho, z=data.all$hcho ) , 
  color = data.all$hcho > 10.0e+16)+
  stat_summary_hex()
  
  ggplot(data.all, aes(x= data.all$temp, y = data.all$hcho, z=data.all$hcho ) ,color = data.all$hcho > 10.0e+16)+
    geom_point(alpha = 1/5)+
    geom_line()
  
  ggplot(data.all, aes(x= data.all$temp, y = data.all$hcho, z=data.all$hcho ) ,color = data.all$hcho > 10.0e+16)+
  geom_smooth(size = 2, method = "lm", se = FALSE)
  
install.packages("ggmap")
library(ggmap)


#install.packages("ggpubr")
#library(ggpubr)
#figure <- ggarrange(p1,p2, labels = c("2005", "2006"), ncol = 1, nrow = 1)
#figure
