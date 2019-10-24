#Ploting a map of NA for HCHO data for 2005 May

library(fields); 
library(maps);
??maps

par(mfrow=c(1,5))
image.plot(Lon, Lat, HCHO_Monthly[,,1,1], title = paste("2005 - ",values))
map('world', add=TRUE)

image.plot(Lon, Lat, HCHO_Monthly[,,1,2], title = paste("2005 - ",values))
map('world', add=TRUE)

image.plot(Lon, Lat, HCHO_Monthly[,,1,3], title = paste("2005 - ",values))
map('world', add=TRUE)

image.plot(Lon, Lat, HCHO_Monthly[,,1,4], title = paste("2005 - ",values))
map('world', add=TRUE)

image.plot(Lon, Lat, HCHO_Monthly[,,1,5], title = paste("2005 - ",values))
map('world', add=TRUE)

#using looping methods
par(mfrow=c(5,5))
d = 1:5
c = 1:5
for (year in d)
{
  for (month in c)
  {
  image.plot(Lon, Lat, HCHO_Monthly[,,year,month])
  map('world', add=TRUE)
  }
}

par(mfrow=c(5,5))
d = 6:10
c = 1:5
for (year in d)
{
  for (month in c)
  {
    image.plot(Lon, Lat, HCHO_Monthly[,,year,month])
    map('world', add=TRUE)
  }
}
