#Ploting a map of SE US for HCHO data for 2005 May

library(fields); library(maps)
image.plot(Lon[85:127], Lat[39:57], HCHO_Monthly[85:127,39:57,1,1])
map('world', add=TRUE)	