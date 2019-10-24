#Ploting a map of difference in SE US for HCHO data for 2011 and 2010 May

library(fields); 
library(maps)

HCHOSE11May=HCHO_Monthly[85:127,39:57,7,1]
DHCHOSE11_10May=HCHOSE11May - HCHOSE10May
image.plot(Lon[85:127], Lat[39:57], DHCHOSE11_10May,xlab='Longitude',ylab='Latitude')
map('world', add=TRUE);
title('Change in HCHO in SE US from 2010 to 2011 in May')	

?image.plot



?map
