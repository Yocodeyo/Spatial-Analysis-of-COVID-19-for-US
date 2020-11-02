library(ggplot2)      # For plotting
library(tidycensus)   # For downloading Census data
library(tmap)         # For creating tmap
library(tmaptools)    # For reading and processing spatial data related to tmap
library(dplyr)        # For data wrangling
library(sf)    
library(rgdal)
library(raster)
library(rgeos)
options(stringsAsFactors = FALSE)
library(sf)
library(tmap)

setwd("~/Downloads/BT4015Project")
# Import the shapefile data into R
state_boundary_us <- st_read("cb_2018_us_state_20m/cb_2018_us_state_20m.shp")
spTransform(state_boundary_us, CRS("+init=epsg:3724"))
#readOGR("cb_2018_us_state_20m")
class(state_boundary_us)
summary(state_boundary_us)
state_boundary_us$ALAND

#library(raster)
#un1 <- union(temp,state_boundary_us)
#class(un1)



tmap_mode('view')
temp <- st_read("seastemp_202007")
class(temp)
summary(temp)
tm_shape(temp) + tm_polygons('Prob')
tm_shape(temp) + tm_fill(col="Prob") + tm_shape(state_boundary_us)+tm_borders()
prec <- st_read('seasprcp_202007')
summary(prec)
tm_shape(prec) + tm_polygons('Prob')
tm_shape(prec) + tm_fill(col="Prob") + tm_shape(state_boundary_us)+tm_borders()




tm_shape(state_boundary_us) + tm_polygons('NAME')

covid <- read.csv('covid_us_county.csv')
demographic <- read.csv('us_county.csv')

covid.sep <- covid[covid$date=="1/9/20", ]
covid.sep.sum <- aggregate(cbind(cases,deaths)~NAME, data = covid.sep, FUN = sum)
sep.total <- sum(covid.sep.sum$cases)
covid.aug <- covid[covid$date=="1/8/20", ]
covid.aug.sum <- aggregate(cbind(cases,deaths)~NAME, data = covid.aug, FUN = sum)
aug.total <- sum(covid.aug.sum$cases)
covid.jul <- covid[covid$date=="1/7/20", ]
covid.jul.sum <- aggregate(cbind(cases,deaths)~NAME, data = covid.jul, FUN = sum)
jul.total <- sum(covid.jul.sum$cases)
covid.jun <- covid[covid$date=="1/6/20", ]
covid.jun.sum <- aggregate(cbind(cases,deaths)~NAME, data = covid.jun, FUN = sum)
jun.total <- sum(covid.jun.sum$cases)
covid.may <- covid[covid$date=="1/5/20", ]
covid.may.sum <- aggregate(cbind(cases,deaths)~NAME, data = covid.may, FUN = sum)
may.total <- sum(covid.may.sum$cases)
covid.apr <- covid[covid$date=="1/4/20", ]
covid.apr.sum <- aggregate(cbind(cases,deaths)~NAME, data = covid.apr, FUN = sum)
apr.total <- sum(covid.apr.sum$cases)
covid.mar <- covid[covid$date=="1/3/20", ]
covid.mar.sum <- aggregate(cbind(cases,deaths)~NAME, data = covid.mar, FUN = sum)
mar.total <- sum(covid.mar.sum$cases)
covid.feb <- covid[covid$date=="1/2/20", ]
covid.feb.sum <- aggregate(cbind(cases,deaths)~NAME, data = covid.feb, FUN = sum)
feb.total <- sum(covid.feb.sum$cases)


sep.inc <- oct.total - sep.total
aug.inc <- sep.total - aug.total
jul.inc <- aug.total - jul.total
jun.inc <- jul.total - jun.total
may.inc <- jun.total - may.total
apr.inc <- may.total - apr.total
mar.inc <- apr.total - mar.total
feb.inc <- mar.total - feb.total
#'Feb','Mar','Apr','May','Jun','Jul','Aug','Sep'
plot(c(2,3,4,5,6,7,8,9),c(feb.inc,mar.inc,apr.inc,may.inc,jun.inc,jul.inc,aug.inc,sep.inc),xlab="Month",ylab="No of New Cases")
#head(covid.oct,5)

jul.aug <- merge(covid.jul.sum, covid.aug.sum,by="NAME")
jul.aug$cases <- jul.aug$cases.y - jul.aug$cases.x
jul.aug$deaths <- jul.aug$deaths.y - jul.aug$deaths.x
jul.aug

#write.csv(covid.oct.sum,'covid_us_oct.csv')
jul.aug$rate <- jul.aug$deaths/jul.aug$cases
#covid.oct.sum

demographic <- read.csv('us_county.csv')
#head(demographic,5)
population.sum <- aggregate(cbind(male,female,population)~state, data = demographic, FUN = sum)
age.mean <- aggregate(cbind(median_age)~state, data = demographic, FUN = mean)
demographic.processed <- merge(population.sum, age.mean, by='state')
#head(demographic.processed,5)

m <- merge(state_boundary_us, jul.aug, by='NAME')
m <- merge(m, demographic.processed, by.x='NAME', by.y = "state")
#m
#calculate detection/death rate/death rate among detected/pop density
m$case.rate <- m$cases / m$population
m$death.rate <- m$deaths / m$population
m$death.rate.among.detected <- m$deaths / m$cases
m$pop.density <-m$population /m$ALAND *1000000

summary(m$pop.density)
head(m$pop.density,500)
m$pop.density[9]<-0 #Make that for District of Columbia zero



tmap_mode('view')
#deaths+cases
tm_shape(m) + tm_polygons('cases')+tm_bubbles('deaths')
res <- cor.test(m$cases, m$deaths, 
                method = "pearson")
res
#death rate
tm_shape(m) + tm_polygons('rate')
#population
tm_shape(m) + tm_polygons('population')+tm_bubbles('cases')
res <- cor.test(m$cases, m$population, 
                method = "pearson")
res #0.8216305
res <- cor.test(m$case.rate, m$population, 
                method = "pearson")
res #0.2731741
#use case rate instead of case, observe case rate
tm_shape(m) + tm_polygons('case.rate')
#median_age
tm_shape(m) + tm_polygons('median_age')+tm_bubbles('death.rate.among.detected')
res <- cor.test(m$median_age, m$death.rate.among.detected, 
                method = "pearson")
res
tm_shape(m) + tm_polygons('median_age')+tm_bubbles('case.rate')
res <- cor.test(m$median_age, m$case.rate, 
                method = "pearson")
res #-0.29
#death rate
tm_shape(m) + tm_polygons('death.rate')
#death rate
tm_shape(m) + tm_polygons('case.rate')
tm_shape(m) + tm_polygons('population')+tm_bubbles('case.rate')
#population density
tm_shape(m) + tm_polygons('pop.density')
tm_shape(m) + tm_polygons('pop.density')+tm_bubbles('cases')
#cc:-0.004840747 
tm_shape(m) + tm_polygons('pop.density')+tm_bubbles('case.rate')
res <- cor.test(m$case.rate, m$pop.density, 
                method = "pearson")
res#-0.2407

#
require(spdep)
state.nb <- poly2nb(m)
state.nb
# Convert the neighbour list to a listw object - use Queen’s case...
state.lw <- nb2listw(state.nb,zero.policy=TRUE)

m$case.rate.lagged.means <- lag.listw(state.lw,m$case.rate)
tm_shape(m) + tm_polygons(col= 'case.rate.lagged.means',title= 'Case Rate') + tm_layout(legend.bg.color = "white")
moran.test(m$case.rate,state.lw,randomisation=FALSE,zero.policy = TRUE)

m$lI <- localmoran(m$case.rate,state.lw)[, 1]
tm_shape(m,unit='miles') + tm_polygons(col= 'lI',title= "Local Moran’s I",legend.format=list(flag= "+")) +
  tm_style('col_blind') + tm_scale_bar(width= 0.15) + tm_layout(legend.position = c("left", "bottom"), legend.text.size= 0.4, asp=1.8)

m$pval <- localmoran(m$case.rate,state.lw)[, 5]
# Draw the map
tm_shape(m,unit= 'miles') + tm_polygons(col= 'pval', title= "p-value" , breaks= c(0, 0.01, 0.05, 0.10, 1), border.col = "black",palette = "-Greens") +
  tm_scale_bar(width=0.15) +
  tm_layout(legend.position = c("left", "bottom"),asp=1.8)


#Use of SAR model without predictor variable
#install.packages("spatialreg")
library(spatialreg)
sar.res <- spautolm (case.rate~ 1, listw=state.lw, data=m) 
summary(sar.res)
sar.res <- spautolm (case.rate~ pop.density, listw=state.lw, data=m)
summary(sar.res)
#with predictor
head(pennLC$data)
require(plyr)
totcases <- ddply(pennLC$data,c("county"),numcolwise(sum))
head(totcases)
totcases <- transform(totcases,rate=10000*cases/population)
head(totcases)
# Check the distribution of rates
boxplot(totcases$rate,horizontal=TRUE,
        xlab='Cancer Rate (Cases per 10,000 Popn.)')

sar.mod <- spautolm(rate~sqrt(m$cases),listw=state.lw,
                    weight=population,data=m)
summary(sar.mod)
