#### Imports ####

#import libraries
library(tmap)
library(tmaptools)
library(dplyr)
library(sf)    
library(rgdal)
library(raster)
library(rgeos)
library(sf)
library(tmap)

#set working directory and plot mode
tmap_mode('plot')
curr_path <- rstudioapi::getActiveDocumentContext()$path
setwd(dirname(curr_path))

# load data
state_boundary_us <- st_read("../data/cb_2018_us_state_20m/cb_2018_us_state_20m.shp")
temp <- st_read("../data/seastemp_202007")
prec <- st_read('../data/seasprcp_202007')
covid <- read.csv('../data/COVID-19_US_County_JHU_Data_&_Demographics/covid_us_county.csv')
demographic <- read.csv('../data/COVID-19_US_County_JHU_Data_&_Demographics/us_county.csv')


#### Preprocessing ####

#process temperature data to get values for each state
r <- raster(ncol=180, nrow=180)
extent(r) <- extent(temp)
Demo_ras = rasterize(temp, r, 'Prob')
plot(Demo_ras)
agri_s2=extract(x=Demo_ras, y=state_boundary_us, fun=mean, df=TRUE)
state_boundary_us$temp=agri_s2$layer
state_boundary_us <- state_boundary_us[!(state_boundary_us$STUSPS %in% c("AK","HI","PR")),]

#Fill up missing values
state_boundary_us[state_boundary_us$NAME %in% c("Washington","California",
"Texas","Louisiana","Florida","Georgia","Maine","Massachusetts","North Carolina","Virginia","New York"),]$temp=50.0
state_boundary_us[state_boundary_us$NAME %in% c("Michigan"),]$temp=35.0
state_boundary_us[state_boundary_us$NAME %in% c("Ohio"),]$temp=42.5

#extract the covid data for the first day of each month and aggregate data by state
covid.oct <- covid[covid$date=="2020-10-01", ]
covid.oct.sum <- aggregate(cbind(cases,deaths)~state, data = covid.oct, FUN = sum)
oct.total <- sum(covid.oct.sum$cases)
covid.sep <- covid[covid$date=="2020-09-01", ]
covid.sep.sum <- aggregate(cbind(cases,deaths)~state, data = covid.sep, FUN = sum)
sep.total <- sum(covid.sep.sum$cases)
covid.aug <- covid[covid$date=="2020-08-01", ]
covid.aug.sum <- aggregate(cbind(cases,deaths)~state, data = covid.aug, FUN = sum)
aug.total <- sum(covid.aug.sum$cases)
covid.jul <- covid[covid$date=="2020-07-01", ]
covid.jul.sum <- aggregate(cbind(cases,deaths)~state, data = covid.jul, FUN = sum)
jul.total <- sum(covid.jul.sum$cases)
covid.jun <- covid[covid$date=="2020-06-01", ]
covid.jun.sum <- aggregate(cbind(cases,deaths)~state, data = covid.jun, FUN = sum)
jun.total <- sum(covid.jun.sum$cases)
covid.may <- covid[covid$date=="2020-05-01", ]
covid.may.sum <- aggregate(cbind(cases,deaths)~state, data = covid.may, FUN = sum)
may.total <- sum(covid.may.sum$cases)
covid.apr <- covid[covid$date=="2020-04-01", ]
covid.apr.sum <- aggregate(cbind(cases,deaths)~state, data = covid.apr, FUN = sum)
apr.total <- sum(covid.apr.sum$cases)
covid.mar <- covid[covid$date=="2020-03-01", ]
covid.mar.sum <- aggregate(cbind(cases,deaths)~state, data = covid.mar, FUN = sum)
mar.total <- sum(covid.mar.sum$cases)
covid.feb <- covid[covid$date=="2020-02-01", ]
covid.feb.sum <- aggregate(cbind(cases,deaths)~state, data = covid.feb, FUN = sum)
feb.total <- sum(covid.feb.sum$cases)

#calculate the increase in each month
sep.inc <- oct.total - sep.total
aug.inc <- sep.total - aug.total
jul.inc <- aug.total - jul.total
jun.inc <- jul.total - jun.total
may.inc <- jun.total - may.total
apr.inc <- may.total - apr.total
mar.inc <- apr.total - mar.total
feb.inc <- mar.total - feb.total

#plot line graph to check new cases in each month
plot(c(2,3,4,5,6,7,8,9),c(feb.inc,mar.inc,apr.inc,may.inc,jun.inc,jul.inc,aug.inc,sep.inc),xlab="Month",ylab="No of New Cases",type="o")

#from 1 july to 1 aug, it's the highest so create dataframe and calculate the new cases/deaths for jul by taking difference
jul.aug <- merge(covid.jul.sum, covid.aug.sum,by="state")
jul.aug$cases <- jul.aug$cases.y - jul.aug$cases.x
jul.aug$deaths <- jul.aug$deaths.y - jul.aug$deaths.x
jul.aug

#aggregate demographic data for each state
population.sum <- aggregate(cbind(male,female,population)~state, data = demographic, FUN = sum)
age.mean <- aggregate(cbind(median_age)~state, data = demographic, FUN = mean)
demographic.processed <- merge(population.sum, age.mean, by='state')

#merge polygon data with covid data and demographic data
m <- merge(state_boundary_us, jul.aug, by.x='NAME',by.y = "state")
m <- merge(m, demographic.processed, by.x='NAME', by.y = "state")

#calculate detection/death rate/death rate among detected/pop density
m$case.rate <- m$cases / m$population
m$death.rate <- m$deaths / m$population
m$death.rate.among.detected <- m$deaths / m$cases
m$pop.density <-m$population /m$ALAND *1000000

#remove three states without any neighbours and District of Columbia (area too small and population density too high)
m <- m[!(m$STUSPS %in% c("AK","HI","PR","DC")),]
dev.off()

#### EDA ####
#temperature
tm_shape(temp) + tm_polygons('Prob')+tm_layout(title="Temperature in US",title.position = c("center",0.9))
tm_shape(temp) + tm_fill(col="Prob") + tm_shape(state_boundary_us)+tm_borders()+tm_layout(title="Temperature in the US (16 July)",title.position = c(0.4,0.9))+tm_compass()

#temperature for states of interest
tm_shape(state_boundary_us) + tm_polygons('temp')+tm_compass(position=c('right',0.1))+tm_scale_bar(position=c('right',0.0))+tm_layout(title="Temperature on July 16 by State",title.position = c(0.65,0.95),title.size = 0.9)

#precipitation
tm_shape(prec) + tm_polygons('Prob',palette = "Blues")
tm_shape(prec) + tm_fill(col="Prob") + tm_shape(state_boundary_us)+tm_borders()
tm_shape(prec) + tm_fill(col="Prob",palette = "Blues") + tm_shape(state_boundary_us)+tm_borders()+tm_layout(title="Precipitation in the US (16 July)",title.position = c(0.4,0.9))+tm_compass()

#number cases and deaths
tm_shape(m) + tm_polygons('cases')+tm_bubbles('deaths')+tm_compass(position=c('right',0.4))+tm_scale_bar(position=c('left',0.1))+tm_layout(title="New COVID-19 Cases & Deaths in July 2020",
                title.position = c(0.6,0.95),title.size = 0.8,legend.position = c(0.78,0),legend.width = 0.5,legend.height=0.5)

#population by state
tm_shape(m) + tm_polygons('population',palette = "Blues")+tm_compass(position=c(0.1,0.2))+tm_scale_bar(position=c('left',0.1))+tm_layout(title="Population Size by State",title.position = c(0.05,0.05),title.size = 0.8,legend.position = c(0.8,0.2))

#median age
tm_shape(m) + tm_polygons('median_age')

#population density
tm_shape(m) + tm_polygons('pop.density')


#### Correlation Analysis ####
#number of cases and population
cor.test(m$cases, m$population, method = "pearson")

#number of deaths and population
cor.test(m$deaths, m$population, method = "pearson")

#median_age and death rate among detected
cor.test(m$median_age, m$death.rate.among.detected, method = "pearson")

#temperature and three risk indicators
cor.test(m$case.rate, m$temp, method = "pearson")
cor.test(m$death.rate, m$temp, method = "pearson")
cor.test(m$death.rate.among.detected, m$temp, method = "pearson")

#population density and three risk indicators
cor.test(m$case.rate, m$pop.density, method = "pearson")
cor.test(m$death.rate, m$pop.density, method = "pearson")
cor.test(m$death.rate.among.detected, m$pop.density, method = "pearson")


#### Spatial Autocorrelation Analysis ####
require(spdep)
state.nb <- poly2nb(m)
state.nb

# Convert the neighbour list to a listw object - use Queen’s case
state.lw <- nb2listw(state.nb,zero.policy=TRUE)

#1. Case Rate
#global moran test
moran.test(m$case.rate,state.lw,randomisation=FALSE,zero.policy = TRUE)

#plot local moran's I for case rate
m$lI <- localmoran(m$case.rate,state.lw)[, 1]
tm_shape(m,unit='miles') + tm_polygons(col= 'lI',title= "Local Moran’s I (Case Rate)",legend.format=list(flag= "+")) +
  tm_style('col_blind') + tm_scale_bar(width= 0.15,position = c('right',0.2)) + tm_layout(legend.position = c("left", "bottom"), legend.text.size= 0.4, asp=1.8, main.title = "Local Moran's I for Case Rate")+tm_compass()

#plot p value for local moran's I for case rate
m$pval <- localmoran(m$case.rate,state.lw)[, 5]
tm_shape(m,unit= 'miles') + tm_polygons(col= 'pval', title= "p-value (Case Rate)" , breaks= c(0, 0.01, 0.05, 0.10, 1), border.col = "black",palette = "-Reds") +
  tm_scale_bar(width=0.15) +
  tm_layout(legend.position = c("left", "bottom"),asp=1.8)+tm_compass()

#2. Death Rate
#global moran test
moran.test(m$death.rate,state.lw,randomisation=FALSE,zero.policy = TRUE)

#plot local moran's I for death rate
m$lI2 <- localmoran(m$death.rate,state.lw)[, 1]
tm_shape(m,unit='miles') + tm_polygons(col= 'lI2',title= "Local Moran’s I (Death Rate)",legend.format=list(flag= "+")) +
  tm_style('col_blind') + tm_scale_bar(width= 0.15,position = c('right',0.2)) + tm_layout(legend.position = c("left", "bottom"), legend.text.size= 0.4, asp=1.8)+tm_compass()

#plot p value for local moran's I for death rate
m$pval2 <- localmoran(m$death.rate,state.lw)[, 5]
tm_shape(m,unit= 'miles') + tm_polygons(col= 'pval2', title= "p-value (Death Rate Per Case)" , breaks= c(0, 0.01, 0.05, 0.10, 1), border.col = "black",palette = "-Reds") +
  tm_scale_bar(width=0.15) +
  tm_layout(legend.position = c("left", "bottom"),asp=1.8)+tm_compass()

#3. Death Rate among Detected
#global moran test for death rate among detected
moran.test(m$death.rate.among.detected,state.lw,randomisation=FALSE,zero.policy = TRUE)

#plot local moran's I for death rate among detected
m$lI3 <- localmoran(m$death.rate.among.detected,state.lw)[, 1]
tm_shape(m,unit='miles') + tm_polygons(col= 'lI3',title= "Local Moran’s I (Death Rate Per Case)",legend.format=list(flag= "+")) +
  tm_style('col_blind') + tm_scale_bar(width= 0.15,position = c('right',0.2)) + tm_layout(legend.position = c("left", "bottom"), legend.text.size= 0.4, asp=1.8)+tm_compass()

#plot p value for local moran's I for death rate among detected
m$pval3 <- localmoran(m$death.rate.among.detected,state.lw)[, 5]
tm_shape(m,unit= 'miles') + tm_polygons(col= 'pval3', breaks= c(0, 0.01, 0.05, 0.10, 1), border.col = "black",palette = "-Reds") +
  tm_scale_bar(width=0.15) +
  tm_layout(legend.position = c("left", "bottom"),asp=1.8,title= "p-value (Death Rate Per Case)",title.position = c(0.6,0.95),title.size=0.7)

