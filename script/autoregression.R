#set working directory
setwd("~/Desktop/Spatial-Annalysis-of-Covid-19-for-US/data")

# read US state shape file
library(sf)
state_boundary_us <- st_read("cb_2018_us_state_20m/cb_2018_us_state_20m.shp")
head(us.state,5)
# view US state
library(tmap) 
tmap_mode('view')
tm_shape(us.state) + tm_polygons()

#load temperature data
temp <- st_read("seastemp_202007")
tm_shape(temp) + tm_polygons('Prob')+tm_layout(title="Temperature in US",title.position = c("center",0.9))

## process mobility data
#rasterize and get values for each state
r <- raster(ncol=180, nrow=180)
extent(r) <- extent(temp)
Demo_ras = rasterize(temp, r, 'Prob')
plot(Demo_ras)
agri_s2=extract(x=Demo_ras, y=state_boundary_us, fun=mean, df=TRUE)
plot(agri_s2)
class(agri_s2)

state_boundary_us$temp=agri_s2$layer
state_boundary_us <- state_boundary_us[!(state_boundary_us$STUSPS %in% c("AK","HI","PR")),]

#Fill up missing values
state_boundary_us[state_boundary_us$NAME %in% c("Washington","California",
                                                "Texas","Louisiana","Florida","Georgia","Maine","Massachusetts","North Carolina","Virginia","New York"),]$temp=50.0
state_boundary_us[state_boundary_us$NAME %in% c("Michigan"),]$temp=35.0
state_boundary_us[state_boundary_us$NAME %in% c("Ohio"),]$temp=42.5
tm_shape(state_boundary_us) + tm_polygons('temp')


# read mobility file
mobility <- read.csv("mobility_data/Trips_by_Distance_-_2020.csv")
head(mobility,5)
max(mobility$Date)

## process mobility data
# create a month column
mobility$Date <- as.Date(mobility$Date)
mobility$Month <- format(mobility$Date,"%m")
head(mobility,5)

# aggregate daily mobility to monthly average mobility per county & state
# when county name is empty, it represent the state summary
# mobility.avg[mobility.avg$County.Name == "",]
mobility.avg <- 
  aggregate(cbind(Population.Staying.at.Home, Population.Not.Staying.at.Home,
                  Number.of.Trips, Number.of.Trips..1,
                  Number.of.Trips.1.3, Number.of.Trips.3.5, Number.of.Trips.5.10,
                  Number.of.Trips.10.25, Number.of.Trips.25.50, Number.of.Trips.50.100,
                  Number.of.Trips.100.250, Number.of.Trips.250.500, Number.of.Trips...500)
            ~State.FIPS+State.Postal.Code+County.Name+Month, data = mobility, FUN = mean)
head(mobility.avg,5)

# calculate polulation
mobility.avg["Population"] <- 
  mobility.avg$Population.Staying.at.Home + mobility.avg$Population.Not.Staying.at.Home

# calculate percentage of population staying at home
mobility.avg["Percentage.Staying.at.Home"] <- 
  mobility.avg$Population.Staying.at.Home / mobility.avg$Population

# calculate percentage of population not staying at home
mobility.avg["Percentage.Not.Staying.at.Home"] <- 
  mobility.avg$Population.Not.Staying.at.Home / mobility.avg$Population

# calculate no. of trips per population
mobility.avg["Trips.per.Population"] <- 
  mobility.avg$Number.of.Trips / mobility.avg$Population

# calculate no. of trips per population not staying at home
mobility.avg["Trips.per.Population.Not.Staying.at.Home"] <- 
  mobility.avg$Number.of.Trips / mobility.avg$Population.Not.Staying.at.Home

head(mobility.avg,5)

# filter to state and county
mobility.state.avg <- mobility.avg[mobility.avg$County.Name == "",]
mobility.county.avg <- mobility.avg[mobility.avg$County.Name != "",]

### JULY ONLY
mobility.state.avg.july <- mobility.state.avg[mobility.state.avg$Month == "07",]
mobility.county.avg.july <- mobility.county.avg[mobility.county.avg$Month == "07",]

# merge mobility attributes with polygon data
mobility.july <- merge(us.state, mobility.state.avg.july, 
                       by.x = "STUSPS", by.y = "State.Postal.Code")
head(mobility.july,5)


demographic <- read.csv('COVID-19_US_County_JHU_Data_&_Demographics/us_county.csv')
demographic
covid <- read.csv('COVID-19_US_County_JHU_Data_&_Demographics/covid_us_county.csv')

#get covid data for July
covid.aug <- covid[covid$date=="2020-08-01", ]
covid.aug.sum <- aggregate(cbind(cases,deaths)~state, data = covid.aug, FUN = sum)
aug.total <- sum(covid.aug.sum$cases)
covid.jul <- covid[covid$date=="2020-07-01", ]
covid.jul.sum <- aggregate(cbind(cases,deaths)~state, data = covid.jul, FUN = sum)
jul.total <- sum(covid.jul.sum$cases)
jul.aug <- merge(covid.jul.sum, covid.aug.sum,by="state")
jul.aug$cases <- jul.aug$cases.y - jul.aug$cases.x
jul.aug$deaths <- jul.aug$deaths.y - jul.aug$deaths.x
jul.aug

population.sum <- aggregate(cbind(male,female,population)~state, data = demographic, FUN = sum)
age.mean <- aggregate(cbind(median_age)~state, data = demographic, FUN = mean)
demographic.processed <- merge(population.sum, age.mean, by='state')


#merge datasets
m <- merge(state_boundary_us, jul.aug, by.x='NAME',by.y="state")
m <- merge(m, demographic.processed, by.x='NAME', by.y = "state")
#m <- merge(m, mobility.july, by="NAME")
head(m,5)

m <- m[!(m$STUSPS %in% c("AK","HI","PR","DC")),]
m$case.rate <- m$cases / m$population
m$death.rate <- m$deaths / m$population
m$death.rate.among.detected <- m$deaths / m$cases
m$pop.density <-m$population /m$ALAND *1000000
# merge
m <- merge(m, mobility.state.avg.july, 
                       by.x = "STUSPS", by.y = "State.Postal.Code")

### Social distancing index
sdi <- read.csv("mobility_data/Social distancing index_ST.csv")
head(sdi,5)

# avg of july
sdi.july <- sdi[,c(2,3,186:216)]
sdi.july$avg.sdi <- rowMeans(sdi.july[3:ncol(sdi.july)])
head(sdi.july,5)

# merge sdi with state shape
m <- merge(m, sdi.july, by = "NAME")

race_df <- read.csv('us_race/us_race.csv')
names(race_df)[names(race_df)=='?..Location'] <- 'Location'

##3.2 replace NA values with 0
race_df[is.na(race_df)] <- 0

##3.3 exclude hispanic ethnicity
race_df$Total <- race_df$Total - race_df$Hispanic
race_df <- subset(race_df, select=-c(Hispanic))
race_df <- race_df[c(2:53),]
class(race_df)
m <- merge(m, race_df, by.x = "NAME", by.y="Location")
m
m$White.rate <-m$White/m$population
m$Black.rate <-m$Black/m$population
m$Asian.rate <-m$Asian/m$population
m$American.Indian.Alaska.Native.rate<-m$American.Indian.Alaska.Native/m$population
m$Native.Hawaiian.Other.Pacific.Islander.rate<-m$Native.Hawaiian.Other.Pacific.Islander/m$population
m$Multiple.Races.rate<-m$Multiple.Races/m$population

require(spdep)
state.nb <- poly2nb(m)
state.nb
# Convert the neighbour list to a listw object - use Queen’s case...
state.lw <- nb2listw(state.nb,zero.policy=TRUE)

library(spatialreg)
#spatial autoregression for case rate
sar.res <- spautolm (case.rate~ pop.density + Percentage.Not.Staying.at.Home+Trips.per.Population+avg.sdi+White.rate
                     +Black.rate+Asian.rate+American.Indian.Alaska.Native.rate+Native.Hawaiian.Other.Pacific.Islander.rate+temp, listw=state.lw, data=m)
summary(sar.res)
#spatial autoregression for death rate
sar.res <- spautolm (death.rate~ pop.density + Percentage.Not.Staying.at.Home+Trips.per.Population+avg.sdi+White.rate
                     +Black.rate+Asian.rate+American.Indian.Alaska.Native.rate+Native.Hawaiian.Other.Pacific.Islander.rate+temp, listw=state.lw, data=m)
summary(sar.res)
#spatial autoregression for death rate per case
sar.res <- spautolm (death.rate.among.detected~ pop.density + Percentage.Not.Staying.at.Home+Trips.per.Population+avg.sdi+White.rate
                     +Black.rate+Asian.rate+American.Indian.Alaska.Native.rate+Native.Hawaiian.Other.Pacific.Islander.rate+temp, listw=state.lw, data=m)
summary(sar.res)