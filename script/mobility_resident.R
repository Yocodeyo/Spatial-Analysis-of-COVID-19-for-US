#### Imports ####

# set working directory
curr_path <- rstudioapi::getActiveDocumentContext()$path
setwd(dirname(curr_path))
#Sys.setenv(LANG = "en")
#Sys.setlocale("LC_TIME", "C")

# load data
library(sf)
us.state <- st_read("../data/cb_2018_us_state_20m/cb_2018_us_state_20m.shp")
mobility <- read.csv("../data/mobility_data/Trips_by_Distance_-_2020.csv")
sdi <- read.csv("../data/mobility_data/Social distancing index_ST.csv")
covid <- read.csv("../data/COVID-19_US_County_JHU_Data_&_Demographics/covid_us_county.csv")
demographic <- read.csv("../data/COVID-19_US_County_JHU_Data_&_Demographics/us_county.csv")

#### Preprocessing ####

## mobility and states ##

# create a month column
mobility$Date <- as.Date(mobility$Date)
mobility$Month <- format(mobility$Date,"%m")

# aggregate daily data to monthly average
mobility.avg <- 
  aggregate(cbind(Population.Staying.at.Home, Population.Not.Staying.at.Home,
                  Number.of.Trips, Number.of.Trips..1,
                  Number.of.Trips.1.3, Number.of.Trips.3.5, Number.of.Trips.5.10,
                  Number.of.Trips.10.25, Number.of.Trips.25.50, Number.of.Trips.50.100,
                  Number.of.Trips.100.250, Number.of.Trips.250.500, Number.of.Trips...500)
            ~State.FIPS+State.Postal.Code+County.Name+Month, data = mobility, FUN = mean)

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

# filter to 51 states
mobility.state.avg <- mobility.avg[mobility.avg$County.Name == "",]

# filter to July
mobility.state.avg.july <- mobility.state.avg[mobility.state.avg$Month == "07",]

# merge mobility attributes with polygon data
mobility.july <- merge(us.state, mobility.state.avg.july, 
                           by.x = "STUSPS", by.y = "State.Postal.Code")

# filter to 48 states
mobility.july <- mobility.july[!(mobility.july$STUSPS %in% c("AK","HI","PR")),]

## social distancing index and states ##

# avg of july
sdi.july <- sdi[,c(2,3,186:216)]
sdi.july$avg.sdi <- rowMeans(sdi.july[3:ncol(sdi.july)])
head(sdi.july,5)

# merge sdi with state shape
us.sdi.july <- merge(us.state, sdi.july, by = "NAME")

# filter to 48 states
us.sdi.july <- us.sdi.july[!(us.sdi.july$STUSPS %in% c("AK","HI","PR")),]

## covid and demographic ##

# accumuated cases in July
covid.july <- covid[covid$date=="2020-07-01", ]
covid.july.sum <- aggregate(cbind(cases,deaths)~state, data = covid.july, FUN = sum)

# accumuated cases in Aug
covid.aug <- covid[covid$date=="2020-08-01", ]
covid.aug.sum <- aggregate(cbind(cases,deaths)~state, data = covid.aug, FUN = sum)

# new cases in July
covid.jul.aug <- merge(covid.july.sum, covid.aug.sum,by="state")
covid.jul.aug$cases <- covid.jul.aug$cases.y - covid.jul.aug$cases.x
covid.jul.aug$deaths <- covid.jul.aug$deaths.y - covid.jul.aug$deaths.x

# aggregate demogephic data by state
demographic.sum <- aggregate(cbind(population)~state, data = demographic, FUN = sum)
head(demographic.sum,5)

# merge covid and demographic
covid.demographic.july <- merge(covid.jul.aug, demographic.sum, by = "state")
covid.demographic.july$case.rate <- covid.demographic.july$cases / covid.demographic.july$population
covid.demographic.july$death.rate <- covid.demographic.july$deaths / covid.demographic.july$population
covid.demographic.july$death.by.case <- covid.demographic.july$deaths / covid.demographic.july$cases

#### EDA ####

library(tmap)
tmap_mode("plot")

# percentage of population going out
tm_shape(mobility.july) + 
  tm_polygons('Percentage.Not.Staying.at.Home', title="% going out",palette = "Purples") + 
  tm_layout(main.title="Percentage of Population Going out per American State",
            main.title.size=1.2, main.title.position="centre",
            legend.title.size=1,legend.text.size=0.7, legend.position=c("right", "bottom")) +
  tm_compass(type="rose", position = c("left", "bottom"), size = 4) +
  tm_scale_bar(position = c("left", "bottom")) +
  tmap_style("beaver")

# number of trips per population
tm_shape(mobility.july) + 
  tm_polygons('Trips.per.Population', title="No. of trips per person", palette = "Purples") +
  tm_layout(main.title="Average No. of Trips per Population per American State",
            main.title.size=1.2, main.title.position="centre",
            legend.title.size=1,legend.text.size=0.7, legend.position=c("right", "bottom")) +
  tm_compass(type="rose", position = c("left", "bottom"), size = 4) +
  tm_scale_bar(position = c("left", "bottom")) +
  tmap_style("beaver")

# social distancing index per state
tm_shape(us.sdi.july) + 
  tm_polygons('avg.sdi', title="Social Distancing Index", palette = "Purples") +
  tm_layout(main.title="Social Distancing Index per American State",
            main.title.size=1.2, main.title.position="centre",
            legend.title.size=1,legend.text.size=0.7, legend.position=c("right", "bottom")) +
  tm_compass(type="rose", position = c("left", "bottom"), size = 4) +
  tm_scale_bar(position = c("left", "bottom")) +
  tmap_style("beaver")

#### Correlation ####

## mobility with case rate, death rate, death by case ##

m <- merge(mobility.july, covid.demographic.july, by.x = "NAME", by.y = "state")

cor.test(m$case.rate, m$Percentage.Not.Staying.at.Home, method = "pearson")
cor.test(m$death.rate, m$Percentage.Not.Staying.at.Home, method = "pearson")
cor.test(m$death.by.case, m$Percentage.Not.Staying.at.Home, method = "pearson")

cor.test(m$case.rate, m$Trips.per.Population, method = "pearson")
cor.test(m$death.rate, m$Trips.per.Population, method = "pearson")
cor.test(m$death.by.case, m$Trips.per.Population, method = "pearson")

## social distancing index with case rate, death rate, death by case ##

m <- merge(us.sdi.july, covid.demographic.july, by.x = "NAME", by.y = "state")
cor.test(m$case.rate, m$avg.sdi, method = "pearson")
cor.test(m$death.rate, m$avg.sdi, method = "pearson")
cor.test(m$death.by.case, m$avg.sdi, method = "pearson")
