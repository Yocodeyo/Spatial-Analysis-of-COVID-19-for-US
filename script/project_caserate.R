setwd("C:/Users/asu/Desktop/BT4015/Project")
Sys.setenv(LANG = "en")
Sys.setlocale("LC_TIME", "C")

# read covid file
covid <- read.csv("covid_us_county.csv")
head(covid,5)

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
head(covid.jul.aug,5)

# read demographic file
demographic <- read.csv('us_county.csv')
head(demographic,5)

# aggregate by state
demographic.sum <- aggregate(cbind(population)~state, data = demographic, FUN = sum)
head(demographic.sum,5)

# merge covid and demographic
covid.demographic.july <- merge(covid.jul.aug, demographic.sum, by = "state")
covid.demographic.july$case.rate <- covid.demographic.july$cases / covid.demographic.july$population
covid.demographic.july$death.rate <- covid.demographic.july$deaths / covid.demographic.july$population
covid.demographic.july$death.by.case <- covid.demographic.july$deaths / covid.demographic.july$cases
head(covid.demographic.july, 5)

# read US shape file
library(sf)
us.sf <- st_read("cb_2018_us_state_20m/cb_2018_us_state_20m.shp")
head(us.sf,5)

# view US shape file
library(tmap) 
tmap_mode('view')
tm_shape(us.sf) + tm_polygons()

# merge shape with covid & demographic
case.rate.july <- merge(us.sf, covid.demographic.july, by.x = "NAME", by.y = "state")

head(case.rate.july, 5)

# view cases per state (California, Texas, Florida, New York)
tm_shape(case.rate.july) + tm_polygons('cases')
tm_shape(case.rate.july) + tm_polygons('case.rate')
tm_shape(case.rate.july) + tm_polygons('death.rate')
tm_shape(case.rate.july) + tm_polygons('death.by.case')

### ANIMATION

# filter to every month start
covid$date <- as.Date(covid$date)
covid$month <- format(covid$date,"%m")
covid$day <- format(covid$date,"%d")
covid.month <- covid[covid$day == "01", ]

# aggregate by state
covid.agg <- aggregate(cbind(cases,deaths)~state+date+month, data = covid.month, FUN = sum)

# merge state with covid data of all months
us.state.covid <- merge(us.sf, covid.agg, by.x = "NAME", by.y = "state")

# animation over month
Sys.setenv(PATH = paste("D:\\Programs\\ImageMagick-7.0.10-Q16-HDRI",Sys.getenv("PATH"), sep = ";"))
# Sys.getenv("PATH")
us.state.covid <- us.state.covid[!(us.state.covid$STUSPS %in% c("AK","HI","PR")),]
library(tmap) 
tmap_mode('plot')

anim <- tm_shape(us.state.covid) + tm_polygons('cases') + tm_facets(along = "date", free.coords = FALSE)
tmap_animation(anim, filename = "cases_animation.gif", delay = 100)
