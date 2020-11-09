#### Imports ####

# set working directory
curr_path <- rstudioapi::getActiveDocumentContext()$path
setwd(dirname(curr_path))
#Sys.setenv(LANG = "en")
#Sys.setlocale("LC_TIME", "C")

# load data
library(sf)
us.sf <- st_read("../data/cb_2018_us_state_20m/cb_2018_us_state_20m.shp")
covid <- read.csv("../data/COVID-19_US_County_JHU_Data_&_Demographics/covid_us_county.csv")

#### Preprocessing ####

# filter to every month start
covid$date <- as.Date(covid$date)
covid$month <- format(covid$date,"%m")
covid$day <- format(covid$date,"%d")
covid.month <- covid[covid$day == "01", ]

# aggregate by state
covid.agg <- aggregate(cbind(cases,deaths)~state+date+month, data = covid.month, FUN = sum)

# merge state with covid data of all months
us.state.covid <- merge(us.sf, covid.agg, by.x = "NAME", by.y = "state")

# filter to 48 states
us.state.covid <- us.state.covid[!(us.state.covid$STUSPS %in% c("AK","HI","PR")),]

#### Animation ####

#Sys.setenv(PATH = paste("D:\\Programs\\ImageMagick-7.0.10-Q16-HDRI",Sys.getenv("PATH"), sep = ";"))

library(tmap) 
tmap_mode('plot')

anim <- tm_shape(us.state.covid) + 
  tm_polygons('cases') + 
  tm_compass() + 
  tm_scale_bar() + 
  tm_facets(along = "date", free.coords = FALSE)

tmap_animation(anim, filename = "cases_animation.gif", delay = 100)
