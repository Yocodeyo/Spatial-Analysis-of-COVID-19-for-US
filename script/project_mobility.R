setwd("C:/Users/asu/Desktop/BT4015/Project")
Sys.setenv(LANG = "en")
Sys.setlocale("LC_TIME", "C")

# read US state shape file
library(sf)
us.state <- st_read("cb_2018_us_state_20m/cb_2018_us_state_20m.shp")
head(us.state,5)
# view US state
library(tmap) 
tmap_mode('view')
tm_shape(us.state) + tm_polygons()

# read US county shape file
us.county <- st_read("cb_2018_us_county_20m/cb_2018_us_county_20m.shp")
head(us.county,5)
# view US county
tm_shape(us.county) + tm_polygons()

# read mobility file
mobility <- read.csv("Trips_by_Distance_-_2020.csv")
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
# merge
mobility.july <- merge(us.state, mobility.state.avg.july, 
                           by.x = "STUSPS", by.y = "State.Postal.Code")
head(mobility.july,5)

# view mobility per state
tm_shape(mobility.july) + tm_polygons('Population.Staying.at.Home') 
tm_shape(mobility.july) + tm_polygons('Percentage.Staying.at.Home') 

tm_shape(mobility.july) + tm_polygons('Population.Not.Staying.at.Home') 
tm_shape(mobility.july) + tm_polygons('Percentage.Not.Staying.at.Home', title='%Population not at home')

tm_shape(mobility.july) + tm_polygons('Number.of.Trips')
tm_shape(mobility.july) + tm_polygons('Trips.per.Population')
tm_shape(mobility.july) + tm_polygons('Trips.per.Population.Not.Staying.at.Home',title='Trips per pop not at home')

m <- merge(mobility.july, covid.demographic.july, 
                       by.x = "NAME", by.y = "state")
head(m,5)
cor.test(m$case.rate, m$Percentage.Not.Staying.at.Home, method = "pearson") # 0.3661132
cor.test(m$death.rate, m$Percentage.Not.Staying.at.Home, method = "pearson") # 0.2462652
cor.test(m$death.by.case, m$Percentage.Not.Staying.at.Home, method = "pearson") # -0.1565564
cor.test(m$case.rate, m$Trips.per.Population.Not.Staying.at.Home, method = "pearson") # -0.5046178
cor.test(m$death.rate, m$Trips.per.Population.Not.Staying.at.Home, method = "pearson") # -0.318808 
cor.test(m$death.by.case, m$Trips.per.Population.Not.Staying.at.Home, method = "pearson") # .04420314

### ANIMATION
# merge state with mobility
us.state.mobility <- merge(us.state, mobility.state.avg, 
                           by.x = "STUSPS", by.y = "State.Postal.Code")

# animation over month
Sys.setenv(PATH = paste("D:\\Programs\\ImageMagick-7.0.10-Q16-HDRI",Sys.getenv("PATH"), sep = ";"))
Sys.getenv("PATH")
us.state.mobility2 <- us.state.mobility[!(us.state.mobility$STUSPS %in% c("AK","HI")),]

tmap_mode('plot')

anim<- tm_shape(us.state.mobility2) + tm_polygons('Population.Staying.at.Home') +
  tm_facets(along = "Month", free.coords = FALSE)
tmap_animation(anim, filename = "Population.Staying.at.Home.gif", delay = 100)

anim2<- tm_shape(us.state.mobility2) + tm_polygons('Percentage.Staying.at.Home') +
  tm_facets(along = "Month", free.coords = FALSE)
tmap_animation(anim2, filename = "Percentage.Staying.at.Home.gif", delay = 100)

anim3<- tm_shape(us.state.mobility2) + tm_polygons('Number.of.Trips') +
  tm_facets(along = "Month", free.coords = FALSE)
tmap_animation(anim3, filename = "Number.of.Trips.gif", delay = 100)

anim4<- tm_shape(us.state.mobility2) + tm_polygons('Trips.per.Population') +
  tm_facets(along = "Month", free.coords = FALSE)
tmap_animation(anim4, filename = "Trips.per.Population.gif", delay = 100)

anim5<- tm_shape(us.state.mobility2) + tm_polygons('Trips.per.Population.Not.Staying.at.Home') +
  tm_facets(along = "Month", free.coords = FALSE)
tmap_animation(anim5, filename = "Trips.per.Population.Not.Staying.at.Home.gif", delay = 100)

### CLUSTERING
require(spdep) 
us.state.july.lw <- nb2listw(poly2nb(us.state.july), zero.policy = TRUE)

### GLOBAL
# lagged mean plot
us.state.july$lagged.means1 <- 
  lag.listw(us.state.july.lw, us.state.july$Trips.per.Population.Not.Staying.at.Home) 
tm_shape(us.state.july) + 
  tm_polygons('lagged.means1', title="Lagged Mean of Trips per Population not at Home")

# compute Moran's I
moran.test(us.state.july$Trips.per.Population.Not.Staying.at.Home,
           us.state.july.lw,randomisation=FALSE, zero.policy = TRUE)
moran.test(us.state.july$Trips.per.Population.Not.Staying.at.Home,
           us.state.july.lw,randomisation=TRUE, zero.policy = TRUE)

# simulation-based approach
moran.mc(us.state.july$Trips.per.Population.Not.Staying.at.Home,
         us.state.july.lw,10000, zero.policy = TRUE)

### LOCAL
# Local Moran's I
us.state.july$lI <- localmoran(us.state.july$Trips.per.Population.Not.Staying.at.Home,
                               us.state.july.lw)[, 1]
tm_shape(us.state.july) + 
  tm_polygons(col= 'lI',title= "Local Moran's I",legend.format=list(flag= "+"))

# Local p-values 
us.state.july$pval <- localmoran(us.state.july$Trips.per.Population.Not.Staying.at.Home,
                             us.state.july.lw)[, 5] 
tm_shape(us.state.july) + 
  tm_polygons(col= 'pval' , title= "p-value" , breaks= c(0, 0.01, 0.05, 0.10, 1), border.col = "black",palette = "-Greens")


### COUNTY
# merge county with mobility
# filter to new york state
us.county$County <- paste(us.county$NAME, "County", sep=" ")
us.county$STATEFP <- as.numeric(us.county$STATEFP)
us.county.july <- merge(us.county, mobility.county.avg.july, 
                        by.x = c("County","STATEFP"), by.y = c("County.Name","State.FIPS"))
ny.county.july <- us.county.july[us.county.july$State.Postal.Code=="NY",]

# view mobility at new york state
tm_shape(ny.county.july) + tm_polygons('Population.Staying.at.Home') 
tm_shape(ny.county.july) + tm_polygons('Percentage.Staying.at.Home') 

tm_shape(ny.county.july) + tm_polygons('Population.Not.Staying.at.Home') 
tm_shape(ny.county.july) + tm_polygons('Percentage.Not.Staying.at.Home')

tm_shape(ny.county.july) + tm_polygons('Number.of.Trips')
tm_shape(ny.county.july) + tm_polygons('Trips.per.Population')
tm_shape(ny.county.july) + tm_polygons('Trips.per.Population.Not.Staying.at.Home')

### Social distancing index
sdi <- read.csv("Social distancing index_ST.csv")
head(sdi,5)

# avg of july
sdi.july <- sdi[,c(2,3,186:216)]
sdi.july$avg.sdi <- rowMeans(sdi.july[3:ncol(sdi.july)])
head(sdi.july,5)

# merge sdi with state shape
us.sdi.july <- merge(us.state, sdi.july, by = "NAME")
tm_shape(us.sdi.july) + tm_polygons('avg.sdi', title="Social Distancing Index")

# corr
m <- merge(us.sdi.july, covid.demographic.july, 
           by.x = "NAME", by.y = "state")
head(m,5)
cor.test(m$case.rate, m$avg.sdi, method = "pearson") # 0.0862768 
cor.test(m$death.rate, m$avg.sdi, method = "pearson") # 0.2236712
cor.test(m$death.by.case, m$avg.sdi, method = "pearson") # 0.1812557
