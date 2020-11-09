setwd('C:/Users/Jing Ying/Documents/Y4S1/BT4015/Project/Codes')

### DATA PREPARATION ###
library(sf)

#1. prepare states data
##1.1 read states shp file
states_sf <- st_read('../Datasets/cb_2018_us_state_20m/cb_2018_us_state_20m.shp')
head(states_sf)

##1.2 exclude non-contiguous states
states_sf <- states_sf[!(states_sf$NAME %in% c('Alaska', 'Hawaii', 'Puerto Rico')),]


#2. prepare test sites data
##2.1 read shp file
test_sites_sf <- st_read('../Datasets/Testing_Locations/Testing_Locations.shp')
head(test_sites_sf)

##2.2 select test sites available between 2020-07-01 to 2020-08-01
test_sites_sf <- test_sites_sf[test_sites_sf$CreationDa>='2020-07-01' & test_sites_sf$CreationDa<='2020-08-01',]

##2.3 select test sites that are still open
test_sites_sf <- test_sites_sf[test_sites_sf$status=='Open',]

##2.4 convert to sp object
test_sites_sp <- as_Spatial(test_sites_sf[!st_is_empty(test_sites_sf$geometry), , drop=FALSE])


#3. prepare population data
##3.1 read csv file
pop_df <- read.csv('../Datasets/us_county.csv')
head(pop_df)

##3.2 sum population per state
pop_sum <- aggregate(population~state, data=pop_df, FUN=sum)
head(pop_sum)

##3.3 merge states and population data
all <- merge(states_sf, pop_sum, by.x='NAME', by.y='state')
head(all)


#4. prepare covid data
##4.1 read csv file
covid_df <- read.csv('../Datasets/covid_us_county.csv')
head(covid_df)

##4.2 select covid data for 2020-07-01 to 2020-08-01
covid_df_jul <- covid_df[covid_df$date=='2020-07-01',]
covid_df <- covid_df[covid_df$date=='2020-08-01',]
covid_df$cases <- covid_df$cases-covid_df_jul$cases
covid_df$deaths <- covid_df$deaths-covid_df_jul$deaths

##4.3 sum cases and deaths per state
covid_sum <- aggregate(cbind(cases,deaths)~state, data=covid_df, FUN=sum)
head(covid_sum)

##4.4 merge states and covid data
all <- merge(all, covid_sum, by.x='NAME', by.y='state')
head(all)


#5. create variables of interest
##5.1 case rate
all$case_rate <- all$cases/all$population

##5.2 death rate
all$death_rate <- all$deaths/all$population

##5.3 death rate among detected cases
all$death_among_detected <- all$deaths/all$cases
head(all)

#=============================================================
### EDA ###
library(tmap)

#1. plot test sites
tm_shape(states_sf) + tm_polygons() +
  tm_shape(test_sites_sp) + tm_dots(size=0.05, col='darkgreen') +
  tm_layout(main.title='Test Site Locations in American States',
            main.title.size=1,
            main.title.fontface='bold',
            main.title.position=c('center', 'center')) +
  tm_compass(type='rose', size=1.8, position=c(0.02, 0.15)) +
  tm_scale_bar(position=c(0.02, 0.02))

#=============================================================
### HYPOTHESIS TESTING ###
#H0: distribution of test sites is consistent with CSR
library(rgdal)
library(maptools)
library(raster)
library(spatstat)

#1. convert test sites to ppp object
test_sites_ppp <- test_sites_sp
crs(test_sites_ppp) <- NA
test_sites_ppp <- as.ppp(test_sites_ppp)

#2. convert states to owin object
states_ppp <- as_Spatial(states_sf)
crs(states_ppp) <- NA
states_ppp <- as.owin(states_ppp)

#3. ANN analysis
ann.p <- mean(nndist(test_sites_ppp, k=1))
ann.p #0.07311647

#4. generate null model
n <- 500L #define no. of simulations
ann.r <- vector(length=n) #store simulated ANN values

for (i in 1:n) { #loop to create different simulated distributions
  rand.p <- rpoint(n=test_sites_ppp$n, win=states_ppp)
  ann.r[i] <- mean(nndist(rand.p, k=1))
}

#5. plot histogram of expected values under the null
hist(ann.r, main=NULL, las=1, breaks=40, col="bisque", xlim=range(ann.p, ann.r))
abline(v=ann.p, col="blue")

#6. compute pseudo p-value from simulation
N.greater <- sum(ann.r > ann.p)
p <- min(N.greater+1, n+1-N.greater)/(n+1)
p #0.001996008

#=============================================================
### HEXAGONAL BINNING ###
library(sp)
library(fMultivar)

#1. define function to convert output of hexBinning to spdf
hexbin_map <- function(spdf, ...) {
  hbins <- fMultivar::hexBinning(coordinates(spdf), ...)
  
  #set up hexagons to plot as polygons
  u <- c(1, 0, -1, -1, 0, 1)
  u <- u * min(diff(unique(sort(hbins$x))))
  v <- c(1, 2, 1, -1, -2, -1)
  v <- v * min(diff(unique(sort(hbins$y))))/3
  
  #construct each polygon in sp model 
  hexes_list <- vector(length(hbins$x), mode='list')
  for (i in 1:length(hbins$x)) {
    pol <- Polygon(cbind(u+hbins$x[i], v+hbins$y[i]), hole=FALSE)
    hexes_list[[i]] <- Polygons(list(pol), i)
  }
  
  #build spdf
  hex_cover_sp <- SpatialPolygons(hexes_list, proj4string=CRS(proj4string(spdf)))
  hex_cover <- SpatialPolygonsDataFrame(hex_cover_sp, data.frame(z=hbins$z), match.ID=FALSE)
  
  #return result
  return(hex_cover)
}

#2. put test sites into hexagonal bins
test_sites_hex <- hexbin_map(test_sites_sp, bins=40)

#=============================================================
### SPATIAL ANALYSIS ###
#1. test site density vs case rate
tm_shape(all) + tm_polygons() +
  tm_shape(test_sites_hex) +
    tm_fill(col='z', palette='Greens', alpha=0.9,
            title='No. of Test Sites') +
  tm_shape(all) + tm_bubbles('case_rate', col='orange') +
  tm_layout(main.title='Test Site Density and COVID-19 Case Rate in American States',
            main.title.size=0.8,
            main.title.fontface='bold',
            main.title.position=c('left', 'top'),
            legend.outside=TRUE) +
  tm_compass(type='rose', size=1.5, position=c(0.85, 0.03)) +
  tm_scale_bar(position=c(0.02, 0.03))

#2. test site density vs death rate
tm_shape(all) + tm_polygons() +
  tm_shape(test_sites_hex) +
  tm_fill(col='z', palette='Greens', alpha=0.9,
          title='No. of Test Sites') +
  tm_shape(all) + tm_bubbles('death_rate', col='orange') +
  tm_layout(main.title='Test Site Density and COVID-19 Death Rate in American States',
            main.title.size=0.8,
            main.title.fontface='bold',
            main.title.position=c('left', 'top'),
            legend.outside=TRUE) +
  tm_compass(type='rose', size=1.5, position=c(0.85, 0.03)) +
  tm_scale_bar(position=c(0.02, 0.03))

#3. test site density vs death rate among detected cases
tm_shape(all) + tm_polygons() +
  tm_shape(test_sites_hex) +
  tm_fill(col='z', palette='Greens', alpha=0.9,
          title='No. of Test Sites') +
  tm_shape(all) + tm_bubbles('death_among_detected', col='orange') +
  tm_layout(main.title='Test Site Density and COVID-19 Death Rate among Detected Cases in American States',
            main.title.size=0.8,
            main.title.fontface='bold',
            main.title.position=c('left', 'top'),
            legend.outside=TRUE) +
  tm_compass(type='rose', size=1.5, position=c(0.85, 0.03)) +
  tm_scale_bar(position=c(0.02, 0.03))

tm_shape(states_merged) + tm_polygons() +
  tm_shape(test_sites_hex) + tm_fill(col='z', palette='Greens', alpha=0.9, title='No. of Test Sites') +
  tm_shape(states_merged) + tm_bubbles('death_among_detected', col='orange', popup.vars=c('No. of Deaths'='deaths', 'No. of Cases'='cases', 'Proportion (%)'='death_among_detected'))
