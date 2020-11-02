#1. set working directory
setwd('C:/Users/Jing Ying/Documents/Y4S1/BT4015/Project/Codes')


#2. read states shp file
library(sf)
states_sf <- st_read('../Datasets/cb_2018_us_state_20m/cb_2018_us_state_20m.shp')
head(states_sf)


#3. prepare population data
##3.1 read csv file
pop_df <- read.csv('../Datasets/us_county.csv')
head(pop_df)

##3.2 sum population per state
pop_sum <- aggregate(population~state, data=pop_df, FUN=sum)
head(pop_sum)

##3.3 merge states and population data
states_merged <- merge(states_sf, pop_sum, by.x='NAME', by.y='state')
head(states_merged)


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
states_merged <- merge(states_merged, covid_sum, by.x='NAME', by.y='state')
head(states_merged)

##4.5 find covid case rate
states_merged$case_rate <- states_merged$cases/states_merged$population*100

##4.6 find covid death rate
states_merged$death_rate <- states_merged$deaths/states_merged$population*100

##4.7 find covid death rate among cases
states_merged$death_case_rate <- states_merged$deaths/states_merged$cases*100
head(states_merged)


#5. prepare test sites data
##5.1 read shp file
test_sites_sf <- st_read('../Datasets/Testing_Locations/Testing_Locations.shp')
head(test_sites_sf)

##5.2 select test sites available between 2020-07-01 to 2020-08-01
test_sites_sf <- test_sites_sf[test_sites_sf$CreationDa>='2020-07-01' & test_sites_sf$CreationDa<='2020-08-01',]

##5.3 convert to sp object
test_sites_sp <- as_Spatial(test_sites_sf[!st_is_empty(test_sites_sf$geometry), , drop=FALSE])


#6. hexagonal binning of test sites
##6.1 define function to convert output of hexBinning to spdf
library(sp)
library(fMultivar)
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

##6.2 view map
library(tmap)
tmap_mode('view')
test_sites_hex <- hexbin_map(test_sites_sp, bins=40)

###6.2.1 with covid case rate
tm_shape(states_merged) + tm_polygons() +
  tm_shape(test_sites_hex) + tm_fill(col='z', palette='Greens', alpha=0.9, title='No. of Test Sites') +
  tm_shape(states_merged) + tm_bubbles('case_rate', popup.vars=c('No. of Cases'='cases', 'Population Size'='population', 'Proportion (%)'='case_rate'))

###6.2.2 with covid death rate
tm_shape(states_merged) + tm_polygons() +
  tm_shape(test_sites_hex) + tm_fill(col='z', palette='Greens', alpha=0.9, title='No. of Test Sites') +
  tm_shape(states_merged) + tm_bubbles('death_rate', popup.vars=c('No. of Deaths'='deaths', 'Population Size'='population', 'Proportion (%)'='death_rate'))

###6.2.3 with covid death rate among case
tm_shape(states_merged) + tm_polygons() +
  tm_shape(test_sites_hex) + tm_fill(col='z', palette='Greens', alpha=0.9, title='No. of Test Sites') +
  tm_shape(states_merged) + tm_bubbles('death_case_rate', popup.vars=c('No. of Deaths'='deaths', 'No. of Cases'='cases', 'Proportion (%)'='death_case_rate'))


#7. hypothesis testing of test sites
library(rgdal)
library(maptools)
library(raster)
library(spatstat)

#convert test sites to ppp object
test_sites_ppp <- test_sites_sp
crs(test_sites_ppp) <- NA
test_sites_ppp <- as.ppp(test_sites_ppp)

#convert states to owin object
states_ppp <- as_Spatial(states_sf)
crs(states_ppp) <- NA
states_ppp <- as.owin(states_ppp)

##7.1 test with uniformity - hypothesise that distribution of test sites is consistent with CSR
###7.1.1 ANN analysis
ann.p <- mean(nndist(test_sites_ppp, k=1))
ann.p #0.07311647

###7.1.2 generate null model
n <- 500L #define no. of simulations
ann.r <- vector(length=n) #store simulated ANN values

for (i in 1:n) { #loop to create different simulated distributions
  rand.p <- rpoint(n=test_sites_ppp$n, win=states_ppp)
  ann.r[i] <- mean(nndist(rand.p, k=1))
}

###7.1.3 plot histogram of expected values under the null
hist(ann.r, main=NULL, las=1, breaks=40, col="bisque", xlim=range(ann.p, ann.r))
abline(v=ann.p, col="blue")

###7.1.4 compute pseudo p-value from simulation
N.greater <- sum(ann.r > ann.p)
p <- min(N.greater+1, n+1-N.greater)/(n+1)
p #0.001996008

##7.2 test with influence of case rate - hypothesise that distribution of test sites is wrt case rate
###7.2.1 convert case rate to raster object
case_rate_r <- raster(nrow=180, ncols=360, ext=extent(states_merged))
case_rate_r <- rasterize(states_merged, case_rate_r, 'case_rate')

###7.2.2 generate null model
n <- 500L #define no. of simulations
ann.r <- vector(length=n) #store simulated ANN values

for (i in 1:n) { #loop to create different simulated distributions
  rand.p <- rpoint(n=test_sites_ppp$n, f=as.im(case_rate_r))
  ann.r[i] <- mean(nndist(rand.p, k=1))
}
Window(rand.p) <- states_ppp

###7.2.3 plot histogram of expected values under the null
hist(ann.r, main=NULL, las=1, breaks=40, col="bisque", xlim=range(ann.p, ann.r))
abline(v=ann.p, col="blue")

###7.2.4 compute pseudo p-value from simulation
N.greater <- sum(ann.r > ann.p)
p <- min(N.greater+1, n+1-N.greater)/(n+1)
p #0.001996008