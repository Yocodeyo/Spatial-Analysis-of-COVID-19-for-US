#### Imports ####

#initialise
curr_path <- rstudioapi::getActiveDocumentContext()$path
setwd(dirname(curr_path))
source("functions.R")

#data
beds.sf <- st_read("../data/Definitive_Healthcare_USA_Hospital_Beds/Definitive_Healthcare_Bed_Locations.shx") 
cty.csv <- read_csv("../data/COVID-19_US_County_JHU_Data_&_Demographics/us_county.csv")
cty.sf <- st_read("../data/COVID-19_US_County_JHU_Data_&_Demographics/us_county.shp", stringsAsFactors = FALSE)
covid <- read_csv("../data/COVID-19_US_County_JHU_Data_&_Demographics/covid_us_county.csv")[, c(1,2,3,8,4,5,6,7,9)] 


#### Preprocessing ####

#union based on county's fips
cty.sf$fips <- as.numeric(cty.sf$fips)
cty <- cty.sf %>% 
  group_by(fips) %>%
  summarize() %>%
  ungroup()
plot(cty)

#union based on state_code
counties <- merge(cty, cty.csv, by="fips")
states <- counties %>%
  filter(!(state %in% c("Puerto Rico", "Hawaii", "Alaska", "District of Columbia"))) %>% #noncontiguous states/non-states
  group_by(state) %>%
  summarize() %>%
  ungroup()
plot(states)

#use july covid data
covid.aug1 <- covid %>% filter(date == as.Date("2020-08-01")) 
covid.jul1 <- covid %>% filter(date == "2020-07-01")
covid.jul <- cbind(covid.aug1[, 1:7], covid.aug1[, c(8:9)] - covid.jul1[, c(8:9)]) %>% 
  group_by(state) %>%
  summarise(ncases = sum(cases), ndeaths = sum(deaths)) %>%
  data.frame()

pop <- cty.csv %>%
  group_by(state) %>%
  summarise(npopulation = sum(population)) %>%
  data.frame()

covid.data <- merge(covid.jul, pop, by="state") %>%
  mutate(
    case_rate = ncases / npopulation,
    death_rate = ndeaths / npopulation,
    death_detected_rate = ndeaths / ncases) %>% 
  merge(states, by="state")

rm(covid.aug1, covid.jul1, covid.jul, pop)


#### Hypothesis testing ####

#Reference: https://stackoverflow.com/questions/59597078/use-sf-polygon-object-as-window-in-spatstat
states.flat <- st_transform(states, crs = 6345)
plot(states.flat)
states.owin <- as.owin(as_Spatial(states.flat))

beds.sf.utm <- st_transform(beds.sf, crs = 6345)
beds.sf.utm2 <- beds.sf.utm[!st_is_empty(beds.sf.utm), ]
beds.sp <- as(beds.sf.utm2, "Spatial")
beds.ppp <- as(beds.sp, "ppp")
beds.km <- rescale(beds.ppp, 1000, "km")

ann.p <- mean(nndist(beds.km, k=1))
ann.p

n <- 300
ann.r <- vector(length=n)

for (i in 1:n){
  rand.p <- rpoint(n=beds.km$n, win=states.owin)
  ann.r[i] <- mean(nndist(rand.p, k=1)) 
}
plot(rand.p, pch=16, main=NULL, cols=rgb(0,0,0,0.5))

hist(ann.r, main=NULL, las=1, breaks=40, col="bisque", xlim=range(ann.p, ann.r))
abline(v=ann.p, col="blue")

rm(beds.sf.utm, beds.sf.utm2, ann.p, n)


#### Vector overlay and hexagonal binning ####

#quadrat analysis of number of hospitals (how to get number of licensed beds)?
tmap_mode("view")

beds.hex <- hexbin_map(beds.sp, bins = 100)
tm_shape(as_Spatial(states)) + tm_borders() +
  tm_shape(beds.hex) + tm_fill(col = "z", title = "No. of Hospital Beds", alpha = 0.8) +
  tm_shape(st_as_sf(covid.data)) + tm_bubbles("case_rate", border.lwd = NA, scale = 0.8)

tm_shape(as_Spatial(states)) + tm_borders() +
  tm_shape(beds.hex) + tm_fill(col = "z", title = "No. of Hospital Beds", alpha = 0.8) +
  tm_shape(st_as_sf(covid.data)) + tm_bubbles("death_rate", border.lwd = NA, scale = 0.8)

tm_shape(as_Spatial(states)) + tm_borders() +
  tm_shape(beds.hex) + tm_fill(col = "z", title = "No. of Hospital Beds", alpha = 0.8) +
  tm_shape(st_as_sf(covid.data)) + tm_bubbles("death_detected_rate", border.lwd = NA, scale = 0.8)