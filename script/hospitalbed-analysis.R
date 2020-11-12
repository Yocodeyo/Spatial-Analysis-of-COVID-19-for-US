# This script was written by Koay Tze Min for the final project of the BT4015 Geospatial Analytics module in NUS. It imports,
# cleans, and analyses polygon demographic and Covid-19 data, together with point data on hospital bed availability across 
# American states and counties. Analyses are both descriptive and spatial. It begins with several rounds of exploring and 
# preprocessing data, before plotting various types of maps and functions. Not all of the latter made it into the presentation  
# and report, but they were left in this script for reference.

library(tidyverse)
library(RColorBrewer)
library(sf)
library(tmap)
library(rgdal)
library(spatstat) # point pattern analysis
library(maptools)
library(spdep) # spatial autocorrelation
library(ggrepel)
library(fMultivar) # hexagonal binning for quadrant analysis

# set paths
curr_path <- rstudioapi::getActiveDocumentContext()$path
setwd(dirname(curr_path))
source("functions.R")

# get data
hospitalbed.file <- "../data/Definitive_Healthcare_USA_Hospital_Beds/Definitive_Healthcare_Bed_Locations.shx"
demographics.file <- "../data/COVID-19_US_County_JHU_Data_&_Demographics/us_county.csv"
boundaries.file <- "../data/COVID-19_US_County_JHU_Data_&_Demographics/us_county.shp"
covid.file <- "../data/COVID-19_US_County_JHU_Data_&_Demographics/covid_us_county.csv"

beds.sf <- st_read(hospitalbed.file) 
beds.sp <- readOGR(hospitalbed.file)
cty.csv <- read_csv(demographics.file) # cty=county
cty.sf <- st_read(boundaries.file, stringsAsFactors=FALSE)
covid <- read_csv(covid.file)[, c(1,2,3,8,4,5,6,7,9)] 

#### EDA I ####

# check for NAs in data, to be taken note of during spatial analysis later
str(beds.sf)
str(cty.csv)
str(cty.sf)
str(covid)
colSums(is.na(beds.sf))
colSums(is.na(cty.csv))
colSums(is.na(cty.sf))
colSums(is.na(covid))

#### Preprocessing I: States/Counties and Covid ####

## This section assumes knowledge of other members' exploratory findings, e.g. we're interested only in July's cases and deaths, 
## and in states or districts within the American mainland. Since the cases and deaths are accumulated daily, we take July's data
## by taking August 1st - July 1st. We're also interested in representing Covid's severity with case rate, death rate, and death
## among detected rate instead.

# merge cty.sf and cty.csv based on county's fips, and remove noncontiguous states
cty.sf$fips <- as.numeric(cty.sf$fips)

cty <- 
  cty.sf %>% 
  group_by(fips) %>%
  summarize() %>%
  ungroup()
#plot(cty)

counties.full <- 
  cty %>%
  merge(cty.csv, by="fips") %>%
  dplyr::filter(!state %in% c("Puerto Rico", "Hawaii", "Alaska")) 

counties <- 
  counties.full %>% 
  select(fips, county, geometry)
#plot(counties)

# summarise county data to state-level, based on state_code
states <- 
  counties.full %>%
  group_by(state, state_code) %>%
  summarize() %>%
  ungroup()
#plot(states)

# select only Covid data in July, and summarise it to state-level
covid.toaug1 <- 
  covid %>% 
  dplyr::filter(date == "2020-08-01")

covid.tojul1 <- 
  covid %>% 
  dplyr::filter(date == "2020-07-01")

covid.jul.state <- 
  covid.toaug1[, 1:7] %>%
  cbind(covid.toaug1[, c(8:9)] - covid.tojul1[, c(8:9)]) %>%
  group_by(state) %>%
  summarise(ncases = sum(cases), ndeaths = sum(deaths)) %>%
  data.frame()

# summarise population size and the computed values of case rate, death rate, and death among detected rate, to state-level
pop.state <- 
  cty.csv %>%
  group_by(state) %>%
  summarise(npopulation = sum(population)) %>%
  data.frame()

covid.data <- 
  covid.jul.state %>%
  merge(pop.state, by="state") %>%
  mutate(
    case_rate = ncases / npopulation,
    death_rate = ndeaths / npopulation,
    death_detected_rate = ndeaths / ncases) %>% 
  merge(states, by="state")

print("covid.data is Covid data by state and it looks like this:")
head(covid.data)

# repeat the above two steps, except to county-level, based on fips and county
covid.jul.cty <- 
  covid.toaug1[, 1:7] %>%
  cbind(covid.toaug1[, c(8:9)] - covid.tojul1[, c(8:9)]) %>% 
  drop_na(state_code, fips) %>%
  group_by(fips, county) %>%
  summarise(ncases = sum(cases), ndeaths = sum(deaths)) %>%
  data.frame()

covid.jul.cty %>% dplyr::filter(ncases < 0) # a few weird errors of negative numbers of cases and deaths
covid.jul.cty %>% dplyr::filter(ndeaths < 0)

covid.jul.cty <- 
  covid.jul.cty %>%
  dplyr::filter(ncases >= 0 & ndeaths >= 0) # get rid of them

pop.cty <- 
  cty.csv %>%
  group_by(fips) %>%
  summarise(npopulation = sum(population)) %>%
  data.frame()

covid.data.cty <- 
  covid.jul.cty %>%
  merge(pop.cty, by="fips") %>%
  mutate(
    case_rate = ncases / npopulation,
    death_rate = ndeaths / npopulation,
    death_detected_rate = ndeaths / ncases) %>% 
  drop_na(case_rate, death_rate, death_detected_rate) %>%
  merge(counties, by="fips") %>%
  select(-county.y) %>%
  rename(county = county.x)

print("covid.data.cty is Covid data by county and it looks like this:")
head(covid.data.cty)

rm(covid.tojul1, covid.toaug1, covid.jul.state, covid.jul.cty)

#### Preprocessing I: Hospital Bed Availability ####

# hospitals are of different types; some don't seem to be meant for the general public or don't take in Covid patients
hospitaltypes <- 
  beds.sf$HOSPITAL_T %>% 
  table() %>% 
  data.frame() %>% 
  rename(Hospital_Type = ".")

ggplot(hospitaltypes, aes(x=Hospital_Type, y=Freq)) + 
  geom_bar(stat="identity") + 
  labs(title="Frequency of Hospital Types in America", x="Hospital Type", y="Frequency") +
  coord_flip() +
  theme_minimal()

# remove the irrelevant hospitals based on their types
noncovidhospitals <- c("VA Hospital", "Rehabilitation Hospital", "Psychiatric Hospital", "Department of Defense Hospital")

beds.sf <- 
  beds.sf %>% 
  dplyr::filter(!(HOSPITAL_T %in% noncovidhospitals))

newhospitaltypes <- 
  beds.sf$HOSPITAL_T %>% 
  table() %>% 
  data.frame() %>% 
  rename(Hospital_Type = ".")

# this is the percentage of points left after removal:
sum(newhospitaltypes$Freq) / sum(hospitaltypes$Freq)

# colour the above barplot to highlight the removed hospital types
hospitaltypes$AcceptsCovidPatients <- 
  ifelse(!(hospitaltypes$Hospital_Type %in% noncovidhospitals), "Yes", "No")

ggplot(hospitaltypes, aes(x=Hospital_Type, y=Freq, fill=AcceptsCovidPatients)) + 
  geom_bar(stat="identity") + 
  labs(title="Frequency of Hospital Types in America", x="Hospital Type", y="Frequency") +
  coord_flip() +
  theme_minimal()

# remove noncontiguous states
beds.sf <- 
  beds.sf %>% 
  dplyr::filter(!(STATE_NAME %in% c("Puerto Rico", "Hawaii", "Alaska"))) 

#### EDA II + Preprocessing II: Rate Data ####

## We perform descriptive analyses on our freshly-cleaned data, esp. computed values of case rates, death rates and death among 
## detected rates. Do additional cleaning if necessary.

# examine non-spatial relationships between rates in each state, through a scatterplot
ggplot(covid.data, aes(x=case_rate, y=death_rate, color=death_detected_rate)) +
  geom_point() +
  scale_color_gradient(low="pink", high="red") +
  geom_text_repel(aes(label=state_code)) +
  labs(
    title="Death Rate vs Case Rate vs Death among Detected Rate per State",
    subtitle="July 2020",
    x="Case Rate", 
    y="Death Rate", 
    color="Death Detected Rate") +
  theme_minimal() +
  theme(legend.position=c(0.9, 0.2))

# examine states' case rates with histogram
summary(covid.data$case_rate)

ggplot(covid.data, aes(x=case_rate)) + 
  geom_histogram(binwidth=0.001, boundary=0, color="black", fill="white") +
  scale_x_continuous(breaks=seq(0, 0.016, 0.001)) +
  labs(
    title="Histogram of Case Rates (State Aggregated)",
    subtitle="= #Cases/Population\nMax = 0.01543596",
    x="Case Rate",
    y="Count") +
  theme_minimal()

# examine states' death rates with histogram
summary(covid.data$death_rate)

ggplot(covid.data, aes(x=death_rate)) + 
  geom_histogram(binwidth=0.00001, boundary=0, color="black", fill="white") +
  scale_x_continuous(breaks=seq(0, 0.0003, 0.00005)) +
  labs(
    title="Histogram of Death Rates (State Aggregated)",
    subtitle="= #Deaths/Population\nMax = 0.0002949608",
    x="Death Rate",
    y="Count") +
  theme_minimal()

# examine states' death among detected rates with histogram
summary(covid.data$death_detected_rate)

ggplot(covid.data, aes(x=death_detected_rate)) + 
  geom_histogram(binwidth=0.005, boundary=0, color="black", fill="white") +
  scale_x_continuous(breaks=seq(0, 0.08, 0.01)) +
  labs(
    title="Histogram of Death among Detected Rates (State Aggregated)",
    subtitle="= #Deaths/#Cases\nMax = 0.07845492",
    x="Death among Detected Rate",
    y="Count") +
  theme_minimal()

# examine counties' case rates with histogram
summary(covid.data.cty$case_rate)

ggplot(covid.data.cty, aes(x=case_rate)) + 
  geom_histogram(binwidth=0.0005, boundary=0, color="black", fill="white") +
  scale_x_continuous(breaks=seq(0, 0.05, 0.0025)) +
  labs(
    title="Histogram of Case Rates (County Aggregated)",
    subtitle="= #Cases/Population\nMax = 0.04643002",
    x="Case Rate",
    y="Count") +
  theme_minimal()

# examine counties' death rates with histogram
summary(covid.data.cty$death_rate)

ggplot(covid.data.cty, aes(x=death_rate)) + 
  geom_histogram(binwidth=0.00001, boundary=0, color="black", fill="white") +
  scale_x_continuous(breaks=seq(0, 0.002, 0.0005)) +
  labs(
    title="Histogram of Death Rates (County Aggregated)",
    subtitle="= #Deaths/Population\nMax = 0.001680672",
    x="Death Rate",
    y="Count") +
  theme_minimal()

# examine counties' death among detected rates with boxplot and histogram
sum(is.na(covid.data.cty$death_detected_rate))

summary(covid.data.cty$death_detected_rate)

ggplot(covid.data.cty, aes(x=death_detected_rate)) +
  geom_boxplot()

ggplot(covid.data.cty, aes(x=death_detected_rate)) + 
  geom_histogram(binwidth=0.0005, boundary=0, color="black", fill="white") +
  scale_x_continuous(breaks=seq(0, 0.5, 0.001)) +
  labs(
    title="Histogram of Death among Detected Rates (County Aggregated)",
    subtitle="= #Deaths/#Cases\nMax = 0.5",
    x="Death among Detected Rate",
    y="Count") +
  theme_minimal()

# convert from st to sf
covid.data.sf <- st_as_sf(covid.data)
covid.data.cty.sf <- st_as_sf(covid.data.cty)

#### EDA II + Preprocessing II: Hospital Bed Availability ####

# clean beds.sf
numNA <- colSums(is.na(beds.sf))

beds.sf.clean <- 
  beds.sf %>% 
  select(-HQ_ADDRE_1) %>% 
  drop_na()

# examine licensed beds data per state, which seems the cleanest out of all bed types
licensed <- 
  beds.sf.clean %>% 
  select(NUM_LICENS, geometry)

summary(licensed$NUM_LICENS)

ggplot(licensed, aes(x=NUM_LICENS)) +
  geom_boxplot()

ggplot(licensed, aes(x=NUM_LICENS)) + 
  geom_histogram(binwidth=10, boundary=0, color="black", fill="white") +
  scale_x_continuous(breaks=seq(0, 700, 50)) +
  theme_classic()

# examine icu beds
icu <- 
  beds.sf.clean %>% 
  select(NUM_ICU_BE, geometry)

summary(icu$NUM_ICU_BE)

ggplot(icu, aes(x=NUM_ICU_BE)) +
  geom_boxplot()

# examine ventilator count
vent <- 
  beds.sf.clean %>% 
  select(AVG_VENTIL, geometry)

summary(vent$AVG_VENTIL)

ggplot(vent, aes(x=AVG_VENTIL)) +
  geom_boxplot()

# convert point to polygon data by aggregating licensed bed per capita/per no. of cases by county
beds.sp.clean <- st_as_sf(beds.sf)
beds.sp.clean$STATE_FIPS <- as.character(beds.sp.clean$STATE_FIPS)
beds.sp.clean$CNTY_FIPS <- as.character(beds.sp.clean$CNTY_FIPS)

beds.sp.clean$fips <- 
  with(beds.sp.clean, paste0(STATE_FIPS, CNTY_FIPS)) %>%
  as.numeric()

beds.cty <- 
  beds.sp.clean %>%
  group_by(fips, COUNTY_NAM) %>%
  summarise(nlicens = sum(NUM_LICENS)) %>%
  data.frame() %>%
  merge(pop.cty, by="fips", all.y=TRUE) %>%
  merge(covid.data.cty, by="fips", all.y=TRUE) %>%
  select(-COUNTY_NAM, -geometry.x, -npopulation.x) %>%
  rename(geometry = geometry.y,
         npopulation = npopulation.y) %>%
  mutate(licens_percapita = nlicens / npopulation,
         licens_percapita = replace_na(licens_percapita, 0),
         licens_percases = nlicens / ncases,
         licens_percases = replace_na(licens_percases, 0),
         licens_perdeaths = nlicens / ndeaths,
         licens_perdeaths = replace_na(licens_perdeaths, 0)) %>%
  st_as_sf()

# summary stats for licensed bed per capita by county
summary(beds.cty$licens_percapita)

ggplot(beds.cty, aes(x=licens_percapita)) +
  geom_boxplot()

ggplot(beds.cty, aes(x=licens_percapita)) + 
  geom_histogram(binwidth=0.0001, boundary=0, color="black", fill="white") +
  scale_x_continuous(breaks=seq(0, 0.035, 0.001)) +
  theme_classic()

# summary stats for licensed bed per no. of cases by county
summary(beds.cty$licens_percases)

ggplot(beds.cty, aes(x=licens_percases)) +
  geom_boxplot()

ggplot(beds.cty, aes(x=licens_percases)) +
  geom_histogram(binwidth=0.1, boundary=0, color="black", fill="white") +
  scale_x_continuous(breaks=seq(0, 1.5, 0.1)) +
  theme_classic()

# log transform the skewed beds.cty licens_percases data
beds.cty$licens_percases.lg <- log(beds.cty$licens_percases)

summary(beds.cty$licens_percases.lg)

ggplot(beds.cty, aes(x=licens_percases.lg)) + 
  geom_histogram(color="black", fill="white") +
  theme_classic()

# summary stats for licensed bed per no. of deaths by county
summary(beds.cty$licens_perdeaths)

ggplot(beds.cty, aes(x=licens_perdeaths)) +
  geom_boxplot()

ggplot(beds.cty, aes(x=licens_perdeaths)) + 
  geom_histogram(binwidth=1, boundary=0, color="black", fill="white") +
  scale_x_continuous(breaks=seq(0, 40, 1)) +
  theme_classic()

# convert point to polygon data by aggregating licensed bed per capita/per no. of cases by state
beds.state <-
  beds.sp.clean %>%
  group_by(STATE_NAME) %>%
  summarise(nlicens = sum(NUM_LICENS)) %>%
  data.frame() %>%
  merge(pop.state, by.x="STATE_NAME", by.y="state") %>%
  merge(covid.data, by.x="STATE_NAME", by.y="state") %>%
  select(-geometry.x, -npopulation.x) %>%
  rename(geometry = geometry.y, state = STATE_NAME, npopulation = npopulation.y) %>%
  mutate(licens_percapita = nlicens / npopulation,
         licens_percapita = replace_na(licens_percapita, 0),
         licens_percases = nlicens / ncases,
         licens_percases = replace_na(licens_percases, 0),
         licens_perdeaths = nlicens / ndeaths,
         licens_perdeaths = replace_na(licens_perdeaths, 0)) %>%
  st_as_sf()

# summary stats for licensed bed per capita by state
summary(beds.state$licens_percapita)

ggplot(beds.state, aes(x=licens_percapita)) +
  geom_boxplot()

ggplot(beds.state, aes(x=licens_percapita)) + 
  geom_histogram(binwidth=0.0001, boundary=0, color="black", fill="white") +
  scale_x_continuous(breaks=seq(0, 0.006, 0.0005)) +
  theme_classic()

# summary stats for licensed bed per no. of cases by state
summary(beds.state$licens_percases)

ggplot(beds.state, aes(x=licens_percases)) +
  geom_boxplot()

ggplot(beds.state, aes(x=licens_percases)) + 
  geom_histogram(binwidth=0.1, boundary=0, color="black", fill="white") +
  scale_x_continuous(breaks=seq(0, 8, 0.5)) +
  theme_classic()

# log transform the skewed beds.state licens_percases data
beds.state$licens_percases.lg <- log(beds.state$licens_percases)

summary(beds.state$licens_percases.lg)

ggplot(beds.state, aes(x=licens_percases.lg)) + 
  geom_histogram(binwidth=0.1, boundary=-7, color="black", fill="white") +
  scale_x_continuous(breaks=seq(-7, -5, 0.5)) +
  theme_classic()

# summary stats for licensed bed per no. of deaths by state
summary(beds.state$licens_perdeaths)

ggplot(beds.state, aes(x=licens_perdeaths)) +
  geom_boxplot()

ggplot(beds.state, aes(x=licens_perdeaths)) +
  geom_histogram(binwidth=10, boundary=0, color="black", fill="white") +
  scale_x_continuous(breaks=seq(0, 1490, 50)) +
  theme_classic()

#### Maps of Covid Rates ####

# county and state borders
tm_shape(as_Spatial(counties)) + tm_borders(col = "grey40", lwd = 0.5) +
  tm_shape(as_Spatial(states)) + tm_borders(col = "black", lwd = 1) 

# case rate by state
tm_shape(covid.data.sf) + 
  tm_polygons("case_rate", breaks=seq(0,0.016,by=0.001), title="Case Rate") +
  tm_layout(main.title="Case Rate per American State",
            main.title.size=1.5, main.title.position="centre",
            legend.title.size=1,legend.text.size=0.7, legend.position=c("right", "bottom"),
            frame=TRUE) +
  tm_compass(type="rose", position = c("left", "bottom")) +
  tm_scale_bar(width=0.5, position = c("left", "bottom")) +
  tmap_style("beaver")

# death rate by state
tm_shape(covid.data.sf) + 
  tm_polygons("death_rate", breaks=seq(0,0.0003,by=0.00005), title="Death Rate") +
  tm_layout(main.title="Death Rate per American State",
            main.title.size=1.5, main.title.position="centre",
            legend.title.size=1,legend.text.size=0.7, legend.position=c("right", "bottom"),
            frame=TRUE) +
  tm_compass(type="rose", position = c("left", "bottom")) +
  tm_scale_bar(width=0.5, position = c("left", "bottom")) +
  tmap_style("beaver")

# death among detected rate by state
tm_shape(covid.data.sf) + 
  tm_polygons("death_detected_rate", breaks=seq(0,0.08,by=0.005), title="Death/Cases Rate") +
  tm_layout(main.title="Death Among Cases Rate per American State",
            main.title.size=1.5, main.title.position="centre",
            legend.title.size=1,legend.text.size=0.7, legend.position=c("right", "bottom"),
            frame=TRUE) +
  tm_compass(type="rose", position = c("left", "bottom")) +
  tm_scale_bar(width=0.5, position = c("left", "bottom")) +
  tmap_style("beaver")

#### Maps of Covid Rates and Licensed Beds by State ####

## We look at licensed beds in the hospital bed availability dataset only, since it's the cleanest.

# toggle between either
tmap_mode("view") 
tmap_mode("plot")

# licensed beds by state and cases rate
tm_shape(covid.data.sf) + 
  tm_polygons("case_rate", 
              breaks=seq(0,0.016,by=0.002), 
              title="Case Rate",
              palette="Greys") +
  tm_shape(licensed) +
  tm_dots("NUM_LICENS", 
          title="No. of Licensed Beds", 
          breaks=c(0,20,40,60,100,200,400,2100),
          size=0.06,
          palette="BuGn",
          colorNA=NULL) +
  tm_layout(main.title="Location and Number of Licensed Beds\nacross Case Rate per American State",
            main.title.size=1.5, main.title.position="centre",
            legend.title.size=1,legend.text.size=0.7, legend.position=c("right", "bottom"),
            frame=TRUE) +
  tm_compass(type = "rose", position = c("left", "bottom")) +
  tm_scale_bar(width = 0.5, position = c("left", "bottom")) +
  tmap_style("white")

# licensed beds by state and death rate
tm_shape(covid.data.sf) + 
  tm_polygons("death_rate", 
              breaks=seq(0,0.0003,by=0.00005), 
              title="Death Rate",
              palette="Greys") +
  tm_shape(licensed) +
  tm_dots("NUM_LICENS", 
          title="No. of Licensed Beds", 
          breaks=c(0,20,40,60,100,200,400,2100),
          size=0.06,
          palette="BuGn",
          colorNA=NULL) +
  tm_layout(main.title="Location and Number of Licensed Beds\nacross Death Rate per American State",
            main.title.size=1.5, main.title.position="centre",
            legend.title.size=1,legend.text.size=0.7, legend.position=c("right", "bottom"),
            frame=TRUE) +
  tm_compass(type = "rose", position = c("left", "bottom")) +
  tm_scale_bar(width = 0.5, position = c("left", "bottom")) +
  tmap_style("white")

# licensed beds by state and death among detected
tm_shape(covid.data.sf) + 
  tm_polygons("death_detected_rate", 
              breaks=seq(0,0.08,by=0.005), 
              title="Death/Cases Rate",
              palette="Greys") +
  tm_shape(licensed) +
  tm_dots("NUM_LICENS", 
          title="No. of Licensed Beds", 
          breaks=c(0,20,40,60,100,200,400,2100),
          size=0.06,
          palette="BuGn",
          colorNA=NULL) +
  tm_layout(main.title="Location and Number of Licensed Beds\nacross Death Among Cases Rate per American State",
            main.title.size=1.5, main.title.position="centre",
            legend.title.size=1,legend.text.size=0.7, legend.position=c("right", "bottom"),
            frame=TRUE) +
  tm_compass(type = "rose", position = c("left", "bottom")) +
  tm_scale_bar(width = 0.5, position = c("left", "bottom")) +
  tmap_style("white")

#### Maps of Covid Rates and Licensed Beds by County ####

# toggle between either
tmap_mode("view") 
tmap_mode("plot")

# licensed beds by county and case rate
tm_shape(covid.data.cty.sf) + 
  tm_polygons("case_rate", 
              breaks=c(0, 1.447e-03, 3.006e-03, 6.510e-03, 0.5), 
              title="Case Rate",
              palette="Greys",
              border.col="grey65") +
  tm_shape(covid.data.sf) +
  tm_borders(col="black", lwd=1) + 
  tm_shape(licensed) +
  tm_dots("NUM_LICENS", 
          title="No. of Licensed Beds", 
          breaks=c(0,20,40,60,100,200,400,2100),
          size=0.06,
          palette="BuGn",
          colorNA=NULL) +
  tm_layout(main.title="Location and Number of Licensed Beds across Case Rate per American County",
            main.title.size=1.5, main.title.position="centre",
            legend.title.size=1,legend.text.size=0.7, legend.position=c("right", "bottom"),
            frame=TRUE) +
  tm_compass(type = "rose", position = c("left", "bottom")) +
  tm_scale_bar(width = 0.5, position = c("left", "bottom")) +
  tmap_style("white")

# licensed beds by county and death rate
tm_shape(covid.data.cty.sf) + 
  tm_polygons("death_rate", 
              breaks=c(0, 2.205e-05, 8.553e-05, 1.681e-03), 
              title="Death Rate",
              palette="Greys",
              border.col="grey65") +
  tm_shape(covid.data.sf) +
  tm_borders(col="black", lwd=1) + 
  tm_shape(licensed) +
  tm_dots("NUM_LICENS", 
          title="No. of Licensed Beds", 
          breaks=c(0,20,40,60,100,200,400,2100),
          size=0.06,
          palette="BuGn",
          colorNA=NULL) +
  tm_layout(main.title="Location and Number of Licensed Beds across Death Rate per American County",
            main.title.size=1.5, main.title.position="centre",
            legend.title.size=1,legend.text.size=0.7, legend.position=c("right", "bottom"),
            frame=TRUE) +
  tm_compass(type = "rose", position = c("left", "bottom")) +
  tm_scale_bar(width = 0.5, position = c("left", "bottom")) +
  tmap_style("white")

# licensed beds by county and death among detected rate
tm_shape(covid.data.cty.sf) + 
  tm_polygons("death_detected_rate", 
              breaks=c(0, 0.006029, 0.017544, 0.5), 
              title="Death among Detected Rate",
              palette="Greys",
              border.col="grey65") +
  tm_shape(covid.data.sf) +
  tm_borders(col="black", lwd=1) + 
  tm_shape(licensed) +
  tm_dots("NUM_LICENS", 
          title="No. of Licensed Beds", 
          breaks=c(0,20,40,60,100,200,400,2100),
          size=0.06,
          palette="BuGn",
          colorNA=NULL) +
  tm_layout(main.title="Location and Number of Licensed Beds across Death among Detected Rate per American County",
            main.title.size=1.5, main.title.position="centre",
            legend.title.size=1,legend.text.size=0.7, legend.position=c("right", "bottom"),
            frame=TRUE) +
  tm_compass(type = "rose", position = c("left", "bottom")) +
  tm_scale_bar(width = 0.5, position = c("left", "bottom")) +
  tmap_style("white")

#### Maps of Licensed Beds per Capita ####

# toggle between either
tmap_mode("view") 
tmap_mode("plot")

# licensed beds per capita by county
tm_shape(beds.cty) +
  tm_polygons("licens_percapita",
              breaks=c(0,0.0001,0.001,0.002,0.003,0.007,0.04),
              title="Licensed Beds Per Capita",
              palette="-BuGn",
              border.col="green4") +
  tm_shape(covid.data.sf) +
  tm_borders(col="black", lwd=1.2) +
  tm_layout(main.title="Licensed Beds Per Capita by County",
            main.title.size=1.5, main.title.position="centre",
            legend.title.size=1,legend.text.size=0.7, legend.position=c("right", "bottom"),
            frame=TRUE) +
  tm_compass(type = "rose", position = c("left", "bottom")) +
  tm_scale_bar(width = 0.5, position = c("left", "bottom")) +
  tmap_style("white")

# licensed beds per capita by state
tm_shape(beds.state) +
  tm_polygons("licens_percapita",
              breaks=c(0,0.0001,0.002,0.003,0.004,0.006),
              title="Licensed Beds Per Capita",
              palette="-BuGn") +
  tm_shape(covid.data.sf) +
  tm_borders(col="black", lwd=1.2) +
  tm_layout(main.title="Licensed Beds Per Capita by State",
            main.title.size=1.5, main.title.position="centre",
            legend.title.size=1,legend.text.size=0.7, legend.position=c("right", "bottom"),
            frame=TRUE) +
  tm_compass(type = "rose", position = c("left", "bottom")) +
  tm_scale_bar(width = 0.5, position = c("left", "bottom")) +
  tmap_style("white")

#### Maps of Licensed Beds per Total Covid Cases ####

# toggle between either
tmap_mode("view") 
tmap_mode("plot")

# licensed beds per total cases by county
tm_shape(beds.cty) +
  tm_polygons("licens_percases",
              #breaks=c(0,0.08,0.4,1.3,1.5,100),
              title="Licensed Beds/Cases",
              palette="-BuGn",
              border.col="green4") +
  tm_shape(covid.data.sf) +
  tm_borders(col="#003300", lwd=1.2) +
  tm_layout(main.title="Licensed Beds Per No. of Cases by County",
            main.title.size=1.5, main.title.position="centre",
            legend.title.size=1,legend.text.size=0.7, legend.position=c("right", "bottom"),
            frame=TRUE) +
  tm_compass(type = "rose", position = c("left", "bottom")) +
  tm_scale_bar(width = 0.5, position = c("left", "bottom")) +
  tmap_style("white")

# licensed beds per total cases by state
tm_shape(beds.state) +
  tm_polygons("licens_percases",
              breaks=c(0,0.0001,0.002,0.003,0.004,0.006),
              title="Licensed Beds/Cases",
              palette="-BuGn") +
  tm_shape(covid.data.sf) +
  tm_borders(col="#003300", lwd=1.2) +
  tm_layout(main.title="Licensed Beds Per No. of Cases by State",
            main.title.size=1.5, main.title.position="centre",
            legend.title.size=1,legend.text.size=0.7, legend.position=c("right", "bottom"),
            frame=TRUE) +
  tm_compass(type = "rose", position = c("left", "bottom")) +
  tm_scale_bar(width = 0.5, position = c("left", "bottom")) +
  tmap_style("white")

#### Maps of Licensed Beds per Total Covid Deaths ####

# toggle between either
tmap_mode("view") 
tmap_mode("plot")

# licensed beds per total deaths by county
tm_shape(beds.cty) +
  tm_polygons("licens_perdeaths",
              #breaks=c(0,4,39),
              title="Licensed Beds/Deaths",
              palette="-BuGn",
              border.col="green4") +
  tm_shape(covid.data.sf) +
  tm_borders(col="#003300", lwd=1.2) +
  tm_layout(main.title="Licensed Beds Per No. of Deaths by County",
            main.title.size=1.5, main.title.position="centre",
            legend.title.size=1,legend.text.size=0.7, legend.position=c("right", "bottom"),
            frame=TRUE) +
  tm_compass(type = "rose", position = c("left", "bottom")) +
  tm_scale_bar(width = 0.5, position = c("left", "bottom")) +
  tmap_style("white")

# licensed beds per total deaths by state
tm_shape(beds.state) +
  tm_polygons("licens_perdeaths",
              breaks=c(0,0.0001,0.002,0.003,0.004,0.006),
              title="Licensed Beds/Deaths",
              palette="-BuGn") +
  tm_shape(covid.data.sf) +
  tm_borders(col="#003300", lwd=1.2) +
  tm_layout(main.title="Licensed Beds Per No. of Deaths by State",
            main.title.size=1.5, main.title.position="centre",
            legend.title.size=1,legend.text.size=0.7, legend.position=c("right", "bottom"),
            frame=TRUE) +
  tm_compass(type = "rose", position = c("left", "bottom")) +
  tm_scale_bar(width = 0.5, position = c("left", "bottom")) +
  tmap_style("white")

#### Spatial Autocorrelation ####

## In order to quantify the above clustering of counties for number of licensed beds/number of cases and deaths, we run the Global
## Moran's I test to compute the statistic and check for its significance.

# toggle between either
tmap_mode("view") 
tmap_mode("plot")

# global moran's I for licensed beds/number of cases
nb <- poly2nb(beds.cty, queen=TRUE)
lw <- nb2listw(nb, style="W", zero.policy=TRUE)
mc <- moran.mc(beds.cty$licens_percases, lw, zero.policy=TRUE, nsim=599)
mc
plot(mc, las=1, main="Monte-Carlo Simulation of Global Moran's I for Licensed Bed/Cases")

wm <- nb2mat(nb, style="W", zero.policy=TRUE)
rwm <- mat2listw(wm, style="W")
mat <- listw2mat(rwm)
moran.plot(beds.cty$licens_percases, rwm, main="Spatial Lag against No. of Licensed Beds/Cases")

# local moran's I for licensed beds/number of cases
# aim: to understand each county's contribution to global Moran's I and draw a map of local indicators of spatial association (LISA)
local_mc <- localmoran(beds.cty$licens_percases, lw)
summary(local_mc)

beds.cty$s_licenspercase <- scale(beds.cty$licens_percases) %>% as.vector()
beds.cty$lags_s_licenspercase <- lag.listw(lw, beds.cty$s_licenspercase)
summary(beds.cty$s_licenspercase)
summary(beds.cty$lags_s_licenspercase)

x <- beds.cty$s_licenspercase
y <- beds.cty$lags_s_licenspercase
xx <- data.frame(x,y)
moran.plot(x, lw, main="No. of Licensed Beds/ Cases (x) against Spatially Lagged Values")

beds.cty <- 
  st_as_sf(beds.cty) %>%
  mutate(quad_sig = ifelse(beds.cty$s_licenspercase > 0 &
                             beds.cty$lags_s_licenspercase > 0 &
                             local_mc[ ,5] <= 0.05,
                    "high-high",
                    ifelse(beds.cty$s_licenspercase <= 0 &
                             beds.cty$lags_s_licenspercase <= 0 &
                             local_mc[ ,5] <= 0.05,
                    "low-low",
                    ifelse(beds.cty$s_licenspercase > 0 &
                             beds.cty$lags_s_licenspercase <= 0 &
                             local_mc[ ,5] <= 0.05,
                    "high-low",
                    ifelse(beds.cty$s_licenspercase <= 0 &
                             beds.cty$lags_s_licenspercase > 0 &
                             local_mc[ ,5] <= 0.05,
                    "low-high",
                    "non-significant")))))

table(beds.cty$quad_sig)
nrow(local_mc[local_mc[, 5] <= 0.05, ])

tm_shape(beds.cty) +
  tm_polygons("quad_sig", title="LISA") +
  tm_shape(covid.data.sf) +
  tm_borders(col="#003300", lwd=1.2) +
  tm_layout(main.title="LISA Map for No. of Licensed Beds/Cases",
            main.title.size=1.5, main.title.position="centre",
            legend.title.size=1,legend.text.size=0.7, legend.position=c("right", "bottom"),
            frame=TRUE) +
  tm_compass(type = "rose", position = c("left", "bottom")) +
  tm_scale_bar(width = 0.5, position = c("left", "bottom")) +
  tmap_style("white")

# global moran's I for licensed beds/number of deaths
beds.cty.noInf <- beds.cty %>% dplyr::filter(ndeaths!=0)
nb <- poly2nb(beds.cty.noInf, queen=TRUE)
lw <- nb2listw(nb, style="W", zero.policy=TRUE)
mc <- moran.mc(beds.cty.noInf$licens_perdeaths, lw, zero.policy=TRUE, nsim=599)
mc
plot(mc, las=1, main="Monte-Carlo Simulation of Global Moran's I for Licensed Bed/Deaths")

wm <- nb2mat(nb, style="W", zero.policy=TRUE)
rwm <- mat2listw(wm, style="W")
mat <- listw2mat(rwm)
moran.plot(beds.cty.noInf$licens_perdeaths, rwm, main="Spatial Lag against No. of Licensed Beds/Deaths")

# local moran's I for licensed beds/number of deaths
# aim: to understand each county's contribution to global Moran's I and draw a map of local indicators of spatial association (LISA)

## NOTE: Discontinued this approach because the above Moran plot showed a low spatial autocorrelation. Code left here for reference.

local_mc_deaths <- localmoran(beds.cty.noInf$licens_perdeaths, lw)
summary(local_mc_deaths)

beds.cty.noInf$s_licens_perdeaths <- scale(beds.cty.noInf$licens_perdeaths) %>% as.vector()
beds.cty.noInf$lags_s_licens_perdeaths <- lag.listw(lw, beds.cty.noInf$s_licens_perdeaths)
summary(beds.cty.noInf$s_licens_perdeaths)
summary(beds.cty.noInf$lags_s_licens_perdeaths)

x <- beds.cty.noInf$s_licens_perdeaths
y <- beds.cty.noInf$lags_s_licens_perdeaths
xx <- data.frame(x,y)
moran.plot(x, lw, main="No. of Licensed Beds/Deaths (x) against Spatially Lagged Values")

beds.cty.noInf <- 
  st_as_sf(beds.cty.noInf) %>%
  mutate(quad_sig = ifelse(beds.cty.noInf$s_licens_perdeaths > 0 &
                           beds.cty.noInf$lags_s_licens_perdeaths > 0 &
                             local_mc_deaths[ ,5] <= 0.05,
                          "high-high",
                    ifelse(beds.cty.noInf$s_licens_perdeaths <= 0 &
                           beds.cty.noInf$lags_s_licens_perdeaths <= 0 &
                             local_mc_deaths[ ,5] <= 0.05,
                           "low-low",
                    ifelse(beds.cty.noInf$s_licens_perdeaths > 0 &
                           beds.cty.noInf$lags_s_licens_perdeaths <= 0 &
                             local_mc_deaths[ ,5] <= 0.05,
                           "high-low",
                    ifelse(beds.cty.noInf$s_licens_perdeaths <= 0 &
                           beds.cty.noInf$lags_s_licens_perdeaths > 0 &
                             local_mc_deaths[ ,5] <= 0.05,
                           "low-high",
                    "non-significant")))))

table(beds.cty.noInf$quad_sig_deaths)
nrow(local_mc_deaths[local_mc_deaths[, 5] <= 0.05, ])

tm_shape(beds.cty) +
  tm_polygons("quad_sig_deaths", title="LISA") +
  tm_shape(covid.data.sf) +
  tm_borders(col="#003300", lwd=1.2) +
  tm_layout(main.title="LISA Map for No. of Licensed Beds/Deaths",
            main.title.size=1.5, main.title.position="centre",
            legend.title.size=1,legend.text.size=0.7, legend.position=c("right", "bottom"),
            frame=TRUE) +
  tm_compass(type = "rose", position = c("left", "bottom")) +
  tm_scale_bar(width = 0.5, position = c("left", "bottom")) +
  tmap_style("white")

#### Point Pattern Analysis: Quadrant ####

## I attempted quadrat analysis and KDE analyses of the number of hospital beds, with Covid case, death and death among detected
## rates overlaid as a graduated symbol map on it at first. However, the density-based and distance-based analyses did not allow me
## to see the exact locations of the hospitals and their beds. This section was eventually dropped.

# toggle between either
tmap_mode("view") 
tmap_mode("plot")

# density-based analysis: quadrant analysis of number of hospital beds and graduated symbol map of Covid rates
beds.hex <- hexbin_map(beds.sp, bins=100)

tm_shape(as_Spatial(states)) + 
  tm_borders() +
  tm_shape(beds.hex) + 
  tm_fill(col="z", title="No. of Hospital Beds", alpha=0.8) +
  tm_shape(st_as_sf(covid.data)) + 
  tm_bubbles("case_rate", border.lwd=NA, scale=0.8)

tm_shape(as_Spatial(states)) + 
  tm_borders() +
  tm_shape(beds.hex) + 
  tm_fill(col="z", title="No. of Hospital Beds", alpha=0.8) +
  tm_shape(st_as_sf(covid.data)) + 
  tm_bubbles("death_rate", border.lwd=NA, scale=0.8)

tm_shape(as_Spatial(states)) + 
  tm_borders() +
  tm_shape(beds.hex) + 
  tm_fill(col="z", title="No. of Hospital Beds", alpha=0.8) +
  tm_shape(st_as_sf(covid.data)) + 
  tm_bubbles("death_detected_rate", border.lwd=NA, scale=0.8)

#### Point Pattern Analysis: KDE ####

states.flat <- st_transform(states, crs=6345)
plot(states.flat)
states.owin <- as.owin(as_Spatial(states.flat))

county.flat <- st_transform(counties, crs=6345)
plot(county.flat)
county.owin <- as.owin(as_Spatial(county.flat))

beds.sf.utm <- st_transform(licensed, crs=6345) #licensed is a sf file with clean NUM_LICENS and geometry
beds.sf.utm2 <- beds.sf.utm[!st_is_empty(beds.sf.utm), ]
beds.sp.utm <- as(beds.sf.utm2, "Spatial")
beds.ppp <- as(beds.sp.utm, "ppp")

# select bandwidth for attempt 2 below
bandwidth <- bw.smoothppp(beds.ppp, kernel="gaussian")

marks(beds.ppp) <- NULL
Window(beds.ppp) <- county.owin
plot(beds.ppp, main=NULL, cols=rgb(0,0,0,0.2), pch=20)

beds.km <- rescale(beds.ppp, 1000, "km")
plot(beds.km)

# attempt 1
K1 <- density(beds.km)
plot(K1, main=NULL, las=1)
contour(K1, add=TRUE)

# attempt 2 (selected this for report)
K2 <- density.ppp(beds.km, sigma=bandwidth/1000)
plot(K2, main="KDE of Licensed Beds", las=1)
contour(K2, add=TRUE)

# attempt 3
plot(density.ppp(beds.km, kernel="quartic", sigma=bw.ppl(beds.km), edge=TRUE, main=paste("KDE of Licensed Beds (Quartic Kernel)")))

#### Point Pattern Analysis: G Function ####

g <- pcf(beds.km)
plot(g, main="", las=1, legendargs=list(cex=0.8, xpd=TRUE, inset=c(1.01,0)))

#### Hypothesis Testing ####

## I attempted this method, but eventually dropped it, because it didn't make sense to check if hospital bed locations followed
## CSR (which it would've unlikely been). Checking if it followed some underlying process was unnecessary as well; that would've
## been more relevant for COVID test sites. Note that it made more sense to check if hospital bed availability followed CSR, and
## I achieved this under the spatial autocorrelation section, using Moran's I tests.

# reference: https://stackoverflow.com/questions/59597078/use-sf-polygon-object-as-window-in-spatstat
states.flat <- st_transform(states, crs=6345)
plot(states.flat)
states.owin <- as.owin(as_Spatial(states.flat))

beds.sf.utm <- st_transform(beds.sf, crs=6345)
beds.sf.utm2 <- beds.sf.utm[!st_is_empty(beds.sf.utm), ]
beds.sp.utm <- as(beds.sf.utm2, "Spatial")
beds.ppp <- as(beds.sp.utm, "ppp")
beds.km <- rescale(beds.ppp, 1000, "km")

ann.p <- mean(nndist(beds.km, k=1))
ann.p

n <- 500L
ann.r <- vector(length=n)

for (i in 1:n){
  rand.p <- rpoint(n=beds.km$n, win=states.owin)
  ann.r[i] <- mean(nndist(rand.p, k=1)) 
}

plot(rand.p, pch=16, main=NULL, cols=rgb(0,0,0,0.5))

hist(ann.r, main=NULL, las=1, breaks=40, col="bisque", xlim=range(ann.p, ann.r))
abline(v=ann.p, col="blue")

N.greater <- sum(ann.r > ann.p)
p <- min(N.greater+1, n+1-N.greater) / (n+1)
p

rm(beds.sf.utm, beds.sf.utm2, ann.p, n)