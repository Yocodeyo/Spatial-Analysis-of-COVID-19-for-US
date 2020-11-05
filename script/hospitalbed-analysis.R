#### Imports ####

#initialise
curr_path <- rstudioapi::getActiveDocumentContext()$path
setwd(dirname(curr_path))
source("functions.R")

#data
beds.sf <- st_read("../data/Definitive_Healthcare_USA_Hospital_Beds/Definitive_Healthcare_Bed_Locations.shx") 
beds.sp <- readOGR("../data/Definitive_Healthcare_USA_Hospital_Beds/Definitive_Healthcare_Bed_Locations.shx")
cty.csv <- read_csv("../data/COVID-19_US_County_JHU_Data_&_Demographics/us_county.csv")
cty.sf <- st_read("../data/COVID-19_US_County_JHU_Data_&_Demographics/us_county.shp", stringsAsFactors = FALSE)
covid <- read_csv("../data/COVID-19_US_County_JHU_Data_&_Demographics/covid_us_county.csv")[, c(1,2,3,8,4,5,6,7,9)] 

#### Preprocessing ####

#states, counties and covid

##union based on county's fips
cty.sf$fips <- as.numeric(cty.sf$fips)
cty <- cty.sf %>% 
  group_by(fips) %>%
  summarize() %>%
  ungroup()
#plot(cty)

counties.full <- merge(cty, cty.csv, by="fips") %>%
  filter(!(state %in% c("Puerto Rico", "Hawaii", "Alaska"))) #noncontiguous states

##just counties, fips and geometry
counties <- counties.full %>%
  select(fips, county, geometry)

##union based on state_code
states <- counties.full %>%
  group_by(state, state_code) %>%
  summarize() %>%
  ungroup()
#plot(states)

##use july covid data, split by state
covid.tojul31 <- covid %>% filter(date == "2020-07-31")
covid.tojun30 <- covid %>% filter(date == as.Date("2020-06-30"))
covid.jul.state <- cbind(covid.tojul31[, 1:7], covid.tojul31[, c(8:9)] - covid.tojun30[, c(8:9)]) %>% 
  group_by(state) %>%
  summarise(ncases = sum(cases), ndeaths = sum(deaths)) %>%
  data.frame()

pop.state <- cty.csv %>%
  group_by(state) %>%
  summarise(npopulation = sum(population)) %>%
  data.frame()

covid.data <- merge(covid.jul.state, pop.state, by="state") %>%
  mutate(
    case_rate = ncases / npopulation,
    death_rate = ndeaths / npopulation,
    death_detected_rate = ndeaths / ncases) %>% 
  merge(states, by="state")

##use july covid data, split by county
covid.jul.cty <- cbind(covid.tojul31[, 1:7], covid.tojul31[, c(8:9)] - covid.tojun30[, c(8:9)]) %>% 
  drop_na(state_code, fips) %>%
  group_by(fips, county) %>%
  summarise(ncases = sum(cases), ndeaths = sum(deaths)) %>%
  data.frame()
covid.jul.cty %>% filter(ncases < 0) ###### what does negative ncase and ndeath mean lol? probably remove ######
covid.jul.cty %>% filter(ndeaths < 0)
covid.jul.cty <- covid.jul.cty %>%
  filter(ncases >= 0 & ndeaths >= 0)

pop.cty <- cty.csv %>%
  group_by(fips) %>%
  summarise(npopulation = sum(population)) %>%
  data.frame()

covid.data.cty <- merge(covid.jul.cty, pop.cty, by="fips") %>%
  mutate(
    case_rate = ncases / npopulation,
    death_rate = ndeaths / npopulation,
    death_detected_rate = ndeaths / ncases) %>% 
  drop_na(case_rate, death_rate, death_detected_rate) %>%
  merge(counties, by="fips") %>%
  select(-county.y) %>%
  rename(county = county.x)

rm(covid.tojul31, covid.tojun30, covid.jul.state, covid.jul.cty, pop.state, pop.cty)


#hospital locations and beds

##uninterested in hospitals not meant for general public or for Covid 
beds.demo <- beds.sf %>% select(HOSPITAL_N, HOSPITAL_T, NUM_LICENS, NUM_STAFFE, NUM_ICU_BE, ADULT_ICU_, PEDI_ICU_B, BED_UTILIZ, Potential_, AVG_VENTIL, FIPS, geometry)

hospitaltypes <- beds.sf$HOSPITAL_T %>% table() %>% data.frame() %>% rename(Hospital_Type = ".")
ggplot(hospitaltypes, aes(x=Hospital_Type, y=Freq)) + 
  geom_bar(stat="identity") + 
  labs(title="Frequency of Hospital Types in America", x="Hospital Type", y="Frequency") +
  coord_flip() +
  theme_minimal()

noncovidhospitals <- c("VA Hospital", "Rehabilitation Hospital", "Psychiatric Hospital", "Department of Defense Hospital")
beds.sf <- beds.sf %>% filter(!(HOSPITAL_T %in% noncovidhospitals))
newhospitaltypes <- beds.sf$HOSPITAL_T %>% table() %>% data.frame() %>% rename(Hospital_Type = ".")
sum(newhospitaltypes$Freq) / sum(hospitaltypes$Freq)

hospitaltypes$AcceptsCovidPatients <- ifelse(!(hospitaltypes$Hospital_Type %in% noncovidhospitals), "Yes", "No")
ggplot(hospitaltypes, aes(x=Hospital_Type, y=Freq, fill=AcceptsCovidPatients)) + 
  geom_bar(stat="identity") + 
  labs(title="Frequency of Hospital Types in America", x="Hospital Type", y="Frequency") +
  coord_flip() +
  theme_minimal()

beds.sf <- beds.sf %>% 
  filter(!(STATE_NAME %in% c("Puerto Rico", "Hawaii", "Alaska"))) #noncontiguous states


#### EDA with freshly cleaned data + Additional cleaning if necessary####

#covid data by state
##scatter
ggplot(covid.data, aes(x=case_rate, y=death_rate, color=death_detected_rate)) +
  geom_point() +
  scale_color_gradient(low="pink", high="red") +
  geom_text_repel(aes(label=state_code)) +
  labs(title="Death Rate vs Case Rate vs Death among Detected Rate per State",
       subtitle="July 2020",
       x="Case Rate", 
       y="Death Rate", 
       color="Death Detected Rate") +
  theme_minimal() +
  theme(legend.position=c(0.9, 0.2))

##hist: case rate
max(covid.data$case_rate)
ggplot(covid.data.sf, aes(x=case_rate)) + 
  geom_histogram(binwidth=0.001, boundary=0, color="black", fill="white") +
  scale_x_continuous(breaks=seq(0, 0.016, 0.001)) +
  labs(title="Histogram of Case Rates (State Aggregated)",
       subtitle="= #Cases/Population\nMax = 0.01543596",
       x="Case Rate",
       y="Count") +
  theme_minimal()

##hist: death rate
max(covid.data$death_rate)
ggplot(covid.data.sf, aes(x=death_rate)) + 
  geom_histogram(binwidth=0.00001, boundary=0, color="black", fill="white") +
  scale_x_continuous(breaks=seq(0, 0.0003, 0.00005)) +
  labs(title="Histogram of Death Rates (State Aggregated)",
       subtitle="= #Deaths/Population\nMax = 0.0002949608",
       x="Death Rate",
       y="Count") +
  theme_minimal()

##hist: death detected rate
max(covid.data$death_detected_rate)
ggplot(covid.data.sf, aes(x=death_detected_rate)) + 
  geom_histogram(binwidth=0.005, boundary=0, color="black", fill="white") +
  scale_x_continuous(breaks=seq(0, 0.08, 0.01)) +
  labs(title="Histogram of Death among Detected Rates (State Aggregated)",
       subtitle="= #Deaths/#Cases\nMax = 0.07845492",
       x="Death among Detected Rate",
       y="Count") +
  theme_minimal()

#covid data by county
##hist: case rate
summary(covid.data.cty$case_rate)
ggplot(covid.data.cty, aes(x=case_rate)) + 
  geom_histogram(binwidth=0.0005, boundary=0, color="black", fill="white") +
  scale_x_continuous(breaks=seq(0, 0.05, 0.0025)) +
  labs(title="Histogram of Case Rates (County Aggregated)",
       subtitle="= #Cases/Population\nMax = 0.04643002",
       x="Case Rate",
       y="Count") +
  theme_minimal()

##hist: death rate
summary(covid.data.cty$death_rate)
ggplot(covid.data.cty, aes(x=death_rate)) + 
  geom_histogram(binwidth=0.00001, boundary=0, color="black", fill="white") +
  scale_x_continuous(breaks=seq(0, 0.002, 0.0005)) +
  labs(title="Histogram of Death Rates (County Aggregated)",
       subtitle="= #Deaths/Population\nMax = 0.001680672",
       x="Death Rate",
       y="Count") +
  theme_minimal()

##hist: death detected rate
sum(is.na(covid.data.cty$death_detected_rate))
summary(covid.data.cty$death_detected_rate)
ggplot(covid.data.cty, aes(x=death_detected_rate)) +
  geom_boxplot()

ggplot(covid.data.cty, aes(x=death_detected_rate)) + 
  geom_histogram(binwidth=0.0005, boundary=0, color="black", fill="white") +
  scale_x_continuous(breaks=seq(0, 0.5, 0.001)) +
  labs(title="Histogram of Death among Detected Rates (County Aggregated)",
       subtitle="= #Deaths/#Cases\nMax = 0.5",
       x="Death among Detected Rate",
       y="Count") +
  theme_minimal()

#conversion from st to sf
covid.data.sf <- st_as_sf(covid.data)
covid.data.cty.sf <- st_as_sf(covid.data.cty)

#clean beds data
numNA <- colSums(is.na(beds.sf))
beds.sf.clean <- beds.sf %>% select(-HQ_ADDRE_1) %>% drop_na()


#### Hypothesis testing ####

#Reference: https://stackoverflow.com/questions/59597078/use-sf-polygon-object-as-window-in-spatstat
states.flat <- st_transform(states, crs = 6345)
plot(states.flat)
states.owin <- as.owin(as_Spatial(states.flat))

beds.sf.utm <- st_transform(beds.sf, crs = 6345)
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
p <- min(N.greater+1, n+1-N.greater)/(n+1)
p

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

#choropleth maps
tmap_mode("plot")

##borders
tm_shape(as_Spatial(counties)) + tm_borders(col = "grey40", lwd = 0.5) +
  tm_shape(as_Spatial(states)) + tm_borders(col = "black", lwd = 1) 

##case rate
tm_shape(covid.data.sf) + 
  tm_polygons("case_rate", breaks=seq(0,0.016,by=0.001), title="Case Rate") +
  tm_layout(main.title="Case Rate per American State",
            main.title.size=1.5, main.title.position="centre",
            legend.title.size=1,legend.text.size=0.7, legend.position=c("right", "bottom"),
            frame=TRUE) +
  tm_compass(type="rose", position = c("left", "bottom")) +
  tm_scale_bar(width=0.5, position = c("left", "bottom")) +
  tmap_style("beaver")

##death rate
tm_shape(covid.data.sf) + 
  tm_polygons("death_rate", breaks=seq(0,0.0003,by=0.00005), title="Death Rate") +
  tm_layout(main.title="Death Rate per American State",
            main.title.size=1.5, main.title.position="centre",
            legend.title.size=1,legend.text.size=0.7, legend.position=c("right", "bottom"),
            frame=TRUE) +
  tm_compass(type="rose", position = c("left", "bottom")) +
  tm_scale_bar(width=0.5, position = c("left", "bottom")) +
  tmap_style("beaver")
  
##death among cases rate
tm_shape(covid.data.sf) + 
  tm_polygons("death_detected_rate", breaks=seq(0,0.08,by=0.005), title="Death/Cases Rate") +
  tm_layout(main.title="Death Among Cases Rate per American State",
            main.title.size=1.5, main.title.position="centre",
            legend.title.size=1,legend.text.size=0.7, legend.position=c("right", "bottom"),
            frame=TRUE) +
  tm_compass(type="rose", position = c("left", "bottom")) +
  tm_scale_bar(width=0.5, position = c("left", "bottom")) +
  tmap_style("beaver")

#dot and choropleth maps

#### licensed beds + state ####
licensed <- beds.sf.clean %>% select(NUM_LICENS, geometry)
max(licensed$NUM_LICENS)
ggplot(licensed, aes(x=NUM_LICENS)) +
  geom_boxplot()

ggplot(licensed, aes(x=NUM_LICENS)) + 
  geom_histogram(binwidth=10, boundary=0, color="black", fill="white") +
  scale_x_continuous(breaks=seq(0, 700, 50)) +
  theme_classic()

##licensed beds + by state + cases rate
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

##licensed beds + by state + death rate
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

##licensed beds + by state + death/cases rate
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

#### licensed beds + county ####
tmap_mode("plot")

##licensed beds + by county + case rate
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

##licensed beds + by county + death rate
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

##licensed beds + by county + death detected rate
tm_shape(covid.data.cty.sf) + 
  tm_polygons("death_detected_rate", 
              breaks=c(0, 0.006029, 0.017544, 0.5), 
              title="Death among Cases Rate",
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

#### icu ####
icu <- beds.sf.clean %>% select(NUM_ICU_BE, geometry)
max(icu$NUM_ICU_BE)
ggplot(icu, aes(x=NUM_ICU_BE)) +
  geom_boxplot()

#### ventilators ####
vent <- beds.sf.clean %>% select(AVG_VENTIL, geometry)
max(vent$AVG_VENTIL)
ggplot(vent, aes(x=AVG_VENTIL)) +
  geom_boxplot()