curr_path <- rstudioapi::getActiveDocumentContext()$path
setwd(dirname(curr_path))

### DATA PREPARATION ###
library(sf)

#1. prepare states data
##1.1 read shp file
states_sf <- st_read('../data/cb_2018_us_state_20m/cb_2018_us_state_20m.shp')
head(states_sf)

##1.2 exclude non-contiguous states
states_sf <- states_sf[!(states_sf$NAME %in% c('Alaska', 'Hawaii', 'Puerto Rico')),]


#2. prepare race data
##2.1 read csv file
race_df <- read.csv('../data/us_race/us_race.csv')
head(race_df)
names(race_df)[names(race_df)=='ï..Location'] <- 'Location'

##2.2 replace NA values with 0
race_df[is.na(race_df)] <- 0

##2.3 exclude hispanic ethnicity
race_df$Total <- race_df$Total - race_df$Hispanic
race_df <- subset(race_df, select=-c(Hispanic))
head(race_df)


#3. prepare covid by race data
##3.1 read csv file
race_covid_df <- read.csv('../data/us_race/us_race_covid.csv')
head(race_covid_df)
sapply(race_covid_df, class)

##3.2 convert data classes
race_covid_df$Date <- as.character(race_covid_df$Date)
race_covid_df$Cases_Total <- as.integer(race_covid_df$Cases_Total)
race_covid_df$Cases_White <- as.integer(race_covid_df$Cases_White)
sapply(race_covid_df, class)

##3.3 replace NA values with NaN
race_covid_df[is.na(race_covid_df)] <- 0

##3.4 select data in july
library(data.table)
race_covid_df <- race_covid_df[race_covid_df$Date %like% '202007',]

##3.5 exclude latino, other and unknown races from total counts
race_covid_df$Cases_Total <- race_covid_df$Cases_Total - race_covid_df$Cases_LatinX - race_covid_df$Cases_Other - race_covid_df$Cases_Unknown
race_covid_df$Deaths_Total <- race_covid_df$Deaths_Total - race_covid_df$Deaths_LatinX - race_covid_df$Deaths_Other - race_covid_df$Deaths_Unknown

##3.6 drop latino, other, unknown races and ethnicity columns
race_covid_df <- subset(race_covid_df, select=-c(Cases_LatinX, Cases_Other, Cases_Unknown, Cases_Ethnicity_Hispanic, Cases_Ethnicity_NonHispanic, Cases_Ethnicity_Unknown, Deaths_LatinX, Deaths_Other, Deaths_Unknown, Deaths_Ethnicity_Hispanic, Deaths_Ethnicity_NonHispanic, Deaths_Ethnicity_Unknown))
head(race_covid_df)

##3.7 aggregate counts across all dates in july
race_covid_july <- cbind(aggregate(Cases_Total~State, data=race_covid_df, FUN=sum),
                         aggregate(Cases_White~State, data=race_covid_df, FUN=sum)[-1],
                         aggregate(Cases_Black~State, data=race_covid_df, FUN=sum)[-1],
                         aggregate(Cases_Asian~State, data=race_covid_df, FUN=sum)[-1],
                         aggregate(Cases_AIAN~State, data=race_covid_df, FUN=sum)[-1],
                         aggregate(Cases_NHPI~State, data=race_covid_df, FUN=sum)[-1],
                         aggregate(Cases_Multiracial~State, data=race_covid_df, FUN=sum)[-1],
                         aggregate(Deaths_Total~State, data=race_covid_df, FUN=sum)[-1],
                         aggregate(Deaths_White~State, data=race_covid_df, FUN=sum)[-1],
                         aggregate(Deaths_Black~State, data=race_covid_df, FUN=sum)[-1],
                         aggregate(Deaths_Asian~State, data=race_covid_df, FUN=sum)[-1],
                         aggregate(Deaths_AIAN~State, data=race_covid_df, FUN=sum)[-1],
                         aggregate(Deaths_NHPI~State, data=race_covid_df, FUN=sum)[-1],
                         aggregate(Deaths_Multiracial~State, data=race_covid_df, FUN=sum)[-1])
head(race_covid_july)

##3.8 average counts for no. of dates considered
for (i in 2:ncol(race_covid_july)) {
  race_covid_july[,i] <- floor(race_covid_july[,i] / length(unique(race_covid_df$Date)))
}
head(race_covid_july)

#3.9 merge race and covid race data
all <- merge(race_df, race_covid_july, by.x='Code', by.y='State')
head(all)


#4. create variables of interest
##4.1 race proportion
all$Pop_White <- all$White/all$Total
all$Pop_Black <- all$Black/all$Total
all$Pop_Asian <- all$Asian/all$Total
all$Pop_AIAN <- all$American.Indian.Alaska.Native/all$Total
all$Pop_NHPI <- all$Native.Hawaiian.Other.Pacific.Islander/all$Total
all$Pop_Multi <- all$Multiple.Races/all$Total

##4.2 case rate by race
all$Case_Rate_White <- all$Cases_White/all$White
all$Case_Rate_Black <- all$Cases_Black/all$Black
all$Case_Rate_Asian <- all$Cases_Asian/all$Asian
all$Case_Rate_AIAN <- all$Cases_AIAN/all$American.Indian.Alaska.Native
all$Case_Rate_NHPI <- all$Cases_NHPI/all$Native.Hawaiian.Other.Pacific.Islander
all$Case_Rate_Multi <- all$Cases_Multiracial/all$Multiple.Races

##4.3 death rate by race
all$Death_Rate_White <- all$Deaths_White/all$White
all$Death_Rate_Black <- all$Deaths_Black/all$Black
all$Death_Rate_Asian <- all$Deaths_Asian/all$Asian
all$Death_Rate_AIAN <- all$Deaths_AIAN/all$American.Indian.Alaska.Native
all$Death_Rate_NHPI <- all$Deaths_NHPI/all$Native.Hawaiian.Other.Pacific.Islander
all$Death_Rate_Multi <- all$Deaths_Multiracial/all$Multiple.Races

##4.4 death rate among detected cases by race
all$Death_Among_Detected_White <- all$Deaths_White/all$Cases_White
all$Death_Among_Detected_Black <- all$Deaths_Black/all$Cases_Black
all$Death_Among_Detected_Asian <- all$Deaths_Asian/all$Cases_Asian
all$Death_Among_Detected_AIAN <- all$Deaths_AIAN/all$Cases_AIAN
all$Death_Among_Detected_NHPI <- all$Deaths_NHPI/all$Cases_NHPI
all$Death_Among_Detected_Multi <- all$Deaths_Multi/all$Cases_Multi
head(all)


#5. merge states and race covid data
all <- merge(states_sf, all, by.x='STUSPS', by.y='Code')
head(all)

#=============================================================
### EDA ###
library(tmap)

#1. race proportion
tm_shape(all) + tm_polygons('Pop_White', title='Proportion') +
  tm_layout(main.title='Proportion of Whites in American States',
            main.title.size=1,
            main.title.fontface='bold',
            main.title.position=c('center', 'center'),
            legend.position=c(0.85, 0.02),
            legend.height=0.5) +
  tm_compass(type='rose', size=1.8, position=c(0.02, 0.15)) +
  tm_scale_bar(position=c(0.02, 0.02))

tm_shape(all) + tm_polygons('Pop_Black', title='Proportion') +
  tm_layout(main.title='Proportion of Blacks in American States',
            main.title.size=1,
            main.title.fontface='bold',
            main.title.position=c('center', 'center'),
            legend.position=c(0.85, 0.02),
            legend.height=0.5) +
  tm_compass(type='rose', size=1.8, position=c(0.02, 0.15)) +
  tm_scale_bar(position=c(0.02, 0.02))

tm_shape(all) + tm_polygons('Pop_Asian', title='Proportion') +
  tm_layout(main.title='Proportion of Asians in American States',
            main.title.size=1,
            main.title.fontface='bold',
            main.title.position=c('center', 'center'),
            legend.position=c(0.85, 0.02),
            legend.height=0.5) +
  tm_compass(type='rose', size=1.8, position=c(0.02, 0.15)) +
  tm_scale_bar(position=c(0.02, 0.02))

tm_shape(all) + tm_polygons('Pop_AIAN', title='Proportion') +
  tm_layout(main.title='Proportion of AIANs in American States',
            main.title.size=1,
            main.title.fontface='bold',
            main.title.position=c('center', 'center'),
            legend.position=c(0.85, 0.02),
            legend.height=0.5) +
  tm_compass(type='rose', size=1.8, position=c(0.02, 0.15)) +
  tm_scale_bar(position=c(0.02, 0.02))

tm_shape(all) + tm_polygons('Pop_NHPI', title='Proportion') +
  tm_layout(main.title='Proportion of NHPIs in American States',
            main.title.size=1,
            main.title.fontface='bold',
            main.title.position=c('center', 'center'),
            legend.position=c(0.84, 0.02),
            legend.height=0.5) +
  tm_compass(type='rose', size=1.8, position=c(0.02, 0.15)) +
  tm_scale_bar(position=c(0.02, 0.02))

tm_shape(all) + tm_polygons('Pop_Multi', title='Proportion') +
  tm_layout(main.title='Proportion of Multiracials in American States',
            main.title.size=1,
            main.title.fontface='bold',
            main.title.position=c('center', 'center'),
            legend.position=c(0.85, 0.02),
            legend.height=0.5) +
  tm_compass(type='rose', size=1.8, position=c(0.02, 0.15)) +
  tm_scale_bar(position=c(0.02, 0.02))


##2. case rate by race
tm_shape(all) + tm_polygons('Case_Rate_White', title='Case Rate') +
  tm_layout(main.title='COVID-19 Case Rate of Whites in American States',
            main.title.size=1,
            main.title.fontface='bold',
            main.title.position=c('center', 'center'),
            legend.position=c(0.84, 0.02),
            legend.height=0.5) +
  tm_compass(type='rose', size=1.8, position=c(0.02, 0.15)) +
  tm_scale_bar(position=c(0.02, 0.02))

tm_shape(all) + tm_polygons('Case_Rate_Black', title='Case Rate') +
  tm_layout(main.title='COVID-19 Case Rate of Blacks in American States',
            main.title.size=1,
            main.title.fontface='bold',
            main.title.position=c('center', 'center'),
            legend.position=c(0.85, 0.02),
            legend.height=0.5) +
  tm_compass(type='rose', size=1.8, position=c(0.02, 0.15)) +
  tm_scale_bar(position=c(0.02, 0.02))

tm_shape(all) + tm_polygons('Case_Rate_Asian', title='Case Rate') +
  tm_layout(main.title='COVID-19 Case Rate of Asians in American States',
            main.title.size=1,
            main.title.fontface='bold',
            main.title.position=c('center', 'center'),
            legend.position=c(0.85, 0.02),
            legend.height=0.5) +
  tm_compass(type='rose', size=1.8, position=c(0.02, 0.15)) +
  tm_scale_bar(position=c(0.02, 0.02))

tm_shape(all) + tm_polygons('Case_Rate_AIAN', title='Case Rate') +
  tm_layout(main.title='COVID-19 Case Rate of AIANs in American States',
            main.title.size=1,
            main.title.fontface='bold',
            main.title.position=c('center', 'center'),
            legend.position=c(0.85, 0.02),
            legend.height=0.5) +
  tm_compass(type='rose', size=1.8, position=c(0.02, 0.15)) +
  tm_scale_bar(position=c(0.02, 0.02))

tm_shape(all) + tm_polygons('Case_Rate_NHPI', title='Case Rate') +
  tm_layout(main.title='COVID-19 Case Rate of NHPIs in American States',
            main.title.size=1,
            main.title.fontface='bold',
            main.title.position=c('center', 'center'),
            legend.position=c(0.85, 0.02),
            legend.height=0.5) +
  tm_compass(type='rose', size=1.8, position=c(0.02, 0.15)) +
  tm_scale_bar(position=c(0.02, 0.02))

tm_shape(all) + tm_polygons('Case_Rate_Multi', title='Case Rate') +
  tm_layout(main.title='COVID-19 Case Rate of Multiracials in American States',
            main.title.size=1,
            main.title.fontface='bold',
            main.title.position=c('center', 'center'),
            legend.position=c(0.84, 0.02),
            legend.height=0.5) +
  tm_compass(type='rose', size=1.8, position=c(0.02, 0.15)) +
  tm_scale_bar(position=c(0.02, 0.02))

##3. death rate by race
tm_shape(all) + tm_polygons('Death_Rate_White', title='Death Rate') +
  tm_layout(main.title='COVID-19 Death Rate of Whites in American States',
            main.title.size=1,
            main.title.fontface='bold',
            main.title.position=c('center', 'center'),
            legend.position=c(0.82, 0.02),
            legend.height=0.5) +
  tm_compass(type='rose', size=1.8, position=c(0.02, 0.15)) +
  tm_scale_bar(position=c(0.02, 0.02))

tm_shape(all) + tm_polygons('Death_Rate_Black', title='Death Rate') +
  tm_layout(main.title='COVID-19 Death Rate of Blacks in American States',
            main.title.size=1,
            main.title.fontface='bold',
            main.title.position=c('center', 'center'),
            legend.position=c(0.83, 0.02),
            legend.height=0.45) +
  tm_compass(type='rose', size=1.8, position=c(0.02, 0.15)) +
  tm_scale_bar(position=c(0.02, 0.02))

tm_shape(all) + tm_polygons('Death_Rate_Asian', title='Death Rate') +
  tm_layout(main.title='COVID-19 Death Rate of Asians in American States',
            main.title.size=1,
            main.title.fontface='bold',
            main.title.position=c('center', 'center'),
            legend.position=c(0.84, 0.02),
            legend.height=0.5) +
  tm_compass(type='rose', size=1.8, position=c(0.02, 0.15)) +
  tm_scale_bar(position=c(0.02, 0.02))

tm_shape(all) + tm_polygons('Death_Rate_AIAN', title='Death Rate') +
  tm_layout(main.title='COVID-19 Death Rate of AIANs in American States',
            main.title.size=1,
            main.title.fontface='bold',
            main.title.position=c('center', 'center'),
            legend.position=c(0.86, 0.02),
            legend.height=0.5) +
  tm_compass(type='rose', size=1.8, position=c(0.02, 0.15)) +
  tm_scale_bar(position=c(0.02, 0.02))

tm_shape(all) + tm_polygons('Death_Rate_NHPI', title='Death Rate') +
  tm_layout(main.title='COVID-19 Death Rate of NHPIs in American States',
            main.title.size=1,
            main.title.fontface='bold',
            main.title.position=c('center', 'center'),
            legend.position=c(0.86, 0.02),
            legend.height=0.5) +
  tm_compass(type='rose', size=1.8, position=c(0.02, 0.15)) +
  tm_scale_bar(position=c(0.02, 0.02))

tm_shape(all) + tm_polygons('Death_Rate_Multi', title='Death Rate') +
  tm_layout(main.title='COVID-19 Death Rate of Multiracials in American States',
            main.title.size=1,
            main.title.fontface='bold',
            main.title.position=c('center', 'center'),
            legend.position=c(0.82, 0.02),
            legend.height=0.48) +
  tm_compass(type='rose', size=1.8, position=c(0.02, 0.15)) +
  tm_scale_bar(position=c(0.02, 0.02))

#4. death rate among detected cases by race
tm_shape(all) + tm_polygons('Death_Among_Detected_White', title='Death Rate Among Detected') +
  tm_layout(main.title='COVID-19 Death Rate Among Detected Cases of Whites in American States',
            main.title.size=1,
            main.title.fontface='bold',
            main.title.position=c('center', 'center'),
            legend.position=c(0.85, 0.02),
            legend.height=0.4,
            legend.title.size=0.77) +
  tm_compass(type='rose', size=1.8, position=c(0.02, 0.15)) +
  tm_scale_bar(position=c(0.02, 0.02))

tm_shape(all) + tm_polygons('Death_Among_Detected_Black', title='Death Rate Among Detected') +
  tm_layout(main.title='COVID-19 Death Rate Among Detected Cases of Blacks in American States',
            main.title.size=1,
            main.title.fontface='bold',
            main.title.position=c('center', 'center'),
            legend.position=c(0.85, 0.02),
            legend.height=0.5,
            legend.title.size=0.77) +
  tm_compass(type='rose', size=1.8, position=c(0.02, 0.15)) +
  tm_scale_bar(position=c(0.02, 0.02))

tm_shape(all) + tm_polygons('Death_Among_Detected_Asian', title='Death Rate Among Detected') +
  tm_layout(main.title='COVID-19 Death Rate Among Detected Cases of Asians in American States',
            main.title.size=1,
            main.title.fontface='bold',
            main.title.position=c('center', 'center'),
            legend.position=c(0.85, 0.02),
            legend.height=0.5,
            legend.title.size=0.77) +
  tm_compass(type='rose', size=1.8, position=c(0.02, 0.15)) +
  tm_scale_bar(position=c(0.02, 0.02))

tm_shape(all) + tm_polygons('Death_Among_Detected_AIAN', title='Death Rate Among Detected') +
  tm_layout(main.title='COVID-19 Death Rate Among Detected Cases of AIANs in American States',
            main.title.size=1,
            main.title.fontface='bold',
            main.title.position=c('center', 'center'),
            legend.position=c(0.85, 0.02),
            legend.height=0.4,
            legend.title.size=0.77) +
  tm_compass(type='rose', size=1.8, position=c(0.02, 0.15)) +
  tm_scale_bar(position=c(0.02, 0.02))

tm_shape(all) + tm_polygons('Death_Among_Detected_NHPI', title='Death Rate Among Detected') +
  tm_layout(main.title='COVID-19 Death Rate Among Detected Cases of NHPIs in American States',
            main.title.size=1,
            main.title.fontface='bold',
            main.title.position=c('center', 'center'),
            legend.position=c(0.85, 0.02),
            legend.height=0.5,
            legend.title.size=0.77) +
  tm_compass(type='rose', size=1.8, position=c(0.02, 0.15)) +
  tm_scale_bar(position=c(0.02, 0.02))

tm_shape(all) + tm_polygons('Death_Among_Detected_Multi', title='Death Rate Among Detected') +
  tm_layout(main.title='COVID-19 Death Rate Among Detected Cases of Multiracials in American States',
            main.title.size=1,
            main.title.fontface='bold',
            main.title.position=c('center', 'center'),
            legend.position=c(0.85, 0.02),
            legend.height=0.45,
            legend.title.size=0.77) +
  tm_compass(type='rose', size=1.8, position=c(0.02, 0.15)) +
  tm_scale_bar(position=c(0.02, 0.02))

#=============================================================
### SPATIAL AUTOCORRELATION ###
#1. race proportion vs case rate
cor.test(all$Pop_White, all$Case_Rate_White, method='pearson') #-0.3042249
cor.test(all$Pop_Black, all$Case_Rate_Black, method='pearson') #-0.1214378
cor.test(all$Pop_Asian, all$Case_Rate_Asian, method='pearson') #-0.1922449
cor.test(all$Pop_AIAN, all$Case_Rate_AIAN, method='pearson') #0.2334901
cor.test(all$Pop_NHPI, all$Case_Rate_NHPI, method='pearson') #NaN
cor.test(all$Pop_Multi, all$Case_Rate_Multi, method='pearson') #-0.01646279

#2. race proportion vs death rate
cor.test(all$Pop_White, all$Death_Rate_White, method='pearson') #-0.2224814
cor.test(all$Pop_Black, all$Death_Rate_Black, method='pearson') #0.3586851
cor.test(all$Pop_Asian, all$Death_Rate_Asian, method='pearson') #0.4655413
cor.test(all$Pop_AIAN, all$Death_Rate_AIAN, method='pearson') #0.1067148
cor.test(all$Pop_NHPI, all$Death_Rate_NHPI, method='pearson') #NaN
cor.test(all$Pop_Multi, all$Death_Rate_Multi, method='pearson') #0.007507577
