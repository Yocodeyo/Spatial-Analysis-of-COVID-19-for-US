#1. set working directory
setwd('C:/Users/Jing Ying/Documents/Y4S1/BT4015/Project/Codes')


#2. read states shp file
library(sf)
states_sf <- st_read('../Datasets/cb_2018_us_state_20m/cb_2018_us_state_20m.shp')
head(states_sf)


#3. prepare race data
##3.1 read csv file
race_df <- read.csv('../Datasets/us_race.csv')
head(race_df)
names(race_df)[names(race_df)=='ï..Location'] <- 'Location'

##3.2 replace NA values with 0
race_df[is.na(race_df)] <- 0

##3.3 exclude hispanic ethnicity
race_df$Total <- race_df$Total - race_df$Hispanic
race_df <- subset(race_df, select=-c(Hispanic))
head(race_df)


#4. prepare covid race data
##4.1 read csv file
race_covid_df <- read.csv('../Datasets/us_race_covid.csv')
head(race_covid_df)
sapply(race_covid_df, class)

##4.2 convert data classes
race_covid_df$Date <- as.character(race_covid_df$Date)
race_covid_df$Cases_Total <- as.integer(race_covid_df$Cases_Total)
race_covid_df$Cases_White <- as.integer(race_covid_df$Cases_White)
sapply(race_covid_df, class)

##4.3 replace NA values with NaN
race_covid_df[is.na(race_covid_df)] <- 0

##4.4 select data in july
library(data.table)
race_covid_df <- race_covid_df[race_covid_df$Date %like% '202007',]

##4.5 exclude latino, other and unknown races from total counts
race_covid_df$Cases_Total <- race_covid_df$Cases_Total - race_covid_df$Cases_LatinX - race_covid_df$Cases_Other - race_covid_df$Cases_Unknown
race_covid_df$Deaths_Total <- race_covid_df$Deaths_Total - race_covid_df$Deaths_LatinX - race_covid_df$Deaths_Other - race_covid_df$Deaths_Unknown

##4.6 drop latino, other, unknown races and ethnicity columns
race_covid_df <- subset(race_covid_df, select=-c(Cases_LatinX, Cases_Other, Cases_Unknown, Cases_Ethnicity_Hispanic, Cases_Ethnicity_NonHispanic, Cases_Ethnicity_Unknown, Deaths_LatinX, Deaths_Other, Deaths_Unknown, Deaths_Ethnicity_Hispanic, Deaths_Ethnicity_NonHispanic, Deaths_Ethnicity_Unknown))
head(race_covid_df)

##4.7 aggregate counts across all dates in july
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

##4.8 average counts for no. of dates considered
for (i in 2:ncol(race_covid_july)) {
  race_covid_july[,i] <- floor(race_covid_july[,i] / length(unique(race_covid_df$Date)))
}
head(race_covid_july)


#5. merge race and covid race data
race_covid_merged <- merge(race_df, race_covid_july, by.x='Code', by.y='State')
head(race_covid_merged)


#6. calculate metrics
##6.1 race population by state
race_covid_merged$Pop_White <- race_covid_merged$White / race_covid_merged$Total * 100
race_covid_merged$Pop_Black <- race_covid_merged$Black / race_covid_merged$Total * 100
race_covid_merged$Pop_Asian <- race_covid_merged$Asian / race_covid_merged$Total * 100
race_covid_merged$Pop_AIAN <- race_covid_merged$American.Indian.Alaska.Native / race_covid_merged$Total * 100
race_covid_merged$Pop_NHPI <- race_covid_merged$Native.Hawaiian.Other.Pacific.Islander / race_covid_merged$Total * 100
race_covid_merged$Pop_Multi <- race_covid_merged$Multiple.Races / race_covid_merged$Total * 100

##6.2 covid case rate by race
race_covid_merged$Case_Rate_White <- race_covid_merged$Cases_White / race_covid_merged$White * 100
race_covid_merged$Case_Rate_Black <- race_covid_merged$Cases_Black / race_covid_merged$Black * 100
race_covid_merged$Case_Rate_Asian <- race_covid_merged$Cases_Asian / race_covid_merged$Asian * 100
race_covid_merged$Case_Rate_AIAN <- race_covid_merged$Cases_AIAN / race_covid_merged$American.Indian.Alaska.Native * 100
race_covid_merged$Case_Rate_NHPI <- race_covid_merged$Cases_NHPI / race_covid_merged$Native.Hawaiian.Other.Pacific.Islander * 100
race_covid_merged$Case_Rate_Multi <- race_covid_merged$Cases_Multiracial / race_covid_merged$Multiple.Races * 100

##6.3 covid death rate by race
race_covid_merged$Death_Rate_White <- race_covid_merged$Deaths_White / race_covid_merged$White * 100
race_covid_merged$Death_Rate_Black <- race_covid_merged$Deaths_Black / race_covid_merged$Black * 100
race_covid_merged$Death_Rate_Asian <- race_covid_merged$Deaths_Asian / race_covid_merged$Asian * 100
race_covid_merged$Death_Rate_AIAN <- race_covid_merged$Deaths_AIAN / race_covid_merged$American.Indian.Alaska.Native * 100
race_covid_merged$Death_Rate_NHPI <- race_covid_merged$Deaths_NHPI / race_covid_merged$Native.Hawaiian.Other.Pacific.Islander * 100
race_covid_merged$Death_Rate_Multi <- race_covid_merged$Deaths_Multiracial / race_covid_merged$Multiple.Races * 100

##6.4 covid death rate among cases by race
race_covid_merged$Death_Case_Rate_White <- race_covid_merged$Deaths_White / race_covid_merged$Cases_White * 100
race_covid_merged$Death_Case_Rate_Black <- race_covid_merged$Deaths_Black / race_covid_merged$Cases_Black * 100
race_covid_merged$Death_Case_Rate_Asian <- race_covid_merged$Deaths_Asian / race_covid_merged$Cases_Asian * 100
race_covid_merged$Death_Case_Rate_AIAN <- race_covid_merged$Deaths_AIAN / race_covid_merged$Cases_AIAN * 100
race_covid_merged$Death_Case_Rate_NHPI <- race_covid_merged$Deaths_NHPI / race_covid_merged$Cases_NHPI * 100
race_covid_merged$Death_Case_Rate_Multi <- race_covid_merged$Deaths_Multi / race_covid_merged$Cases_Multi * 100

head(race_covid_merged)


#7. merge states and race covid data
states_race_covid <- merge(states_sf, race_covid_merged, by.x='STUSPS', by.y='Code')
head(states_race_covid)


#8. plot map
library(tmap)
tmap_mode('view')

##8.1 race population
tm_shape(states_race_covid) + tm_polygons('Pop_White')
tm_shape(states_race_covid) + tm_polygons('Pop_Black')
tm_shape(states_race_covid) + tm_polygons('Pop_Asian')
tm_shape(states_race_covid) + tm_polygons('Pop_AIAN')
tm_shape(states_race_covid) + tm_polygons('Pop_NHPI')
tm_shape(states_race_covid) + tm_polygons('Pop_Multi')

##8.2 covid case rate by race
tm_shape(states_race_covid) + tm_polygons('Case_Rate_White')
tm_shape(states_race_covid) + tm_polygons('Case_Rate_Black')
tm_shape(states_race_covid) + tm_polygons('Case_Rate_Asian')
tm_shape(states_race_covid) + tm_polygons('Case_Rate_AIAN')
tm_shape(states_race_covid) + tm_polygons('Case_Rate_NHPI')
tm_shape(states_race_covid) + tm_polygons('Case_Rate_Multi')

##8.3 covid death rate by race
tm_shape(states_race_covid) + tm_polygons('Death_Rate_White')
tm_shape(states_race_covid) + tm_polygons('Death_Rate_Black')
tm_shape(states_race_covid) + tm_polygons('Death_Rate_Asian')
tm_shape(states_race_covid) + tm_polygons('Death_Rate_AIAN')
tm_shape(states_race_covid) + tm_polygons('Death_Rate_NHPI')
tm_shape(states_race_covid) + tm_polygons('Death_Rate_Multi')

##8.4 covid death rate among cases by race
tm_shape(states_race_covid) + tm_polygons('Death_Case_Rate_White')
tm_shape(states_race_covid) + tm_polygons('Death_Case_Rate_Black')
tm_shape(states_race_covid) + tm_polygons('Death_Case_Rate_Asian')
tm_shape(states_race_covid) + tm_polygons('Death_Case_Rate_AIAN')
tm_shape(states_race_covid) + tm_polygons('Death_Case_Rate_NHPI')
tm_shape(states_race_covid) + tm_polygons('Death_Case_Rate_Multi')


#9. find correlation with race population
##9.1 covid case rate
cor.test(states_race_covid$White, states_race_covid$Case_Rate_White, method='pearson') #-0.08775968
cor.test(states_race_covid$Black, states_race_covid$Case_Rate_Black, method='pearson') #-0.2226937
cor.test(states_race_covid$Asian, states_race_covid$Case_Rate_Asian, method='pearson') #-0.1525789
cor.test(states_race_covid$American.Indian.Alaska.Native, states_race_covid$Case_Rate_AIAN, method='pearson') #0.1157069
cor.test(states_race_covid$Native.Hawaiian.Other.Pacific.Islander, states_race_covid$Case_Rate_NHPI, method='pearson') #NaN
cor.test(states_race_covid$Multiple.Races, states_race_covid$Case_Rate_Multi, method='pearson') #0.04204983

##9.2 covid death rate
cor.test(states_race_covid$White, states_race_covid$Death_Rate_White, method='pearson') #0.1585279
cor.test(states_race_covid$Black, states_race_covid$Death_Rate_Black, method='pearson') #0.2966328
cor.test(states_race_covid$Asian, states_race_covid$Death_Rate_Asian, method='pearson') #0.2306686
cor.test(states_race_covid$American.Indian.Alaska.Native, states_race_covid$Death_Rate_AIAN, method='pearson') #0.08039107
cor.test(states_race_covid$Native.Hawaiian.Other.Pacific.Islander, states_race_covid$Death_Rate_NHPI, method='pearson') #NaN
cor.test(states_race_covid$Multiple.Races, states_race_covid$Death_Rate_Multi, method='pearson') #0.07138413
