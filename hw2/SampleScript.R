# LeafletMapsExample.R
#Gates

##########################################################################
##This code works well and creates an interactive layered leaflet/R map
## The map is choropleth and markered
##
## Required datasets are here:
##    CancerCountyFIPS.csv
##    CancerCountyFIPS_Breast.csv
##    LandUseDatasetREALLatlong.csv
## AND ##
############
# Download county shape file.
## !! This is important. Shape files can be found here
#https://www.census.gov/geo/maps-data/data/cbf/cbf_counties.html
#us.map <- tigris::counties(cb = TRUE, year = 2015)
#OR
# Download county shape file from Tiger.
# https://www.census.gov/geo/maps-data/data/cbf/cbf_counties.html
# I downloaded the zip and placed all files in the zip into my RStudio folder
##us.map <- readOGR(dsn = ".", layer = "cb_2016_us_county_20m", stringsAsFactors = FALSE)
##head(us.map)
###############
##Not all of these libraries are required for this code, but
## they are good for more generalized goals
############################################################################

library(leaflet)
library(sp)
library(mapproj)
library(maps)
library(mapdata)
library(maptools)
library(htmlwidgets)
library(magrittr)
library(XML)
library(plyr)
library(rgdal)
library(WDI)
library(raster)
library(noncensus)
library(stringr)
library(tidyr)
library(tigris)
library(rgeos)
library(ggplot2)
library(scales)

data(zip_codes)
data(counties)

##############################
## REVIEW ALL OF THIS CODE
##############################

##
##Then you will add two layers to it
##
## See the Week 2 Assignment


################################################################
##https://www.statecancerprofiles.cancer.gov/incidencerates/index.php?stateFIPS=99&cancer=001&race=07&sex=
##0&age=001&type=incd&sortVariableName=rate&sortOrder=default#results
CancerRates <- read.csv('CancerCountyFIPS.csv')
#head(CancerRates)
CancerRatesB <- read.csv('CancerCountyFIPS_Breast.csv')
#head(CancerRatesB)
LandUse <- read.csv('LandUseDatasetREALLatlong.csv')
#head(LandUse)
## Not using this dataset yet...
#PowerPlants <- read.csv("PowerPlants.csv")
#head(PowerPlants)

## Make all the column names lowercase
names(CancerRates) <- tolower(names(CancerRates))
#head(CancerRates)

# Rename columns to make for a clean  df merge later.
##GEOID is the same as FIPS
colnames(CancerRates) <- c("location", "GEOID", "rate")
#head(CancerRates)
colnames(CancerRatesB) <- c("location", "GEOID", "rate")
#head(CancerRatesB)
colnames(LandUse) <- c("offset", "lat", "lng", "url", "name")
#head(LandUse)

##Add leading zeos to any FIPS code that's less than 5 digits long to get a good match.
##formatC is from C code formatting - creates a 5 digit int
CancerRates$GEOID <- formatC(CancerRates$GEOID, width = 5, format = "d", flag = "0")
#head(CancerRates)
CancerRatesB$GEOID <- formatC(CancerRatesB$GEOID, width = 5, format = "d", flag = "0")
head(CancerRatesB)

## Convert column called location to two columns: State and County
CancerRates <- separate(CancerRates, location, into = c("county", "state"), sep = ", ")
#head(CancerRates)
CancerRatesB <- separate(CancerRatesB, location, into = c("county", "state"), sep = ", ")
#head(CancerRatesB)

##Remove the (...) from the state values
CancerRates[] <- lapply(CancerRates, function(x) gsub("\\s*\\([^\\)]+\\)", "", x))
head(CancerRates)
CancerRatesB[] <- lapply(CancerRatesB, function(x) gsub("\\s*\\([^\\)]+\\)", "", x))
head(CancerRatesB)

##Remove the space# from the end of some of the values in the rate column
CancerRatesB[] <- lapply(CancerRatesB, function(x) gsub("\\#", "", x))
#CancerRatesB

# Convert full state names to abbreviations for a clean df merge later.
CancerRates$state <- state.abb[match(CancerRates$state,state.name)]
#head(CancerRates)
CancerRatesB$state <- state.abb[match(CancerRatesB$state,state.name)]
#head(CancerRatesB)

#Change CancerRates$rate to a number
CancerRates$rate <- as.numeric(as.character(CancerRates$rate))
#head(CancerRates)
CancerRatesB$rate <- as.numeric(as.character(CancerRatesB$rate))
#head(CancerRatesB)


# Download county shape file.
## !! This is important. Shape files can be found here
#https://www.census.gov/geo/maps-data/data/cbf/cbf_counties.html
#us.map <- tigris::counties(cb = TRUE, year = 2015)
#OR
# Download county shape file from Tiger.
# https://www.census.gov/geo/maps-data/data/cbf/cbf_counties.html

# I downloaded the zip and placed all files in the zip into my RStudio folder
us.map <- readOGR(dsn = "cb_2016_us_county_20m", layer = "cb_2016_us_county_20m", stringsAsFactors = FALSE)
head(us.map)
# Remove Alaska(2), Hawaii(15), Puerto Rico (72), Guam (66), Virgin Islands (78), American Samoa (60)
#  Mariana Islands (69), Micronesia (64), Marshall Islands (68), Palau (70), Minor Islands (74)
us.map <- us.map[!us.map$STATEFP %in% c("02", "15", "72", "66", "78", "60", "69",
                                        "64", "68", "70", "74"),]
#head(us.map)

# Make sure other outling islands are removed.
us.map <- us.map[!us.map$STATEFP %in% c("81", "84", "86", "87", "89", "71", "76",
                                        "95", "79"),]

# Merge spatial df with downloaded data.
## This is important
## Now we have our data and the needed carto data
cancermap <- merge(us.map, CancerRates, by=c("GEOID"))
cancermapB <- merge(us.map, CancerRatesB, by=c("GEOID"))

# Format popup data for leaflet map.
popup_dat <- paste0("<strong>County: </strong>", 
                    cancermap$county, 
                    "<br><strong>Cancer Rate (Age Adjusted) Out of 100,000: </strong>", 
                    cancermap$rate)

#--------------------------------------------------------------------#
#--------------------------------------------------------------------#
#----------------------------Shaoyu Feng-----------------------------#
#----------------------------sf865-----------------------------------#
#This data source contains data representing seismic events in different countries over a seven-day period in 2012.
#Earthquake data is collected in real-time by geological institutions worldwide, such as the United States Geological Survey (USGS). 
#You can find further details about the data types on the USGS website.
#The date source if from: http://js.cit.datalens.api.here.com/datasets/starter_pack/Earthquakes_7day.csv

# Src	String	Two-letter network code representing the data contributor which recorded the earthquake
# Eqid	String	Unique earthquake ID
# Version	String	Version of the record
# Datetime	Date	Date and time of the earthquake
# Lat	Number	Latitude of the earthquake location
# Lon	Number	Longitude of the earthquake location
# Magnitude	Number	Magnitude of the earthquake
# Depth	Number	Depth of the earthquake
# NST	Number	Total number of earthquake-reporting stations used to determine the location of the earthquake
# Region	String	Region where the earthquake occurred


EarthQuake <- read.csv('Earthquakes_7day.csv')
### Format the datetime to yyyy-mm-dd for ease of read
## original datetime example is Wednesday, September 26, 2012 15:07:51 UTC
EarthQuake$Datetime<- as.Date(as.character(EarthQuake$Datetime), format = "%A, %B %d, %Y %H:%M:%S")
#To take necessary columns for the plot 
EarthQuake <- EarthQuake[c('Lat','Lon','Magnitude','Depth','Region','Datetime')]
popup_EA <- paste0("<strong>Region: </strong>", 
                   EarthQuake$Region, 
                   "<br><strong>Date: </strong>", 
                   EarthQuake$Datetime, 
                   "<br><strong>Magnitude: </strong>", 
                   EarthQuake$Magnitude, 
                   "<br><strong>Depth: </strong>", 
                   EarthQuake$Depth
                   )

## to define the colors of icon based on the magnitude of earthquake
getColor <- function(EarthQuake) {
  sapply(EarthQuake$Magnitude, function(mag) {
    if(mag <= 4) {
      "green"
    } else if(mag <= 5) {
      "orange"
    } else {
      "red"
    } })
}

## To define a icon to be added for earthquake map
icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = getColor(EarthQuake)
)
#--------------------------------------------------------------------#
#--------------------------------------------------------------------#

#--------------------------------------------------------------------#
#--------------------------------------------------------------------#
#----------------------------Shaoyu Feng-----------------------------#
#----------------------------sf865-----------------------------------#

#This data source contains data representing country populations 
#as measured by the World Bank in 2013, 
#represented by the country's name, ISO code, 
#geographical co-ordinates (latitude, longitude) and population.

# recID	Number	Record ID of the entry
# CountryName	String	Name of country
# CountryCode	String	ISO 3166-1 alpha-3 country code
# pop_2013	Number	Population in 2013
# lat	Number	Latitude of country location
# lon	Number	Longitude of country location

CountryPop<-read.csv('Global_country_populations_2013.csv')

popup_Pop <- paste0("<strong>Country Name: </strong>", 
                  CountryPop$CountryName, 
                   "<br><strong>Country Code: </strong>", 
                   CountryPop$CountryCode, 
                   "<br><strong>Population: </strong>", 
                   CountryPop$pop_2013
)

PoppulationIcon <- makeIcon(
  iconUrl = "icon.png",
  iconWidth = 13, iconHeight = 31,
  iconAnchorX = 7, iconAnchorY = 30
)

#--------------------------------------------------------------------#
#--------------------------------------------------------------------#

#--------------------------------------------------------------------#
#--------------------------------------------------------------------#
#----------------------------Shaoyu Feng-----------------------------#
#----------------------------sf865-----------------------------------#
StarBucksLoc<-read.csv('USStarBucksLoc.csv')

#to extract the state from the store infomatin
state<-  lapply(StarBucksLoc$BriefInfo, function(x) {   strsplit(as.character(x),"-")[1][[1]][2] 
})
StarBucksLoc$State <- unlist(state)

#to extract the store number from the store information
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

StoreNo <-lapply(StarBucksLoc$BriefInfo, function(x) {   temp<-strsplit(as.character(x),"-")[1][[1]][3] 
substrRight(temp,5)
})





#--------------------------------------------------------------------#
#--------------------------------------------------------------------#





#Color Pallette
#pal <- colorQuantile("YlOrRd", NULL, n = 9)

# Render final map in leaflet.The better map is below so this is
## commented out
# leaflet(data = cancermap) %>% addTiles() %>%
#   addPolygons(fillColor = ~pal(rate), 
#               fillOpacity = 0.8, 
#               color = "#BDBDC3", 
#               weight = 1,
#               popup = popup_dat) %>% 
#   addMarkers(lat=39.8, lng=-105.2, popup="Rocky Flats SuperFund Site")






#Grouping for map options and User Choices
#https://rstudio.github.io/leaflet/showhide.html

##Make pop up for the land use sites
# Format popup data for leaflet map.
popup_LU <- paste0("<strong>Use Name: </strong>", 
                   LandUse$name, 
                   "<br><strong>Link: </strong>", 
                   LandUse$url)

pal <- colorQuantile("YlOrRd", NULL, n = 9)

gmap <- leaflet(data = cancermap) %>%
  # Base groups
  addTiles() %>%
  setView(lng = -105, lat = 40, zoom = 4) %>% 
  addPolygons(fillColor = ~pal(rate), 
              fillOpacity = 0.8, 
              color = "#BDBDC3", 
              weight = 1,
              popup = popup_dat,
              group="Cancer Rate/100,000 by Counties") %>% 
  
  # Overlay groups
  addMarkers(data=LandUse,lat=~lat, lng=~lng, popup=popup_LU, group = "Land Use Sites") %>% 
  #addCircles(~long, ~lat, ~10^mag/5, stroke = F, group = "Quakes") %>%
  #addPolygons(data = outline, lng = ~long, lat = ~lat,
  #           fill = F, weight = 2, color = "#FFFFCC", group = "Outline") %>%
  # Layers control
  

 #----------------------------Shaoyu Feng-----------------------------#
 #----------------------------sf865-----------------------------------#

  
  addMarkers(data=CountryPop,lat=~lat, lng=~lon, popup=popup_Pop, icon=PoppulationIcon, group = "Country Population") %>% 
  # To Add the Earthquake Icon into the gmap
  # define icon 
  # enable the option to cluter markers when there are too many markers near by
  addAwesomeMarkers(data=EarthQuake,lat=~Lat, lng=~Lon, popup=popup_EA, icon=icons, group = "Earthquake Location", clusterOptions = markerClusterOptions()) %>% 
 #--------------------------------------------------------------------#
 #--------------------------------------------------------------------#
  
  addLayersControl(
    baseGroups = c("Cancer Rate/100,000 by Counties"),
    overlayGroups = c("Land Use Sites","Earthquake Location","Country Population"),
    options = layersControlOptions(collapsed = TRUE)
  )
gmap
#saveWidget(gmap, 'US_county_cancer_poll_map.html', selfcontained = TRUE)