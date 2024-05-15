options(java.parameters = "-Xmx8g")

library(r5r)
library(sf)
sf::sf_use_s2(FALSE) #Incompatible with projections
library(data.table)
library(tidycensus)
#census_api_key("5290b3ffc1891002ed20b36df6668db209b09f47",install=TRUE, overwrite=TRUE)
library(tigris)
options(tigris_use_cache = TRUE)
var_acs19 <- load_variables(2019, "acs5", cache = TRUE) #Use this to lookup variables

library(ggplot2)
library(mapview)
mapviewOptions(platform = 'leafgl')
library(tictoc)
library(corrplot)

GET_PEDS <- FALSE #Flag for peds vs adult maps
EXCL_TRACTS <- c("53033027701","53033027702") #Excluding Vashon because it is out of the catchment areas from Seattle and heavily skews results


data_path <- "./"
options(timeout=120)
r5r_core <- setup_r5(data_path = data_path,temp_dir = FALSE)

#Vaccine Coverage Data from KCPH
vac_coverage <- read.csv("./VaccineRates.csv",fileEncoding="UTF-8-BOM") #Due to Mac encoding
vac_coverage$ZIP_CODE <- as.character(vac_coverage$ZIP_CODE)

WA_KING_zctas <- get_acs(geography = "zcta",
                         table = "B01003",
                         geometry = TRUE,
                         cb = FALSE) %>% 
  dplyr::arrange(GEOID) %>%
  filter (GEOID %in% vac_coverage$ZIP_CODE)


KING_OUTLINE <- get_acs(geography = "county",state="WA",county="KING",table="B01003",geometry=TRUE,cb=FALSE)
zctaShapes <- get_acs(geography = "zcta",geometry=TRUE,table="B01003")
zctaShapes$inKingCty <- !is.na(as.numeric(st_intersects(zctaShapes,KING_OUTLINE))) 
WA_KING_tracts <- get_acs(geography = "tract",
                          state = "WA",
                          county = "KING",
                          table = "B01003",
                          geometry = TRUE,
                          cb = FALSE) %>% dplyr::arrange(GEOID)
WA_KING_tracts$popTotal <- WA_KING_tracts$estimate


#Table B08201: Vehicle ownership by household
#Var B08201_001: Total number of households
#Var B08201_002: Households with zero vehicles
WA_KING_vehicleOwn <- get_acs(geography = "tract",
                              state = "WA",
                              county = "KING",
                              table = "B08201",
                              geometry = FALSE,
                              output = "wide") %>% dplyr::arrange(GEOID)
WA_KING_tracts$percNoVehicles <- WA_KING_vehicleOwn$B08201_002E/WA_KING_vehicleOwn$B08201_001E * 100

#Table B10010: Median Income
#Var B10010_001
WA_KING_hhIncome <- get_acs(geography = "tract",
                              state = "WA",
                              county = "KING",
                              table = "B10010",
                              geometry = FALSE,
                              output = "wide") %>% dplyr::arrange(GEOID)

WA_KING_tracts$hhMedInc <- WA_KING_hhIncome$B10010_001E #Note about 80% of the values are NA...

#Table B17010 : Income Below Poverty Line 
#Var B17010_001: Total households
#Var B17010_002: Households below FPL
WA_KING_hhPov <- get_acs(geography = "tract",
                            state = "WA",
                            county = "KING",
                            table = "B17010",
                            geometry = FALSE,
                            output = "wide") %>% dplyr::arrange(GEOID)

WA_KING_tracts$hhPercPov <- WA_KING_hhPov$B17010_002E/WA_KING_hhPov$B17010_001E * 100

#Table B03002: Race
#Var B03002_001: Total population
#Var B03002_003: NonHispWhite
#Var B03002_004: NonHispBlack
#Var B03002_005: NonHispAIAN
#Var B03002_006: NonHispAsian
#Var B03002_007: NonHispNHPI
#Var B03002_008: NonHispOther
#Var B03002_009: NonHispMulti
#Var B03002_012: Hispanic/Latino

WA_KING_race <- get_acs(geography = "tract",
                            state = "WA",
                            county = "KING",
                            table = "B03002",
                            geometry = FALSE,
                            output = "wide") %>% dplyr::arrange(GEOID)

WA_KING_tracts$racePercBlack <- WA_KING_race$B03002_004E/WA_KING_race$B03002_001E * 100
WA_KING_tracts$racePercWhite <- WA_KING_race$B03002_003E/WA_KING_race$B03002_001E * 100
WA_KING_tracts$racePercAIAN <- WA_KING_race$B03002_005E/WA_KING_race$B03002_001E * 100
WA_KING_tracts$racePercAsian <- WA_KING_race$B03002_006E/WA_KING_race$B03002_001E * 100
WA_KING_tracts$racePercNHPI <- WA_KING_race$B03002_007E/WA_KING_race$B03002_001E * 100
WA_KING_tracts$racePercHisp <- WA_KING_race$B03002_012E/WA_KING_race$B03002_001E * 100
WA_KING_tracts$racePercOther <- WA_KING_race$B03002_008E/WA_KING_race$B03002_001E * 100
WA_KING_tracts$racePercMulti <- WA_KING_race$B03002_009E/WA_KING_race$B03002_001E * 100

#Table B01001: Age and sex
#Var B01001_002: Total male
#Var B01001_003: Male, less than 5
#Var B01001_004-006: Male, 5-17 year old
#Var B01001_026: Total female
#Var B01001_027: Female, less than 5
#Var B01001_028-030: Female, 5-17 year old

WA_KING_age <- get_acs(geography = "tract",
                        state = "WA",
                        county = "KING",
                        table = "B01001",
                        geometry = FALSE,
                        output = "wide") %>% dplyr::arrange(GEOID)

WA_KING_tracts$totalMale <- WA_KING_age$B01001_002E
WA_KING_tracts$popLessThan5Male <- WA_KING_age$B01001_003E
WA_KING_tracts$popPedsMale <- WA_KING_age$B01001_004E + WA_KING_age$B01001_005E + WA_KING_age$B01001_006E
WA_KING_tracts$popAdultMale <- WA_KING_tracts$totalMale - WA_KING_tracts$popLessThan5Male - WA_KING_tracts$popPedsMale

WA_KING_tracts$totalFemale <- WA_KING_age$B01001_026E
WA_KING_tracts$popLessThan5Female <- WA_KING_age$B01001_003E
WA_KING_tracts$popPedsFemale <- WA_KING_age$B01001_028E + WA_KING_age$B01001_029E + WA_KING_age$B01001_030E
WA_KING_tracts$popAdultFemale <- WA_KING_tracts$totalFemale - WA_KING_tracts$popLessThan5Female - WA_KING_tracts$popPedsFemale

WA_KING_tracts$popPeds <- WA_KING_tracts$popPedsMale + WA_KING_tracts$popPedsFemale
WA_KING_tracts$popAdult <- WA_KING_tracts$popAdultMale + WA_KING_tracts$popAdultFemale

st_erase <- function(x, y) {
  st_difference(x, st_union(y))
}

KING_WATER <- area_water("WA", "KING", class = "sf") 

tic("Spatial Water Removal")
  WA_KING_tracts <- st_erase(WA_KING_tracts, KING_WATER)
toc()
#Strips away the list structuring from st_centroid to make a numerical dataframe
cent<-do.call(rbind,
  do.call(rbind,as.data.frame(st_centroid(WA_KING_tracts$geometry))
          )
  )
#Calculate population density with water removed
WA_KING_tracts$area <- 3.86102e-7 * st_area(WA_KING_tracts) #Convert sq meters to sq mi
WA_KING_tracts$popDens <- WA_KING_tracts$popTotal/as.vector(WA_KING_tracts$area) #As vector cleans units

WA_KING_tracts$lon <- cent[,1]
WA_KING_tracts$lat <- cent[,2]

WA_cities <- places(state="WA")
seattleBoundary<-WA_cities$geometry[WA_cities$NAME=="Seattle"]
WA_KING_tracts$inSeattle <- as.numeric(st_intersects(WA_KING_tracts,seattleBoundary))

#Remove the tracts designated for exclusion 
WA_KING_tracts <- dplyr::filter(WA_KING_tracts,!(GEOID %in% EXCL_TRACTS))

#Fully restructure for r5r to accept
tractCoord <- data.frame(
  id=WA_KING_tracts$GEOID,
  lon=WA_KING_tracts$lon,
  lat=WA_KING_tracts$lat)

#Pick which filename to use based on if we are getting peds vs adult
vaxFilename <- ifelse(GET_PEDS,
       "./VaccineLocationsPeds.csv",
       "./VaccineLocationsAdult.csv")
WA_KING_tracts$popCatchement <- ifelse(GET_PEDS,
                                       WA_KING_tracts$popPeds,
                                       WA_KING_tracts$popAdult)

vaxCoord <- fread(input=vaxFilename)
vaxCoord$id <- vaxCoord$id <- 1:length(vaxCoord$Addresses)

#Switch to points, only keep sites if in King County
vaxCoord_geo <- st_as_sf(vaxCoord,coords = c("lon","lat"),crs = st_crs(WA_KING_tracts))
vaxCoord$inKingCty <- !is.na(as.numeric(st_intersects(vaxCoord_geo,
                                                      st_make_valid(WA_KING_tracts)))) 
vaxCoord <- dplyr::filter(vaxCoord,inKingCty == TRUE)

  
modeCar <- c("WALK", "CAR")
modeTransit <- c("WALK","TRANSIT")
max_walk_dist <- 800
max_trip_duration_transit <- 120
max_trip_duration_car <- 60
departure_datetime <- as.POSIXct("31-10-2021 14:00:00",
                                 format = "%d-%m-%Y %H:%M:%S")
attr(departure_datetime,"tzone") <- "America/Los_Angeles"

tic("OD Matrix Calculation - Transit")
ttm_Transit <- travel_time_matrix(r5r_core = r5r_core,
                                  origins = tractCoord,
                                  destinations = vaxCoord,
                                  departure_datetime = departure_datetime,
                                  mode = modeTransit,
                                  max_walk_dist = max_walk_dist,
                                  max_trip_duration = max_trip_duration_transit,
                                  verbose = FALSE)
toc()

tic("OD Matrix Calculation - Car")
ttm_Car <- travel_time_matrix(r5r_core = r5r_core,
                          origins = tractCoord,
                          destinations = vaxCoord,
                          mode = modeCar,
                          max_walk_dist = max_walk_dist,
                          max_trip_duration = max_trip_duration_car,
                          verbose = FALSE)
toc()

stop_r5(r5r_core)
rJava::.jgc(R.gc = TRUE) #Clear up RAM back to R


#Empiric Weighting Function, see supplement for derivation
TIME_HALF <- 30
TIME_SCALE <- 4
weight_fcn_car <- function(time){
  ifelse(time <= 15, 1,
  ifelse(time <= 30, 0.68,
  ifelse(time <= 45, 0.20,
  ifelse(time <= 60, 0.50,0))))
}
weight_fcn_transit <- function(time){
  ifelse(time <= 45, 1,
  ifelse(time <= 75, 0.54,
  ifelse(time <= 90, 0.24,
  ifelse(time <= 120, 0.10,0))))
}


WA_KING_tracts$accScore_Car <- 0
WA_KING_tracts$accScore_Transit <- 0



tic("Accessibility Score Calculation")
vaxCoord$catchPop <- 0 #Weighted population for each provider ratio
for(i in 1:length(vaxCoord$catchPop)){
  oriProv <- vaxCoord$id[i]
  catchmentTracts <- ttm_Car[which(ttm_Car$toId == oriProv)]
  catchmentTracts$weights <- weight_fcn_car(catchmentTracts$travel_time)
  catchmentTracts$pop <- 0
  for(j in 1:length(catchmentTracts$pop)){
    catchmentTracts$pop <- WA_KING_tracts$popCatchement[which(WA_KING_tracts$GEOID == catchmentTracts$fromId[j])]
  }
  vaxCoord$catchPop[i] <- sum(catchmentTracts$pop * catchmentTracts$weight)
}
for(i in 1:length(WA_KING_tracts$GEOID)){
  oriTract <- WA_KING_tracts$GEOID[i]
  destProvCar <- ttm_Car[which(ttm_Car$fromId == oriTract)]
  destProvCar$weight <- weight_fcn_car(destProvCar$travel_time)
  destProvCar$pop <- 0 
  for(j in 1:length(destProvCar$pop)){
    destProvCar$pop[j] <- vaxCoord$catchPop[which(vaxCoord$id == destProvCar$toId[j])] 
  }
  WA_KING_tracts$accScore_Car[i] <- sum(destProvCar$weight/destProvCar$pop) * 1E5

  destProvTransit <- ttm_Transit[which(ttm_Transit$fromId == oriTract)]
  destProvTransit$weight <- weight_fcn_transit(destProvTransit$travel_time)
  destProvTransit$pop <- 0 
  for(j in 1:length(destProvTransit$pop)){
    destProvTransit$pop[j] <- vaxCoord$catchPop[which(vaxCoord$id == destProvTransit$toId[j])] 
  }
  WA_KING_tracts$accScore_Transit[i] <- sum(destProvTransit$weight/destProvTransit$pop) * 1E5
  
  
  #Combine the two scores based on households with no cars will have to use public transit
  WA_KING_tracts$accScore_Combined[i] <-
    WA_KING_tracts$percNoVehicles[i]/100 * WA_KING_tracts$accScore_Transit[i] +
    (1-WA_KING_tracts$percNoVehicles[i]/100) * WA_KING_tracts$accScore_Car[i]
  
  #weightsTransit <- weight_fcn_transit(ttm_Transit$travel_time[which(ttm_Transit$fromId == oriTract)])
  
  #WA_KING_tracts$accScore_Car[i] <- sum(weightsCar)
  #WA_KING_tracts$accScore_Transit[i] <- sum(weightsTransit)
}
toc()

#Sanity plot
#street_net <- street_network_to_sf(r5r_core)
#ggplot() +
#  geom_sf(data = street_net$edges, color='gray85') +

WA_KING_tracts$inSeattle[is.na(WA_KING_tracts$inSeattle)] <- 0 #Switch NA to 0
seattleTracts <- WA_KING_tracts[WA_KING_tracts$inSeattle == 1,]

mapview(WA_KING_tracts,
        zcol="accScore_Car",
        alpha.regions=0.7,
        layer.name = 'Vaccine Access Score - Car')

mapview(WA_KING_tracts,
        zcol="accScore_Transit",
        alpha.regions=0.7,
        layer.name = 'Vaccine Access Score - Transit')

mapview(WA_KING_tracts,
        zcol="accScore_Combined",
        alpha.regions=0.7,
        layer.name = 'Vaccine Access Score - Combined')


mapview(WA_KING_tracts,
        zcol="percNoVehicles",
        alpha.regions=0.7,
        at = seq(0,70,10),
        layer.name = '% Households With No Personal Vehicles')

mapview(WA_KING_tracts,
        zcol="hhPercPov",
        at = seq(0,40,5),
        alpha.regions=0.7,
        layer.name = '% Households Below Poverty Line')

mapview(WA_KING_tracts,
        zcol="popDens",
        alpha.regions=0.7,
        layer.name = 'Population Density Per Sq Mi')

mapview(WA_KING_tracts,
        zcol="racePercWhite",
        alpha.regions=0.7,
        layer.name = '% White')

mapview(WA_KING_tracts,
        zcol="racePercBlack",
        alpha.regions=0.7,
        layer.name = '% Black')

mapview(WA_KING_tracts,
        zcol="racePercAsian",
        alpha.regions=0.7,
        layer.name = '% Asian')


cor.test(WA_KING_tracts$popDens,WA_KING_tracts$accScore_Combined,method = "spearman")
cor.test(WA_KING_tracts$hhPercPov,WA_KING_tracts$accScore_Combined,method = "spearman")
cor.test(WA_KING_tracts$percNoVehicles,WA_KING_tracts$accScore_Combined,method = "spearman")
cor.test(WA_KING_tracts$racePercBlack,WA_KING_tracts$accScore_Combined,method = "spearman")
cor.test(WA_KING_tracts$racePercAsian,WA_KING_tracts$accScore_Combined,method = "spearman")
cor.test(WA_KING_tracts$racePercHisp,WA_KING_tracts$accScore_Combined,method = "spearman")
cor.test(WA_KING_tracts$racePercWhite,WA_KING_tracts$accScore_Combined,method = "spearman")

cor.test(seattleTracts$popDens,seattleTracts$accScore_Combined,method = "spearman")
cor.test(seattleTracts$hhPercPov,seattleTracts$accScore_Combined,method = "spearman")
cor.test(seattleTracts$percNoVehicles,seattleTracts$accScore_Combined,method = "spearman")
cor.test(seattleTracts$racePercBlack,seattleTracts$accScore_Combined,method = "spearman")
cor.test(seattleTracts$racePercAsian,seattleTracts$accScore_Combined,method = "spearman")
cor.test(seattleTracts$racePercHisp,seattleTracts$accScore_Combined,method = "spearman")
cor.test(seattleTracts$racePercWhite,seattleTracts$accScore_Combined,method = "spearman")

cor.test(WA_KING_tracts$hhPercPov,WA_KING_tracts$accScore_Transit,method = "spearman")
cor.test(WA_KING_tracts$percNoVehicles,WA_KING_tracts$accScore_Transit,method = "spearman")

#Making the correlation plots
cvarsKing <- dplyr::select(as.data.frame(WA_KING_tracts),popDens,percNoVehicles,hhPercPov,racePercBlack,racePercWhite,racePercAIAN,racePercAsian,racePercNHPI,accScore_Car,accScore_Transit,accScore_Combined)
MAT_NAMES <- c("Population Density","% No Vehicles","% Poverty", "% Black", "% White", "% AI/AN", "% Asian", "% NH/PI", "Score - Car", "Score - Transit", "Score - Combined")
matKing <- cor(cvarsKing,method="pearson") 
rownames(matKing) <- MAT_NAMES
colnames(matKing) <- MAT_NAMES
corrplot(matKing,method=c("number"))

cvarsSea <- dplyr::select(as.data.frame(seattleTracts),popDens,percNoVehicles,hhPercPov,racePercBlack,racePercWhite,racePercAIAN,racePercAsian,racePercNHPI,accScore_Car,accScore_Transit,accScore_Combined)
matSea <- cor(cvarsSea,method="pearson") 
rownames(matSea) <- MAT_NAMES
colnames(matSea) <- MAT_NAMES
corrplot(matSea,method=c("number"))

ggplot() +
  geom_sf(data=WA_KING_zctas$geometry,
          aes(fill=WA_KING_zctas$accTeen)) +
  ggtitle("Access - Adolescent")

ggplot() +
  geom_sf(data=WA_KING_zctas$geometry,
          aes(fill=WA_KING_zctas$coverTeen)) +
  ggtitle("Vaccine Coverage - Adolescent")
