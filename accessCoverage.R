 options(java.parameters = "-Xmx64g")
#install.packages("lwgeom") #Dependency for centroid

library(r5r)
library(sf)
sf::sf_use_s2(FALSE) #Incompatible with projections
library(spdep)
library(spatialreg)

library(data.table)
library(tidycensus)
census_api_key("5290b3ffc1891002ed20b36df6668db209b09f47",install=TRUE, overwrite=TRUE) #Please install with your own census API key
library(tigris)
options(tigris_use_cache = TRUE)
var_acs19 <- load_variables(2019, "acs5", cache = TRUE) #Use this to lookup variables

library(ggplot2)
library(mapview)
mapviewOptions(platform = 'leafgl')
library(tictoc)
library(corrplot)
library(dplyr)
library(RColorBrewer)
library(ggplot2)

EXCL_ZCTAS <- c("98070") #Excluding Vashon because geographically it is isolated and is an outlier heavily skews results


data_path <- "./"
options(timeout=120)
r5r_core <- setup_r5(data_path = data_path,temp_dir = FALSE)

#Vaccine Coverage Data from KCPH
vac_coverage <- read.csv("./VaccineRates.csv",fileEncoding = "UTF-8-BOM") #Due to Mac
vac_coverage$ZIP_CODE <- as.character(vac_coverage$ZIP_CODE)
vac_coverage <- vac_coverage %>% 
  mutate(rateChild=replace(rateChild, rateChild == -1,NA)) %>%
  mutate(rateTeen=replace(rateTeen, rateTeen == -1,NA))

#break
#Get King County for clipping ZCTAs
KING_OUTLINE <- get_acs(geography = "county",state="WA",county="KING",table="B01003",geometry=TRUE,cb=FALSE)

WA_KING_zctas <- get_acs(geography = "zcta",
                         table = "B01003",
                         geometry = TRUE,
                         cb = FALSE) %>% 
  dplyr::arrange(GEOID) %>%
  filter(GEOID %in% vac_coverage$ZIP_CODE) #Limits ZCTAs to King County ZIP Codes
WA_KING_zctas$popTotal <- WA_KING_zctas$estimate


#Table B08201: Vehicle ownership by household
#Var B08201_001: Total number of households
#Var B08201_002: Households with zero vehicles
WA_KING_vehicleOwn <- get_acs(geography = "zcta",
                              table = "B08201",
                              geometry = FALSE,
                              output = "wide") %>% 
  dplyr::arrange(GEOID) %>%
  filter(GEOID %in% vac_coverage$ZIP_CODE)
WA_KING_zctas$percNoVehicles <- WA_KING_vehicleOwn$B08201_002E/WA_KING_vehicleOwn$B08201_001E * 100

#Table B10010: Median Income
#Var B10010_001
WA_KING_hhIncome <- get_acs(geography = "zcta",,
                              table = "B10010",
                              geometry = FALSE,
                              output = "wide") %>% 
  dplyr::arrange(GEOID) %>%
  filter(GEOID %in% vac_coverage$ZIP_CODE)

WA_KING_zctas$hhMedInc <- WA_KING_hhIncome$B10010_001E #Note high NA percentage, do not use for analysis

#Table B17010 : Income Below Poverty Line 
#Var B17010_001: Total households
#Var B17010_002: Households below FPL
WA_KING_hhPov <- get_acs(geography = "zcta",
                            table = "B17010",
                            geometry = FALSE,
                            output = "wide") %>% 
  dplyr::arrange(GEOID) %>%
  filter(GEOID %in% vac_coverage$ZIP_CODE)

WA_KING_zctas$hhPercPov <- WA_KING_hhPov$B17010_002E/WA_KING_hhPov$B17010_001E * 100

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

WA_KING_race <- get_acs(geography = "zcta",
                            table = "B03002",
                            geometry = FALSE,
                            output = "wide") %>% 
  dplyr::arrange(GEOID) %>%
  filter(GEOID %in% vac_coverage$ZIP_CODE)

WA_KING_zctas$racePercBlack <- WA_KING_race$B03002_004E/WA_KING_race$B03002_001E * 100
WA_KING_zctas$racePercWhite <- WA_KING_race$B03002_003E/WA_KING_race$B03002_001E * 100
WA_KING_zctas$racePercAIAN <- WA_KING_race$B03002_005E/WA_KING_race$B03002_001E * 100
WA_KING_zctas$racePercAsian <- WA_KING_race$B03002_006E/WA_KING_race$B03002_001E * 100
WA_KING_zctas$racePercNHPI <- WA_KING_race$B03002_007E/WA_KING_race$B03002_001E * 100
WA_KING_zctas$racePercHisp <- WA_KING_race$B03002_012E/WA_KING_race$B03002_001E * 100
WA_KING_zctas$racePercOther <- WA_KING_race$B03002_008E/WA_KING_race$B03002_001E * 100
WA_KING_zctas$racePercMulti <- WA_KING_race$B03002_009E/WA_KING_race$B03002_001E * 100

#Table B01001: Age and sex
#Var B01001_002: Total male
#Var B01001_003: Male, less than 5
#Var B01001_004: Male, 5-9 year old
#Var B01001_005: Male, 10-14 year old
#Var B01001_006: Male, 15-17 year old
#Var B01001_026: Total female
#Var B01001_027: Female, less than 5
#Var B01001_028: Female, 5-9 year old
#Var B01001_029: Female, 10-14 year old
#Var B01001_030: Female, 15-17 year old

WA_KING_age <- get_acs(geography = "zcta",
                        table = "B01001",
                        geometry = FALSE,
                        output = "wide") %>% 
  dplyr::arrange(GEOID) %>%
  filter(GEOID %in% vac_coverage$ZIP_CODE)

WA_KING_zctas$totalMale <- WA_KING_age$B01001_002E
WA_KING_zctas$popToddlerMale <- WA_KING_age$B01001_003E #< 5
WA_KING_zctas$popChildMale <- WA_KING_age$B01001_004E + 0.6 * WA_KING_age$B01001_005E #5-9
WA_KING_zctas$popTeensMale <- 0.6 * WA_KING_age$B01001_005E + WA_KING_age$B01001_006E #10-17
WA_KING_zctas$popAdultMale <- WA_KING_zctas$totalMale - WA_KING_zctas$popToddlerMale - WA_KING_zctas$popChildMale - WA_KING_zctas$popTeensMale

WA_KING_zctas$totalFemale <- WA_KING_age$B01001_026E
WA_KING_zctas$popToddlerFemale <- WA_KING_age$B01001_027E #< 5
WA_KING_zctas$popChildFemale <- WA_KING_age$B01001_028E #5-9
WA_KING_zctas$popTeensFemale <- WA_KING_age$B01001_029E + WA_KING_age$B01001_030E #10-17
WA_KING_zctas$popAdultFemale <- WA_KING_zctas$totalFemale - WA_KING_zctas$popToddlerFemale - WA_KING_zctas$popChildFemale - WA_KING_zctas$popTeensFemale

WA_KING_zctas$popToddler <- WA_KING_zctas$popToddlerMale + WA_KING_zctas$popToddlerFemale
WA_KING_zctas$popChild <- WA_KING_zctas$popChildMale + WA_KING_zctas$popChildFemale
WA_KING_zctas$popTeens <- WA_KING_zctas$popTeensMale + WA_KING_zctas$popTeensFemale
WA_KING_zctas$popAdult <- WA_KING_zctas$popAdultMale + WA_KING_zctas$popAdultFemale

WA_KING_zctas$coverChild <-  vac_coverage$rateChild
WA_KING_zctas$coverTeen <-  vac_coverage$rateTeen


#B15001: Educational attainment for older than 25 year old
#Var B15003_001: Total Pop Older Than 25
#Var B15003_22: Bachelor's or higher
#Var B15003_23: Master's or higher
#Var B15003_24: Professional or higher
#Var B15004_25: Doctoral or higher

WA_KING_edu <- get_acs(geography = "zcta",
                       table = "B15003",
                       geometry = FALSE,
                       output = "wide") %>% 
  dplyr::arrange(GEOID) %>%
  filter(GEOID %in% vac_coverage$ZIP_CODE)

WA_KING_zctas$percCollegeGrad <- 100*(WA_KING_edu$B15003_022E + WA_KING_edu$B15003_023E + WA_KING_edu$B15003_024E + WA_KING_edu$B15003_025E)/WA_KING_edu$B15003_001E

st_erase <- function(x, y) {
  st_difference(x, st_union(y))
}

KING_WATER <- area_water("WA", "KING", class = "sf") 

tic("Spatial Water Removal and County Clipping")
  WA_KING_zctas <- st_erase(WA_KING_zctas, KING_WATER)
  WA_KING_zctas <- st_intersection(WA_KING_zctas,KING_OUTLINE)
toc()
#Strips away the list structuring from st_centroid to make a numerical dataframe
cent<-do.call(rbind,
  do.call(rbind,as.data.frame(st_centroid(WA_KING_zctas$geometry))
          )
  )
#Calculate population density with water removed
WA_KING_zctas$area <- 3.86102e-7 * st_area(WA_KING_zctas) #Convert sq meters to sq mi
WA_KING_zctas$popDens <- WA_KING_zctas$popTotal/as.vector(WA_KING_zctas$area) #As vector cleans units

WA_KING_zctas$lon <- cent[,1]
WA_KING_zctas$lat <- cent[,2]

WA_cities <- places(state="WA")
seattleBoundary<-WA_cities$geometry[WA_cities$NAME=="Seattle"]
WA_KING_zctas$inSeattle <- as.numeric(st_intersects(WA_KING_zctas,seattleBoundary))

#Remove the tracts designated for exclusion 
WA_KING_zctas <- dplyr::filter(WA_KING_zctas,!(GEOID %in% EXCL_ZCTAS))

#Fully restructure for r5r to accept


rasterPop <- raster(paste0("./PopRaster/waPop.tif")) #Use for sampling

NUM_SAMPLES <- 200
NUM_ZCTAS <- length(WA_KING_zctas$GEOID)
lat_samp <- matrix(data=0,nrow = NUM_ZCTAS,ncol=NUM_SAMPLES)
lon_samp <- matrix(data=0,nrow = NUM_ZCTAS,ncol=NUM_SAMPLES)

#Sampling various origin points across the ZCTA  by population
for(i in 1:NUM_ZCTAS){
  allPoints <- st_sample(WA_KING_zctas$geometry[i],NUM_SAMPLES * 20)
  pointWt <- raster::extract(rasterPop,st_as_sf(allPoints)) * runif(n = NUM_SAMPLES*20) #Think more about this distribution
  finalPoints <- allPoints[order(pointWt,decreasing = TRUE)[1:NUM_SAMPLES]]
  temp <- as.data.frame(finalPoints) %>% unlist
  lat_samp[i,] <- temp[seq(2,NUM_SAMPLES*2,2)]
  lon_samp[i,] <- temp[seq(1,NUM_SAMPLES*2,2)]
}

#Make the accessibility scores, creates a bootstrap distribution 
accSample_Car <- matrix(data=0,nrow = NUM_ZCTAS,ncol=NUM_SAMPLES)
accSample_Transit <- matrix(data=0,nrow = NUM_ZCTAS,ncol=NUM_SAMPLES)
accSample_Comb <- matrix(data=0,nrow = NUM_ZCTAS,ncol=NUM_SAMPLES)
accSample_Child <- matrix(data=0,nrow = NUM_ZCTAS,ncol=NUM_SAMPLES)
accSample_Teen <- matrix(data=0,nrow = NUM_ZCTAS,ncol=NUM_SAMPLES)
for(s in 1:NUM_SAMPLES){
  print(paste("Simulation No",s,"out of",NUM_SAMPLES))
  tic("Sample Runtime:")
  zctaCoord <- data.frame(
    id=WA_KING_zctas$GEOID,
    lon=lon_samp[,s],
    lat=lat_samp[,s])
  #Swap which filename to use based on if we are getting child vs teen
  for(GET_PEDS in 0:1){
    vaxFilename <- ifelse(GET_PEDS,
           "./VaccineLocationsPeds.csv", #Child
           "./VaccineLocationsAdult.csv") #Teen
    WA_KING_zctas$popCatchement <- ifelse(GET_PEDS,
                                           WA_KING_zctas$popChild, #<12 yo shots
                                           WA_KING_zctas$popTeens) #12 and older shots
    
    vaxCoord <- fread(input=vaxFilename)
    vaxCoord$id <- vaxCoord$id <- 1:length(vaxCoord$Addresses)
    
    #Switch to points, only keep sites if in King County
    vaxCoord_geo <- st_as_sf(vaxCoord,coords = c("lon","lat"),crs = st_crs(WA_KING_zctas))
    vaxCoord_geo$inKingCty <- !is.na(as.numeric(st_intersects(vaxCoord_geo,
                                                              st_make_valid(WA_KING_zctas))))
    if(GET_PEDS){
      vaxCoordChild <- vaxCoord_geo
    } else{
      vaxCoordTeen <- vaxCoord_geo
    }
    vaxCoord$inKingCty <- !is.na(as.numeric(st_intersects(vaxCoord_geo,
                                                          st_make_valid(WA_KING_zctas)))) 
    vaxCoord <- dplyr::filter(vaxCoord,inKingCty == TRUE)
    
      
    modeCar <- c("WALK", "CAR")
    modeTransit <- c("WALK","TRANSIT")
    max_walk_dist <- 800
    max_trip_duration_transit <- 120
    max_trip_duration_car <- 60
    departure_datetime <- as.POSIXct("31-10-2021 14:00:00",
                                     format = "%d-%m-%Y %H:%M:%S")
    attr(departure_datetime,"tzone") <- "America/Los_Angeles"
  
    ttm_Transit <- travel_time_matrix(r5r_core = r5r_core,
                                      origins = zctaCoord,
                                      destinations = vaxCoord,
                                      departure_datetime = departure_datetime,
                                      mode = modeTransit,
                                      max_walk_dist = max_walk_dist,
                                      max_trip_duration = max_trip_duration_transit,
                                      verbose = FALSE)
  
    ttm_Car <- travel_time_matrix(r5r_core = r5r_core,
                              origins = zctaCoord,
                              destinations = vaxCoord,
                              mode = modeCar,
                              max_walk_dist = max_walk_dist,
                              max_trip_duration = max_trip_duration_car,
                              verbose = FALSE)
    
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
    
    
    WA_KING_zctas$accScore_Car <- 0
    WA_KING_zctas$accScore_Transit <- 0
    
    vaxCoord$catchPop <- 0 #Weighted population for each provider ratio
    
    for(i in 1:length(vaxCoord$catchPop)){
      oriProv <- vaxCoord$id[i]
      catchmentZctas <- ttm_Car[which(ttm_Car$toId == oriProv)]
      catchmentZctas$weights <- weight_fcn_car(catchmentZctas$travel_time)
      catchmentZctas$pop <- 0
      for(j in 1:length(catchmentZctas$pop)){
        catchmentZctas$pop <- WA_KING_zctas$popCatchement[which(WA_KING_zctas$GEOID == catchmentZctas$fromId[j])]
      } 
      vaxCoord$catchPop[i] <- sum(catchmentZctas$pop * catchmentZctas$weight)
    }
    
    for(i in 1:length(WA_KING_zctas$GEOID)){
      oriTract <- WA_KING_zctas$GEOID[i]
      destProvCar <- ttm_Car[which(ttm_Car$fromId == oriTract)]
      destProvCar$weight <- weight_fcn_car(destProvCar$travel_time)
      destProvCar$pop <- 0 
      for(j in 1:length(destProvCar$pop)){
        destProvCar$pop[j] <- vaxCoord$catchPop[which(vaxCoord$id == destProvCar$toId[j])] 
      }
      accSample_Car[i,s] <- sum(destProvCar$weight/destProvCar$pop) * 1E5
    
      destProvTransit <- ttm_Transit[which(ttm_Transit$fromId == oriTract)]
      destProvTransit$weight <- weight_fcn_transit(destProvTransit$travel_time)
      destProvTransit$pop <- 0 
      for(j in 1:length(destProvTransit$pop)){
        destProvTransit$pop[j] <- vaxCoord$catchPop[which(vaxCoord$id == destProvTransit$toId[j])] 
      }
      accSample_Transit[i,s] <- sum(destProvTransit$weight/destProvTransit$pop) * 1E5
      
      
      #Combine the two scores based on households with no cars will have to use public transit
      accSample_Comb[i,s] <-
        WA_KING_zctas$percNoVehicles[i]/100 * accSample_Transit[i,s] +
        (1-WA_KING_zctas$percNoVehicles[i]/100) * accSample_Car[i,s]
      
      #weightsTransit <- weight_fcn_transit(ttm_Transit$travel_time[which(ttm_Transit$fromId == oriTract)])
      
      #WA_KING_tracts$accScore_Car[i] <- sum(weightsCar)
      #WA_KING_tracts$accScore_Transit[i] <- sum(weightsTransit)
    }
  
    if(GET_PEDS){ #Assign to the right column
      accSample_Child[,s] <- accSample_Comb[,s]
    }else{
      accSample_Teen[,s] <- accSample_Comb[,s]
    }
  }
  toc()
}
#Sanity plot
#street_net <- street_network_to_sf(r5r_core)
#ggplot() +
#  geom_sf(data = street_net$edges, color='gray85') +

#Keep the median for analysis
WA_KING_zctas$accChild<-apply(accSample_Child,1,median)
WA_KING_zctas$accTeen<-apply(accSample_Teen,1,median)

#Keep the SD for diagnostics
WA_KING_zctas$accChild_sd<-apply(accSample_Child,1,sd)
WA_KING_zctas$accTeen_sd<-apply(accSample_Teen,1,sd)

WA_KING_zctas$inSeattle[is.na(WA_KING_zctas$inSeattle)] <- 0 #Switch NA to 0
seattleZctas <- WA_KING_zctas[WA_KING_zctas$inSeattle == 1,]

stop_r5(r5r_core)
rJava::.jgc(R.gc = TRUE) #Clear up RAM back to R

#Summary Table
meanTable <- as.data.frame(WA_KING_zctas) %>%
  summarize(popDen = mean(popDens,na.rm=TRUE),
            percPov = mean(hhPercPov,na.rm=TRUE),
            percAsian = mean(racePercAsian,na.rm=TRUE),
            percBlack = mean(racePercBlack,na.rm=TRUE),
            percHispanic = mean(racePercHisp,na.rm=TRUE),
            percAIAN = mean(racePercAIAN,na.rm=TRUE),
            percNHPI = mean(racePercNHPI,na.rm=TRUE),
            percCollege = mean(percCollegeGrad,na.rm=TRUE),
            percNoVehicles = mean(percNoVehicles,na.rm=TRUE),
            accTeen = mean(accTeen,na.rm=TRUE),
            accChild = mean(accChild,na.rm=TRUE),
            coverTeen = mean(coverTeen,na.rm=TRUE),
            coverChild = mean(coverChild,na.rm=TRUE)) %>%
  t()
sdTable <- as.data.frame(WA_KING_zctas) %>%
  summarize(popDen = sd(popDens,na.rm=TRUE),
            percPov = sd(hhPercPov,na.rm=TRUE),
            percAsian = sd(racePercAsian,na.rm=TRUE),
            percBlack = sd(racePercBlack,na.rm=TRUE),
            percHispanic = sd(racePercHisp,na.rm=TRUE),
            percAIAN = sd(racePercAIAN,na.rm=TRUE),
            percNHPI = sd(racePercNHPI,na.rm=TRUE),
            percCollege = sd(percCollegeGrad,na.rm=TRUE),
            percNoVehicles = sd(percNoVehicles,na.rm=TRUE),
            accTeen = sd(accTeen,na.rm=TRUE),
            accChild = sd(accChild,na.rm=TRUE),
            coverTeen = sd(coverTeen,na.rm=TRUE),
            coverChild = sd(coverChild,na.rm=TRUE)) %>%
  t()  
summTable <- cbind(meanTable,sdTable) 
colnames(summTable) <- c("Mean", "SD")
rownames(summTable) <- c("Population Density","Percent Households Below the Poverty Line",
                         "Percent Asian Residents","Percent Black Residents","Percent Hispanic Residents",
                         "Percent AI/AN Residents", "Percent NHPI Residents",
                         "Percent Adults with Bachelors or Higher Degree","Percent No Vehicles",
                         "Vaccine Accessibility - Adolescent","Vaccine Accessibility - School Age",
                         "Vaccine Coverage - Adolescent","Vaccine Coverage - School Age")

knitr::kable(summTable,format="html",digits=1,row.names = TRUE)

#Paired t-test for accessibility and coverage
t.test(WA_KING_zctas$accChild,WA_KING_zctas$accTeen,paired = TRUE)
t.test(WA_KING_zctas$coverChild,WA_KING_zctas$coverTeen,paired = TRUE)

#Correlation analysis for accessibility and coverage
cor(WA_KING_zctas$accChild,WA_KING_zctas$accTeen,use="complete.obs")
cor(WA_KING_zctas$coverChild,WA_KING_zctas$coverTeen,use="complete.obs")

#Spatial analysis
nb<-poly2nb(WA_KING_zctas)
lw <- nb2listw(nb,style="W",zero.policy = TRUE) #Neighbor weights, adjacency based, equal

miTeen<-moran.mc(x = WA_KING_zctas$coverTeen,listw = lw,na.action = na.omit,zero.policy = TRUE,nsim=9999)
miChild<-moran.mc(x = WA_KING_zctas$coverChild,listw = lw,na.action = na.omit,zero.policy = TRUE,nsim=9999)

WA_KING_zctas$popDenScale <- WA_KING_zctas$popDens/1E3 #Scale pop density to be per 1000/sq mi for reporting


logit <- function(p){
  return(log(p/(1-p)))
}

#VIF Check
corrTesting <- lm(coverTeen ~ accTeen +
             log10(popDens) +
             hhPercPov +
             racePercBlack +
             racePercAsian +
             racePercHisp +
             racePercAIAN + 
             racePercNHPI +
             percCollegeGrad,
           data = WA_KING_zctas)
car::vif(corrTesting)

corrTesting_Child <- lm(coverChild ~ accChild +
                    log10(popDens) +
                    hhPercPov +
                    racePercBlack +
                    racePercAsian +
                    racePercHisp +
                    racePercAIAN + 
                    racePercNHPI +
                    percCollegeGrad,
                  data = WA_KING_zctas)
car::vif(corrTesting_Child)


corrTesting_noPopDens <- lm(coverTeen ~ accTeen +
                              hhPercPov +
                              racePercBlack +
                              racePercAsian +
                              racePercHisp +
                              racePercAIAN + 
                              racePercNHPI +
                              percCollegeGrad,
                            data = WA_KING_zctas)

car::vif(corrTesting_noPopDens)

corrTesting_Child_noPD <- lm(coverChild ~ accChild +
                          hhPercPov +
                          racePercBlack +
                          racePercAsian +
                          racePercHisp +
                          racePercAIAN + 
                          racePercNHPI +
                          percCollegeGrad,
                        data = WA_KING_zctas)
car::vif(corrTesting_Child_noPD)

#Interaction Checking
intDensModel <- errorsarlm(coverTeen ~ accTeen * log10(popDens) +
                     log10(popDens) +
                     hhPercPov +
                     racePercBlack +
                     racePercAsian +
                     racePercHisp +
                     racePercAIAN + 
                     racePercNHPI +
                     percCollegeGrad,
                   data = WA_KING_zctas,listw = lw,zero.policy = TRUE)

intPovModel <- errorsarlm(coverTeen ~ accTeen * hhPercPov +
                             log10(popDens) +
                             hhPercPov +
                             racePercBlack +
                             racePercAsian +
                             racePercHisp +
                             racePercAIAN + 
                             racePercNHPI +
                             percCollegeGrad,
                           data = WA_KING_zctas,listw = lw,zero.policy = TRUE)

intCollegeModel <- errorsarlm(coverTeen ~ accTeen * percCollegeGrad +
                             log10(popDens) +
                             hhPercPov +
                             racePercBlack +
                             racePercAsian +
                             racePercHisp +
                             racePercAIAN + 
                             racePercNHPI +
                             percCollegeGrad,
                           data = WA_KING_zctas,listw = lw,zero.policy = TRUE)

summary(
  errorsarlm(coverTeen ~ accTeen * percCollegeGrad +
             hhPercPov +
             racePercBlack +
             racePercAsian +
             racePercHisp +
             racePercAIAN + 
             racePercNHPI +
             percCollegeGrad,
           data = WA_KING_zctas,listw = lw,zero.policy = TRUE)
)
#Make the range of values the heat map will test over
acc.seq <- seq(70,170,length.out=101)
pov.seq <- seq(0,15,length.out=101)
dens.seq <- 10^seq(0,5,length.out=101) #Log-space transform
col.seq <- seq(0,100,length.out=101)

#Houeshold Poverty Interaction
pov.grid <- data.frame(expand.grid(acc.seq,pov.seq))
colnames(pov.grid) <- c("acc","pov")
pov.predFcn <- function(acc,pov){ #Prediction function which uses mean values for variables not interacting
  predict(
    intPovModel,
    data.frame(
      accTeen = acc,
      hhPercPov = pov,
      popDens = mean(WA_KING_zctas$popDens),
      racePercBlack = mean(WA_KING_zctas$racePercBlack,na.rm=TRUE),
      racePercAsian = mean(WA_KING_zctas$racePercAsian,na.rm=TRUE),
      racePercHisp = mean(WA_KING_zctas$racePercHisp,na.rm=TRUE),
      racePercAIAN = mean(WA_KING_zctas$racePercAIAN,na.rm=TRUE),
      racePercNHPI = mean(WA_KING_zctas$racePercNHPI,na.rm=TRUE),
      percCollegeGrad = mean(WA_KING_zctas$percCollegeGrad,na.rm=TRUE)
    )
  )  
} 

pov.grid$cov <- pov.predFcn(pov.grid$acc,pov.grid$pov)[,1]
ggplot(data=pov.grid) +
  geom_tile(aes(
    acc,
    pov,
    fill=cov
    )) +
  scale_fill_gradient(low="red",high="yellow") +
  labs(x="Access",y="Household Poverty (%)",fill="Predicted Coverage (%)") +
  geom_point(data=filter(WA_KING_zctas,accTeen>70),
             aes(
               x=accTeen,
               y=hhPercPov
             )
  )


#Population Density
dens.grid <- data.frame(expand.grid(acc.seq,dens.seq))
colnames(dens.grid) <- c("acc","dens")
dens.predFcn <- function(acc,dens){
  predict(
    intDensModel,
    data.frame(
      accTeen = acc,
      popDens = dens,
      hhPercPov = mean(WA_KING_zctas$hhPercPov,na.rm=TRUE),
      racePercBlack = mean(WA_KING_zctas$racePercBlack,na.rm=TRUE),
      racePercAsian = mean(WA_KING_zctas$racePercAsian,na.rm=TRUE),
      racePercHisp = mean(WA_KING_zctas$racePercHisp,na.rm=TRUE),
      racePercAIAN = mean(WA_KING_zctas$racePercAIAN,na.rm=TRUE),
      racePercNHPI = mean(WA_KING_zctas$racePercNHPI,na.rm=TRUE),
      percCollegeGrad = mean(WA_KING_zctas$percCollegeGrad,na.rm=TRUE)
    )
  )  
} 

dens.grid$cov <- dens.predFcn(dens.grid$acc,dens.grid$dens)[,1]
ggplot(data=dens.grid) +
  geom_tile(aes(
    acc,
    dens,
    fill=cov
  )) +
  scale_fill_gradient(low="red",high="yellow") +
  scale_y_log10() +
  labs(x="Access",y="Population Density",fill="Predicted Coverage (%)") +
  geom_point(data=filter(WA_KING_zctas,accTeen>70),
             aes(
               x=accTeen,
               y=popDens
               )
             )

#College Density
col.grid <- data.frame(expand.grid(acc.seq,col.seq))
colnames(col.grid) <- c("acc","col")
col.predFcn <- function(acc,col){
  predict(
    intCollegeModel,
    data.frame(
      accTeen = acc,
      popDens = mean(WA_KING_zctas$popDens,na.rm=TRUE),
      hhPercPov = mean(WA_KING_zctas$hhPercPov,na.rm=TRUE),
      racePercBlack = mean(WA_KING_zctas$racePercBlack,na.rm=TRUE),
      racePercAsian = mean(WA_KING_zctas$racePercAsian,na.rm=TRUE),
      racePercHisp = mean(WA_KING_zctas$racePercHisp,na.rm=TRUE),
      racePercAIAN = mean(WA_KING_zctas$racePercAIAN,na.rm=TRUE),
      racePercNHPI = mean(WA_KING_zctas$racePercNHPI,na.rm=TRUE),
      percCollegeGrad = col
    )
  )  
} 

col.grid$cov <- col.predFcn(col.grid$acc,col.grid$col)[,1]
ggplot(data=col.grid) +
  geom_tile(aes(
    acc,
    col,
    fill=cov
  )) +
  scale_fill_gradient(low="red",high="yellow") +  
  labs(x="Access",y="College Percent",fill="Predicted Coverage (%)") +
  geom_point(data=filter(WA_KING_zctas,accTeen>70),
             aes(
               x=accTeen,
               y=percCollegeGrad
               )
             )


summary(lm(coverTeen ~ accTeen,data = WA_KING_zctas))
oneVarCovReg_Teen <- errorsarlm(coverTeen ~ accTeen,data = WA_KING_zctas,listw = lw,zero.policy = TRUE)
multiVarCovReg_Teen_noInt <- errorsarlm(coverTeen ~ accTeen + 
                                    hhPercPov +
                                    racePercBlack +
                                    racePercAsian +
                                    racePercHisp +
                                    racePercAIAN + 
                                    racePercNHPI +
                                    percCollegeGrad,
                                  data = WA_KING_zctas,listw = lw,zero.policy = TRUE)

multiVarCovReg_Teen_noInt_poisson <- errorsarlm(log(coverTeen) ~ accTeen + 
                                          hhPercPov +
                                          racePercBlack +
                                          racePercAsian +
                                          racePercHisp +
                                          racePercAIAN + 
                                          racePercNHPI +
                                          percCollegeGrad,
                                        data = WA_KING_zctas,listw = lw,zero.policy = TRUE)
break

multiVarCovReg_Teen <- errorsarlm(coverTeen ~ accTeen + 
                               hhPercPov +
                               racePercBlack +
                               racePercAsian +
                               racePercHisp +
                               racePercAIAN + 
                               racePercNHPI +
                               percCollegeGrad * accTeen,
                             data = WA_KING_zctas,listw = lw,zero.policy = TRUE)

multiVarAccReg_Teen <- errorsarlm(accTeen ~ hhPercPov +
                               racePercBlack +
                               racePercAsian +
                               racePercHisp +
                               racePercAIAN + 
                               racePercNHPI + 
                               percCollegeGrad,
                               data = WA_KING_zctas,listw = lw,zero.policy = TRUE)

oneVarCovReg_Child <- errorsarlm(coverChild ~ accChild,data = WA_KING_zctas,listw = lw,zero.policy = TRUE)
multiVarCovReg_Child <- errorsarlm(coverChild ~ accChild +
                                    hhPercPov +
                                    racePercBlack +
                                    racePercAsian +
                                    racePercHisp +
                                    racePercAIAN + 
                                    racePercNHPI +
                                    percCollegeGrad * accChild,
                                  data = WA_KING_zctas,listw = lw,zero.policy = TRUE)
multiVarCovReg_Child_noInt <- errorsarlm(coverChild ~ accChild +
                                     hhPercPov +
                                     racePercBlack +
                                     racePercAsian +
                                     racePercHisp +
                                     racePercAIAN + 
                                     racePercNHPI +
                                     percCollegeGrad,
                                   data = WA_KING_zctas,listw = lw,zero.policy = TRUE)
multiVarAccReg_Child <- errorsarlm(accChild ~ hhPercPov +
                                    racePercBlack +
                                    racePercAsian +
                                    racePercHisp +
                                    racePercAIAN + 
                                    racePercNHPI +
                                    percCollegeGrad,
                                  data = WA_KING_zctas,listw = lw,zero.policy = TRUE)


theme_set(theme_bw())

#Quick map plot for whatever checks you need
ggplot() +
  geom_sf(data=WA_KING_zctas$geometry,
          aes(fill=WA_KING_zctas$racePercAIAN)) +
  labs(title="%AI/AN",fill="Percent") +
  scale_fill_gradient(low="blue3",high="cyan",limits=c(0,4.1))

#Demo map to explain e2SFCA



ggplot() +
  geom_sf(data = WA_KING_zctas$geometry)


#Access Maps
mapAcc_Adol<- ggplot() +
  geom_sf(data=WA_KING_zctas$geometry,
          aes(fill=WA_KING_zctas$accTeen)) +
  labs(title="Vaccine Accessibility - Adolescent",fill="Vaccine Sites \nper 100,000 Children") +
  scale_fill_gradient(low="blue3",high="cyan",limits=c(0,200))
mapAcc_Adol %>% ggsave(filename="./Fig/AdolAccess_Map.tif", width = 8,height = 6,dpi = 300,device = "tiff")

mapAcc_Child <- ggplot() +
  geom_sf(data=WA_KING_zctas$geometry,
          aes(fill=WA_KING_zctas$accChild)) +
  labs(title="Vaccine Accessibility - School Age",fill="Vaccine Sites \nper 100,000 Children") +
  scale_fill_gradient(low="blue3",high="cyan",limits=c(0,50))
mapAcc_Child %>% ggsave(filename="./Fig/ChildAccess_Map.tif", width = 8,height = 6,dpi = 300,device = "tiff")

mapAcc_Adol_SD <- ggplot() +
  geom_sf(data=WA_KING_zctas$geometry,
          aes(fill=WA_KING_zctas$accTeen_sd)) +
  labs(title="SD of Vaccine Accessibility - Adolescent",fill="SD") +
  scale_fill_gradient(low="blue3",high="cyan",limits=c(0,21))
mapAcc_Adol_SD %>% ggsave(filename="./Fig/AdolAccess_Map_SD.tif", width = 8,height = 6,dpi = 300,device = "tiff")


mapAcc_Child_SD <- ggplot() +
  geom_sf(data=WA_KING_zctas$geometry,
          aes(fill=WA_KING_zctas$accChild_sd)) +
  labs(title="SD of Vaccine Accessibility - School Age",fill="SD") +
  scale_fill_gradient(low="blue3",high="cyan",limits=c(0,5))
mapAcc_Child_SD %>% ggsave(filename="./Fig/ChildAccess_Map_SD.tif", width = 8,height = 6,dpi = 300,device = "tiff")



mapCov_Adol <- ggplot() +
  geom_sf(data=WA_KING_zctas$geometry,
          aes(fill=WA_KING_zctas$coverTeen)) +
  labs(title="Vaccine Coverage - Adolescent",fill="% Vaccinated \nChildren") +
  scale_fill_gradient(low="red",high="yellow",limits=c(20,100))
mapCov_Adol %>% ggsave (filename="./Fig/AdolCov_Map.tif",width = 8,height = 6, dpi = 300, device = "tiff")

mapCov_Child <- ggplot() +
  geom_sf(data=WA_KING_zctas$geometry,
          aes(fill=WA_KING_zctas$coverChild)) +
  labs(title="Vaccine Coverage - School Age",fill="% Vaccinated \nChildren") +
  scale_fill_gradient(low="red",high="yellow",limits=c(20,100))
mapCov_Child %>% ggsave (filename="./Fig/ChildCov_Map.tif",width = 8,height = 6, dpi = 300, device = "tiff")

#Vaccine Sites Maps
mapSite_Child <- ggplot() +
  geom_sf(data = WA_KING_zctas$geometry,
          color="black") +
  geom_sf(data = vaxCoordChild[vaxCoordChild$inKingCty==TRUE,],
          color = "Red") + 
  ggtitle("Vaccination Sites - School Age")
mapSite_Child %>% ggsave(filename="./Fig/ChildSite_Map.tif", width = 8,height = 6,dpi = 300,device = "tiff")

mapSite_Adol <- ggplot() +
  geom_sf(data = WA_KING_zctas$geometry,
          color="black") +
  geom_sf(data = vaxCoordTeen[vaxCoordTeen$inKingCty==TRUE,],
          color = "Red") + 
  ggtitle("Vaccination Sites - Adolescent")
mapSite_Adol %>% ggsave(filename="./Fig/AdolSite_Map.tif", width = 8,height = 6,dpi = 300,device = "tiff")


mapVehicle <- ggplot() +
  geom_sf(data=WA_KING_zctas$geometry,
          aes(fill=WA_KING_zctas$percNoVehicles)) +
  labs(title="",fill="% No Car") +
  scale_fill_gradient(low="yellow",high="red",limits=c(0,100))
#Scatter Plots For Diagnostics
scatterCorr <- function(x,y,xlab,ylab,xlim=NA,ylim=NA,m="pearson"){
  if(is.na(xlim)[1]){
    plot(x,y,
         main=paste("Correlation:",
                    round(cor(x,y,use="complete.obs",method=m),digits=3)
         ),
         xlab = xlab,
         ylab = ylab
    )
  }else{
    plot(x,y,
         main=paste("Correlation:",
                    round(cor(x,y,use="complete.obs",method=m),digits=3)
                    ),
         xlab = xlab,
         ylab = ylab,
         xlim = xlim,
         ylim = ylim
         )
  }
}

par(mfrow=c(4,2))
par(mar=c(4,4,2,2))
scatterCorr(x=WA_KING_zctas$percCollegeGrad,
            y=WA_KING_zctas$coverTeen,
            xlab="% College",
            ylab="Coverage")    

scatterCorr(x=WA_KING_zctas$percCollegeGrad,
            y=WA_KING_zctas$accTeen,
            xlab="% College",
            ylab="Access")


scatterCorr(x=WA_KING_zctas$racePercAsian,
            y=WA_KING_zctas$coverTeen,
            xlab="% Asian",
            ylab="Coverage")    

scatterCorr(x=WA_KING_zctas$racePercAsian,
            y=WA_KING_zctas$accTeen,
            xlab="% Asian",
            ylab="Access")

scatterCorr(x=WA_KING_zctas$hhPercPov,
            y=WA_KING_zctas$coverTeen,
            xlab="% Poverty",
            ylab="Coverage")    

scatterCorr(x=WA_KING_zctas$hhPercPov,
            y=WA_KING_zctas$accTeen,
            xlab="% Poverty",
            ylab="Access")

scatterCorr(x=WA_KING_zctas$accTeen,
            y=WA_KING_zctas$coverTeen,
            xlab="Access",
            ylab="Coverage")    

WA_KING_zctas$highCollege <- WA_KING_zctas$percCollegeGrad >= 50



zctas_lowCollege <- filter(WA_KING_zctas,percCollegeGrad < 50)
zctas_highCollege <- filter(WA_KING_zctas,percCollegeGrad >= 50)

nb.hc<-poly2nb(zctas_highCollege)
lw.hc <- nb2listw(nb.hc,style="W",zero.policy = TRUE) #Neighbor weights, adjacency based, equal

nb.lc<-poly2nb(zctas_lowCollege)
lw.lc <- nb2listw(nb.lc,style="W",zero.policy = TRUE) #Neighbor weights, adjacency based, equal

CovReg_Teen_HighCollege <- errorsarlm(coverTeen ~ accTeen,
                                      data = zctas_highCollege,listw = lw.hc,zero.policy = TRUE)
CovReg_Teen_LowCollege <- errorsarlm(coverTeen ~ accTeen,
                                     data = zctas_lowCollege,listw = lw.lc,zero.policy = TRUE)

CovReg_Child_HighCollege <- errorsarlm(coverChild ~ accChild,
                                      data = zctas_highCollege,listw = lw.hc,zero.policy = TRUE)
CovReg_Child_LowCollege <- errorsarlm(coverChild ~ accChild,
                                     data = zctas_lowCollege,listw = lw.lc,zero.policy = TRUE)


par(mfrow=c(2,2))
scatterCorr(x=zctas_highCollege$accTeen,
            y=zctas_highCollege$coverTeen,
            xlab="Access (College >= 50%)",
            ylab="Coverage (Teen)",
            xlim=c(0,170),
            ylim=c(0,100))
scatterCorr(x=zctas_lowCollege$accTeen,
            y=zctas_lowCollege$coverTeen,
            xlab="Access (College < 50%)",
            ylab="Coverage (Teen)",
            xlim=c(0,170),
            ylim=c(0,100))
scatterCorr(x=zctas_highCollege$accChild,
            y=zctas_highCollege$coverChild,
            xlab="Access (College >= 50%)",
            ylab="Coverage (Child)",
            xlim=c(0,50),
            ylim=c(0,100))
scatterCorr(x=zctas_lowCollege$accChild,
            y=zctas_lowCollege$coverChild,
            xlab="Access (College < 50%)",
            ylab="Coverage (Child)",
            xlim=c(0,50),
            ylim=c(0,100))

t.test(zctas_highCollege$accChild,zctas_lowCollege$accChild)
t.test(zctas_highCollege$coverChild,zctas_lowCollege$coverChild)


t.test(zctas_highCollege$accTeen,zctas_lowCollege$accTeen)
t.test(zctas_highCollege$coverTeen,zctas_lowCollege$coverTeen)
