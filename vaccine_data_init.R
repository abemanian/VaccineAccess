# Filename: vaccine_data_init.r
# Author: Amin Bemanian
# Description: Loads and processes the data, including calculating the travel distances, and then saves it for analysis by vaccine_acc_reg.r
# We separated the data initialization since the travel time calculation step can be very time intensive, so the data can be analyzed without re-running the entire initialization process
# NOTE script relies on package libraries being loaded previously in the run script (vaccine_run.r)
set.seed(17) #Reproducibility
EXCL_ZCTAS <- c("98070") #Excluding Vashon because geographically it is isolated and is an outlier heavily skews results

data_path <- "./"
options(timeout=120)
r5r_core <- setup_r5(data_path = data_path,temp_dir = FALSE)

#Vaccine Coverage Data from KCPH, manually scrapped datafile
vac_coverage <- read.csv("./VaccineRates.csv",fileEncoding = "UTF-8-BOM") #Due to Mac
vac_coverage$ZIP_CODE <- as.character(vac_coverage$ZIP_CODE)
vac_coverage <- vac_coverage %>% 
  mutate(rateChild=replace(rateChild, rateChild == -1,NA)) %>%
  mutate(rateTeen=replace(rateTeen, rateTeen == -1,NA))


#Get King County for clipping ZCTAs
KING_OUTLINE <- get_acs(geography = "county",state="WA",county="KING",table="B01003",geometry=TRUE,cb=FALSE)
WA_KING_zctas <- get_acs(geography = "zcta",
                         table = "B01003",
                         geometry = TRUE,
                         cb = FALSE) %>% 
  dplyr::arrange(GEOID) %>%
  filter(GEOID %in% vac_coverage$ZIP_CODE) #Limits ZCTAs to King County ZIP Codes
WA_KING_zctas$popTotal <- WA_KING_zctas$estimate


#For the following section get_acs returns a data table that includes the estimate of interest for a particular measure
#We then subset the table on the ZCTAs we kept from the vaccine coverage table and sort them in the correct order
#We then add the specific variable of interest from that table to our main dataframe
#Variable naming conventinon is BXXXXX_YYYE where XXXXX is a table number, YYY is variable number, and E indicates estimate (as opposed to variance) 

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
WA_KING_hhIncome <- get_acs(geography = "zcta",
                            table = "B10010",
                            geometry = FALSE,
                            output = "wide") %>% 
  dplyr::arrange(GEOID) %>%
  filter(GEOID %in% vac_coverage$ZIP_CODE)

#Note high NA percentage, do not use for analysis
WA_KING_zctas$hhMedInc <- WA_KING_hhIncome$B10010_001E 

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

#Use this to remove a group of polygons (Y) from a large polygon (X)
st_erase <- function(x, y) {
  st_difference(x, st_union(y))
}

#King County water file
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

#Store the centroid lon/lat 
WA_KING_zctas$lon <- cent[,1]
WA_KING_zctas$lat <- cent[,2]

WA_cities <- places(state="WA")
seattleBoundary<-WA_cities$geometry[WA_cities$NAME=="Seattle"]
WA_KING_zctas$inSeattle <- as.numeric(st_intersects(WA_KING_zctas,seattleBoundary))

#Remove the tracts designated for exclusion 
VASHON_ZCTA <- dplyr::filter(WA_KING_zctas,(GEOID %in% EXCL_ZCTAS)) #Keep for visualization purposes
WA_KING_zctas <- dplyr::filter(WA_KING_zctas,!(GEOID %in% EXCL_ZCTAS))

rasterPop <- raster(paste0("./PopRaster/waPop.tif")) #Population raster to use for sampling different locations for the travel times
NUM_SAMPLES <- 200 #200 set of points takes about 12 hours to run on a Ryzen 5800 with 64 GB RAM
NUM_ZCTAS <- length(WA_KING_zctas$GEOID)
lat_samp <- matrix(data=0,nrow = NUM_ZCTAS,ncol=NUM_SAMPLES)
lon_samp <- matrix(data=0,nrow = NUM_ZCTAS,ncol=NUM_SAMPLES)

#Generate sample points using the population raster
for(i in 1:NUM_ZCTAS){
  allPoints <- st_sample(WA_KING_zctas$geometry[i],NUM_SAMPLES * 20) #Create 20-fold extra points to pick from
  pointWt <- raster::extract(rasterPop,st_as_sf(allPoints)) * runif(n = NUM_SAMPLES*20) #Multiply the raster value with a random uniform weight to create a weighted sample
  finalPoints <- allPoints[order(pointWt,decreasing = TRUE)[1:NUM_SAMPLES]] #Then sort all the created points to get the final sample
  temp <- as.data.frame(finalPoints) %>% unlist
  lat_samp[i,] <- temp[seq(2,NUM_SAMPLES*2,2)]
  lon_samp[i,] <- temp[seq(1,NUM_SAMPLES*2,2)]
}

#Load the weighting function for E2SFCA from the NHTS Travel Time project
load("./AccessibilityWeights.Rdata") #weightMed_Auto and weightMed_Transit

#Then calculate the travel times from the ZCTAs to all the vaccine sites for each sample set of points
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
  #Flag of school age child (GET_PEDS = TRUE) vs teenager (GET_PEDS = FALSE)
  for(GET_PEDS in 0:1){
    vaxFilename <- ifelse(GET_PEDS,
                          "./VaccineLocationsPeds.csv", #Child
                          "./VaccineLocationsAdult.csv") #Teen
    WA_KING_zctas$popCatchement <- ifelse(GET_PEDS,
                                          WA_KING_zctas$popChild, #<12 yo
                                          WA_KING_zctas$popTeens) #12 and older
    
    vaxCoord <- fread(input=vaxFilename)
    vaxCoord$id <- vaxCoord$id <- 1:length(vaxCoord$Addresses)
    
    #Switch to points, only keep sites if in King County
    vaxCoord_geo <- st_as_sf(vaxCoord,coords = c("lon","lat"),crs = st_crs(WA_KING_zctas))
    vaxCoord_geo$inKingCty <- !is.na(as.numeric(st_intersects(vaxCoord_geo,
                                                              st_make_valid(WA_KING_zctas))))
    
    if(GET_PEDS){ #This structure is due to how the code was designed prior to restructuring with samples, will keep as is
      vaxCoordChild <- vaxCoord_geo
    } else{
      vaxCoordTeen <- vaxCoord_geo
    }
    #Double check the vaccine site coordinates all sit within the county
    vaxCoord$inKingCty <- !is.na(as.numeric(st_intersects(vaxCoord_geo, 
                                                          st_make_valid(WA_KING_zctas)))) 
    vaxCoord <- dplyr::filter(vaxCoord,inKingCty == TRUE)
    
    
    #Set up the R5R parameters
    modeCar <- c("WALK", "CAR") #Allow for some walking in order to deal with parking and travel to transit stops
    modeTransit <- c("WALK","TRANSIT")
    max_walk_dist <- 800
    max_trip_duration_transit <- 120
    max_trip_duration_car <- 60
    departure_datetime <- as.POSIXct("31-10-2021 14:00:00",
                                     format = "%d-%m-%Y %H:%M:%S")
    attr(departure_datetime,"tzone") <- "America/Los_Angeles"
    
    #Calculate the travel time matrices for a sample
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
    
    vaxCoord$catchPop <- 0 #Weighted population for each provider ratio
    for(i in 1:length(vaxCoord$catchPop)){
      oriProv <- vaxCoord$id[i]
      catchmentZctas <- ttm_Car[which(ttm_Car$toId == oriProv)]
      #Based solely on automobile weighting since this is the supply side to have as large of a catchment as possible
      catchmentZctas$weights <- weightMed_Auto(catchmentZctas$travel_time) 
      catchmentZctas$pop <- 0
      for(j in 1:length(catchmentZctas$pop)){
        catchmentZctas$pop <- WA_KING_zctas$popCatchement[which(WA_KING_zctas$GEOID == catchmentZctas$fromId[j])]
      } 
      vaxCoord$catchPop[i] <- sum(catchmentZctas$pop * catchmentZctas$weight)
    }
    
    for(i in 1:length(WA_KING_zctas$GEOID)){
      oriTract <- WA_KING_zctas$GEOID[i]
      destProvCar <- ttm_Car[which(ttm_Car$fromId == oriTract)]
      destProvCar$weight <- weightMed_Auto(destProvCar$travel_time)
      destProvCar$pop <- 0 
      for(j in 1:length(destProvCar$pop)){
        destProvCar$pop[j] <- vaxCoord$catchPop[which(vaxCoord$id == destProvCar$toId[j])] 
      }
      accSample_Car[i,s] <- sum(destProvCar$weight/destProvCar$pop) * 1E5 #Used 1E5 as scaling factor for readability (i.e. per 100,000)
      
      destProvTransit <- ttm_Transit[which(ttm_Transit$fromId == oriTract)]
      destProvTransit$weight <- weightMed_Transit(destProvTransit$travel_time)
      destProvTransit$pop <- 0 
      for(j in 1:length(destProvTransit$pop)){
        destProvTransit$pop[j] <- vaxCoord$catchPop[which(vaxCoord$id == destProvTransit$toId[j])] 
      }
      accSample_Transit[i,s] <- sum(destProvTransit$weight/destProvTransit$pop) * 1E5
      
      
      #Combine the two scores based on households with no cars will have to use public transit
      accSample_Comb[i,s] <-
        WA_KING_zctas$percNoVehicles[i]/100 * accSample_Transit[i,s] +
        (1-WA_KING_zctas$percNoVehicles[i]/100) * accSample_Car[i,s]
      
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
WA_KING_zctas$accChild<-apply(accSample_Child,1,median, na.rm=TRUE)
WA_KING_zctas$accTeen<-apply(accSample_Teen,1,median, na.rm=TRUE)

#Keep the SD for diagnostics
WA_KING_zctas$accChild_sd<-apply(accSample_Child,1,sd, na.rm=TRUE)
WA_KING_zctas$accTeen_sd<-apply(accSample_Teen,1,sd, na.rm=TRUE)

WA_KING_zctas$inSeattle[is.na(WA_KING_zctas$inSeattle)] <- 0 #Switch NA to 0
seattleZctas <- WA_KING_zctas[WA_KING_zctas$inSeattle == 1,] #For internal analysis of Seattle city proper, not incl in paper

stop_r5(r5r_core)
rJava::.jgc(R.gc = TRUE) #Clear up RAM back to R

save.image(file="./VaccineData.Rdata")
