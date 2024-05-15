options(java.parameters = "-Xmx64g") #Change based on machine you plan to run, only matters for initialization/parallelization steps
#install.packages("lwgeom") #Dependency for centroid calculation

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

RUN_DATA_INIT <- FALSE  #Flag to determine if need to rerun the entire data initialization step

if(RUN_DATA_INIT){source("./vaccine_data_init.R")}
source("./vaccine_acc_reg.R")
