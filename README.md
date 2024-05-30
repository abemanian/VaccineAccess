# Pediatric COVID-19 Vaccine Accessibility Code

## Introduction

This document is to serve as a basic guide for using the code that was developed to publish our paper "Investigating the Spatial Accessibility and Coverage of the Pediatric COVID-19 Vaccine: An Ecologic Study of Regional Health Data" in _Vaccines_. [The full-text of that paper can be found here.](https://www.mdpi.com/2076-393X/12/5/545) Our goal is to provide code and the original datasets that should work with minimal modifications to replicate the figures and tables from the paper. Our additional goal is to provide understandable code the can be modified and used by other researchers and organizations for their own questions. If you have any difficulty running the code or questions please feel free to ask any questions. This work is licensed under a Creative Commons Attribution license ([CC-BY](https://creativecommons.org/licenses/by/4.0/)), and any work based on or incorporating this code should cite our [originial paper](https://www.mdpi.com/2076-393X/12/5/545). 

## Hardware and Software Requirements

All of the code for this project was written in [R](https://www.r-project.org/about.html). At the time of writing, we were using R version 4.2.1 (2022-06-23), but we anticipate later versions should work as well. We have tested the code on Windows 10 and 11 as well as macOS 12-14. We would recommend running this code on a multicore CPU (minimum of 4 cores recommended to take advantage of r5r's parallelization) and at minimum 16 GB of RAM. For reference, it took approximately 12 hours to run the full code on a Windows 11 desktop PC with a Ryzen 7 3700X CPU (8 cores/16 threads at 3.6 GHz) and 64 GB DDR4 memory. 

The following R packages are also required for running the code
- `corrplot`
- `data.table`
- `dplyr`
- `ggplot2`
- `ggpubr`
- `lwgeom` 
- `mapview`
- `r5r`
- `RColorBrewer`
- `sf`
- `spatialreg`
- `spdep`
- `tictoc`
- `tidycensus`
- `tigris`

## Code Structure

The code is broken up into 3 main R files. `vaccine_run.R` acts as the primary launch script for the code, and we recommend always running the code from this file. It loads all the basic dependencies and has some flags and parameters required for smooth operation of the variables. After it completes this, it then either launches the full initialization code in `vaccine_data_init.R` or skips ahead to the regression/data analysis code in `vaccine_acc_reg.R` There are three main parameters that should be checked in `vaccine_run.R` prior to running the code:

- On line 1: We set R's Java parameters to `-Xmx64g` so it has access to 64 GB of RAM. This should be changed to how much RAM you would like the code to use. If you are planning to run the data initialization step, we would recommend setting it as high as possible. The regression step does not require as much memory and 4 to 8 GB will likely be sufficient.
- On line 12: The first time this code is run, tidycensus will need to initialize a US Census API key to be able to download the ACS 5-year data sets. If you have used tidycensus before, you can skip this step. Otherwise you will need to request a census key from the US Census Bureau at this [link.](https://api.census.gov/data/key_signup.html) Once you have the key please replace the text `"INSERT_CENSUS_KEY_HERE"` with a string containing the key and uncomment the line for the first time you run the code.
- On line 26: `RUN_DATA_INIT` determines whether the entire code is run (including data initialization) or just the data analysis step. We would recommend only setting this true if you want to test how the E2SFCA calculation code works or if you are applying this code on a new dataset, because the initialization step takes much longer to run than the regression analyses. 

This read-me is still under construction. Please feel to contact us in the interim if you have any questions about the code.
