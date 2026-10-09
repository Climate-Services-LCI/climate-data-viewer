---
  title: "Precip average max 1 day"
output: html_document
date: "2026-03-12"
editor_options: 
  chunk_output_type: console
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Flood Analysis for Vulnerable Communities Platform

#This code utilizes downscaled climate data available from the Cal-Adapt Analytics Engine to generate indicators of 
#flood for the VCP. Specifically, we use downscaled precipitation data from from the five general use projections under 
#SSP370 for two different time scales (2015-2044 and 2045-2074) 
#and a modeled historical baseline data set for precipitation from 1950-2014.

## Data Description

The data are available from the following link:
  
  https://cadcat.s3.amazonaws.com/index.html#loca2/aaa-ca-hybrid/

## Average Max 1 day precip for 2045-2074 ACCESS-CM2

#Determines the average precipitation (inches) from the wettest day of each year during the time period



library(terra)
library(ncdf4)

#Read units 
#nc_data <- nc_open("ACCESSCM2/precip/pr.ACCESS-CM2.historical.r1i1p1f1.1950-2014.LOCA2_CA_hybrid_v20230115.nc")
#head(nc_data)
#units are kg/m^2/s = which is a rate
#units I want: inches of precipitation for each day at each grid location 

###ACCESS 2045###

# File paths
#hist_file <- "ACCESSCM2/precip/pr.ACCESS-CM2.historical.r1i1p1f1.1950-2014.LOCA2_CA_hybrid_v20230115.nc"
future_file <- "ACCESSCM2/precip/pr.ACCESS-CM2.ssp370.r1i1p1f1.2045-2074.LOCA2_CA_hybrid_v20230115.nc"

library(terra)
library(ncdf4)

# Temp directory
if (!dir.exists("terra_tmp")) dir.create("terra_tmp")

terraOptions(progress=1, memfrac=0.8, tempdir="terra_tmp")

# Load precipitation data
r_future <- rast(future_file)

# Convert units: kg m-2 s-1 -> inches/day
r_future <- r_future * 86400 / 25.4

# Extract dates
dates <- as.Date(time(r_future))

# Extract years
years <- format(dates, "%Y")
unique_years <- unique(years)

# Number of years
n_years <- length(unique_years)

# Store yearly max rasters
annual_max_list <- list()

for (y in unique_years) {
  
  # Subset layers for that year
  r_year <- r_future[[which(years == y)]]
  
  # Max daily precipitation for that year
  annual_max <- app(r_year, max, na.rm=TRUE)
  
  annual_max_list[[y]] <- annual_max
}

# Combine yearly maxima
annual_max_stack <- rast(annual_max_list)

# Sum annual maxima
sum_annual_max <- app(
  annual_max_stack,
  sum,
  na.rm=TRUE,
  filename="sum_annual_max.tif",
  overwrite=TRUE
)

# Average annual max 1-day precipitation (inches)
avg_max_1day <- sum_annual_max / n_years

# Save output raster
writeRaster(
  avg_max_1day,
  filename="Precip_AvgMax1Day_2045-2074_ACCESS.nc",
  overwrite=TRUE,
  filetype="netcdf"
)




#ACCESS 2015#

library(terra)
library(ncdf4)

future_file <- "ACCESSCM2/precip/pr.ACCESS-CM2.ssp370.r1i1p1f1.2015-2044.LOCA2_CA_hybrid_v20230115.nc"

# Temp directory
if (!dir.exists("terra_tmp")) dir.create("terra_tmp")

terraOptions(progress=1, memfrac=0.8, tempdir="terra_tmp")

# Load precipitation data
r_future <- rast(future_file)

# Convert units: kg m-2 s-1 -> inches/day
r_future <- r_future * 86400 / 25.4

# Extract dates
dates <- as.Date(time(r_future))

# Extract years
years <- format(dates, "%Y")
unique_years <- unique(years)

# Number of years
n_years <- length(unique_years)

# Store yearly max rasters
annual_max_list <- list()

for (y in unique_years) {
  
  # Subset layers for that year
  r_year <- r_future[[which(years == y)]]
  
  # Max daily precipitation for that year
  annual_max <- app(r_year, max, na.rm=TRUE)
  
  annual_max_list[[y]] <- annual_max
}

# Combine yearly maxima
annual_max_stack <- rast(annual_max_list)

# Sum annual maxima
sum_annual_max <- app(
  annual_max_stack,
  sum,
  na.rm=TRUE,
  filename="sum_annual_max.tif",
  overwrite=TRUE
)

# Average annual max 1-day precipitation (inches)
avg_max_1day <- sum_annual_max / n_years

# Save output raster
writeRaster(
  avg_max_1day,
  filename="Precip_AvgMax1Day_2015-2044_ACCESS.nc",
  overwrite=TRUE,
  filetype="netcdf"
)

###ECEARTH 2045###
install.packages("terra")
install.packages("ncdf4")
library(terra)
library(ncdf4)

# File paths
#hist_file <- "ECEARTH/precip/pr.EC-Earth3.historical.r1i1p1f1.1950-2014.LOCA2_CA_hybrid_v20230115.nc"
future_file <- "ECEARTH/precip/pr.EC-Earth3.ssp370.r1i1p1f1.2045-2074.LOCA2_CA_hybrid_v20230115.nc"


library(terra)
library(ncdf4)

# Temp directory
if (!dir.exists("terra_tmp")) dir.create("terra_tmp")

terraOptions(progress=1, memfrac=0.8, tempdir="terra_tmp")

# Load precipitation data
r_future <- rast(future_file)

# Convert units: kg m-2 s-1 -> inches/day
r_future <- r_future * 86400 / 25.4

# Extract dates
dates <- as.Date(time(r_future))

# Extract years
years <- format(dates, "%Y")
unique_years <- unique(years)

# Number of years
n_years <- length(unique_years)

# Store yearly max rasters
annual_max_list <- list()

for (y in unique_years) {
  
  # Subset layers for that year
  r_year <- r_future[[which(years == y)]]
  
  # Max daily precipitation for that year
  annual_max <- app(r_year, max, na.rm=TRUE)
  
  annual_max_list[[y]] <- annual_max
}

# Combine yearly maxima
annual_max_stack <- rast(annual_max_list)

# Sum annual maxima
sum_annual_max <- app(
  annual_max_stack,
  sum,
  na.rm=TRUE,
  filename="sum_annual_max.tif",
  overwrite=TRUE
)

# Average annual max 1-day precipitation (inches)
avg_max_1day <- sum_annual_max / n_years

# Save output raster
writeRaster(
  avg_max_1day,
  filename="Precip_AvgMax1Day_2045-2074_ECEARTH.nc",
  overwrite=TRUE,
  filetype="netcdf"
)



###ECEARTH 2015###

# File paths
#hist_file <- "ECEARTH/precip/pr.EC-Earth3.historical.r1i1p1f1.1950-2014.LOCA2_CA_hybrid_v20230115.nc"
future_file <- "ECEARTH/precip/pr.EC-Earth3.ssp370.r1i1p1f1.2015-2044.LOCA2_CA_hybrid_v20230115.nc"

library(terra)
library(ncdf4)

# Temp directory
if (!dir.exists("terra_tmp")) dir.create("terra_tmp")

terraOptions(progress=1, memfrac=0.8, tempdir="terra_tmp")

# Load precipitation data
r_future <- rast(future_file)

# Convert units: kg m-2 s-1 -> inches/day
r_future <- r_future * 86400 / 25.4

# Extract dates
dates <- as.Date(time(r_future))

# Extract years
years <- format(dates, "%Y")
unique_years <- unique(years)

# Number of years
n_years <- length(unique_years)

# Store yearly max rasters
annual_max_list <- list()

for (y in unique_years) {
  
  # Subset layers for that year
  r_year <- r_future[[which(years == y)]]
  
  # Max daily precipitation for that year
  annual_max <- app(r_year, max, na.rm=TRUE)
  
  annual_max_list[[y]] <- annual_max
}

# Combine yearly maxima
annual_max_stack <- rast(annual_max_list)

# Sum annual maxima
sum_annual_max <- app(
  annual_max_stack,
  sum,
  na.rm=TRUE,
  filename="sum_annual_max.tif",
  overwrite=TRUE
)

# Average annual max 1-day precipitation (inches)
avg_max_1day <- sum_annual_max / n_years

# Save output raster
writeRaster(
  avg_max_1day,
  filename="Precip_AvgMax1Day_2015-2044_ECEARTH.nc",
  overwrite=TRUE,
  filetype="netcdf"
)


###FGOALS 2045###

# File paths
#hist_file <- "FGOALS/precip/pr.FGOALS-g3.historical.r1i1p1f1.1950-2014.LOCA2_CA_hybrid_v20230115.nc"
future_file <- "FGOALS/precip/pr.FGOALS-g3.ssp370.r1i1p1f1.2045-2074.LOCA2_CA_hybrid_v20230115.nc"


# Temp directory
if (!dir.exists("terra_tmp")) dir.create("terra_tmp")

terraOptions(progress=1, memfrac=0.8, tempdir="terra_tmp")

# Load precipitation data
r_future <- rast(future_file)

# Convert units: kg m-2 s-1 -> inches/day
r_future <- r_future * 86400 / 25.4

# Extract dates
dates <- as.Date(time(r_future))

# Extract years
years <- format(dates, "%Y")
unique_years <- unique(years)

# Number of years
n_years <- length(unique_years)

# Store yearly max rasters
annual_max_list <- list()

for (y in unique_years) {
  
  # Subset layers for that year
  r_year <- r_future[[which(years == y)]]
  
  # Max daily precipitation for that year
  annual_max <- app(r_year, max, na.rm=TRUE)
  
  annual_max_list[[y]] <- annual_max
}

# Combine yearly maxima
annual_max_stack <- rast(annual_max_list)

# Sum annual maxima
sum_annual_max <- app(
  annual_max_stack,
  sum,
  na.rm=TRUE,
  filename="sum_annual_max.tif",
  overwrite=TRUE
)

# Average annual max 1-day precipitation (inches)
avg_max_1day <- sum_annual_max / n_years

# Save output raster
writeRaster(
  avg_max_1day,
  filename="Precip_AvgMax1Day_2045-2074_FGOALS.nc",
  overwrite=TRUE,
  filetype="netcdf"
)

###FGOALS 2015###

#File paths
future_file <- "FGOALS/precip/pr.FGOALS-g3.ssp370.r1i1p1f1.2015-2044.LOCA2_CA_hybrid_v20230115.nc"


# Temp directory
if (!dir.exists("terra_tmp")) dir.create("terra_tmp")

terraOptions(progress=1, memfrac=0.8, tempdir="terra_tmp")

# Load precipitation data
r_future <- rast(future_file)

# Convert units: kg m-2 s-1 -> inches/day
r_future <- r_future * 86400 / 25.4

# Extract dates
dates <- as.Date(time(r_future))

# Extract years
years <- format(dates, "%Y")
unique_years <- unique(years)

# Number of years
n_years <- length(unique_years)

# Store yearly max rasters
annual_max_list <- list()

for (y in unique_years) {
  
  # Subset layers for that year
  r_year <- r_future[[which(years == y)]]
  
  # Max daily precipitation for that year
  annual_max <- app(r_year, max, na.rm=TRUE)
  
  annual_max_list[[y]] <- annual_max
}

# Combine yearly maxima
annual_max_stack <- rast(annual_max_list)

# Sum annual maxima
sum_annual_max <- app(
  annual_max_stack,
  sum,
  na.rm=TRUE,
  filename="sum_annual_max.tif",
  overwrite=TRUE
)

# Average annual max 1-day precipitation (inches)
avg_max_1day <- sum_annual_max / n_years

# Save output raster
writeRaster(
  avg_max_1day,
  filename="Precip_AvgMax1Day_2015-2044_FGOALS.nc",
  overwrite=TRUE,
  filetype="netcdf"
)

###MIROC6 2045###

library(terra)
library(ncdf4)

# File paths
#hist_file <- "MIROC6/precip/pr.MIROC6.historical.r1i1p1f1.1950-2014.LOCA2_CA_hybrid_v20230115.nc"
future_file <- "MIROC6/precip/pr.MIROC6.ssp370.r1i1p1f1.2045-2074.LOCA2_CA_hybrid_v20230115.nc"

# Temp directory
if (!dir.exists("terra_tmp")) dir.create("terra_tmp")

terraOptions(progress=1, memfrac=0.8, tempdir="terra_tmp")

# Load precipitation data
r_future <- rast(future_file)

# Convert units: kg m-2 s-1 -> inches/day
r_future <- r_future * 86400 / 25.4

# Extract dates
dates <- as.Date(time(r_future))

# Extract years
years <- format(dates, "%Y")
unique_years <- unique(years)

# Number of years
n_years <- length(unique_years)

# Store yearly max rasters
annual_max_list <- list()

for (y in unique_years) {
  
  # Subset layers for that year
  r_year <- r_future[[which(years == y)]]
  
  # Max daily precipitation for that year
  annual_max <- app(r_year, max, na.rm=TRUE)
  
  annual_max_list[[y]] <- annual_max
}

# Combine yearly maxima
annual_max_stack <- rast(annual_max_list)

# Sum annual maxima
sum_annual_max <- app(
  annual_max_stack,
  sum,
  na.rm=TRUE,
  filename="sum_annual_max.tif",
  overwrite=TRUE
)

# Average annual max 1-day precipitation (inches)
avg_max_1day <- sum_annual_max / n_years

# Save output raster
writeRaster(
  avg_max_1day,
  filename="Precip_AvgMax1Day_2045-2074_MIROC6.nc",
  overwrite=TRUE,
  filetype="netcdf"
)

#TROUBLESHOOTING MIROC6 2045##

library(terra)
library(ncdf4)

future_file <- "MIROC6/precip/pr.MIROC6.ssp370.r1i1p1f1.2045-2074.LOCA2_CA_hybrid_v20230115.nc"

# Create temp directory
if (!dir.exists("terra_tmp")) dir.create("terra_tmp")

terraOptions(
  progress = 1,
  memfrac = 0.6,
  tempdir = "terra_tmp"
)

# Load precipitation data
r_future <- rast(future_file)

# Convert units: kg m-2 s-1 -> inches/day
r_future <- r_future * 86400 / 25.4

# Extract dates and years
dates <- as.Date(time(r_future))
years <- format(dates, "%Y")
unique_years <- unique(years)

# Number of years
n_years <- length(unique_years)

# Store filenames instead of rasters
annual_files <- c()

for (y in unique_years) {
  
  cat("Processing year:", y, "\n")
  
  # Subset layers for that year
  r_year <- r_future[[which(years == y)]]
  
  # Output filename
  out_file <- paste0("annual_max_", y, ".tif")
  
  # Compute yearly max and write immediately
  annual_max <- app(
    r_year,
    max,
    na.rm = TRUE,
    filename = out_file,
    overwrite = TRUE
  )
  
  annual_files <- c(annual_files, out_file)
  
  rm(r_year, annual_max)
  gc()
}

# Load yearly max rasters
annual_max_stack <- rast(annual_files)

# Sum annual maxima
sum_annual_max <- app(
  annual_max_stack,
  sum,
  na.rm = TRUE,
  filename = "sum_annual_max.tif",
  overwrite = TRUE
)

# Average annual max 1-day precipitation (inches)
avg_max_1day <- sum_annual_max / n_years

# Save final NetCDF
writeRaster(
  avg_max_1day,
  filename = "Precip_AvgMax1Day_2045-2074_MIROC6.nc",
  overwrite = TRUE,
  filetype = "netcdf"
)

#File Clean up
file.remove(annual_files)
file.remove("sum_annual_max.tif")

###MIROC6 2015###

#File paths
future_file <- "MIROC6/precip/pr.MIROC6.ssp370.r1i1p1f1.2015-2044.LOCA2_CA_hybrid_v20230115.nc"

# Create temp directory
if (!dir.exists("terra_tmp")) dir.create("terra_tmp")

terraOptions(
  progress = 1,
  memfrac = 0.6,
  tempdir = "terra_tmp"
)

# Load precipitation data
r_future <- rast(future_file)

# Convert units: kg m-2 s-1 -> inches/day
r_future <- r_future * 86400 / 25.4

# Extract dates and years
dates <- as.Date(time(r_future))
years <- format(dates, "%Y")
unique_years <- unique(years)

# Number of years
n_years <- length(unique_years)

# Store filenames instead of rasters
annual_files <- c()

for (y in unique_years) {
  
  cat("Processing year:", y, "\n")
  
  # Subset layers for that year
  r_year <- r_future[[which(years == y)]]
  
  # Output filename
  out_file <- paste0("annual_max_", y, ".tif")
  
  # Compute yearly max and write immediately
  annual_max <- app(
    r_year,
    max,
    na.rm = TRUE,
    filename = out_file,
    overwrite = TRUE
  )
  
  annual_files <- c(annual_files, out_file)
  
  rm(r_year, annual_max)
  gc()
}

# Load yearly max rasters
annual_max_stack <- rast(annual_files)

# Sum annual maxima
sum_annual_max <- app(
  annual_max_stack,
  sum,
  na.rm = TRUE,
  filename = "sum_annual_max.tif",
  overwrite = TRUE
)

# Average annual max 1-day precipitation (inches)
avg_max_1day <- sum_annual_max / n_years

# Save final NetCDF
writeRaster(
  avg_max_1day,
  filename = "Precip_AvgMax1Day_2015-2044_MIROC6.nc",
  overwrite = TRUE,
  filetype = "netcdf"
)

#File Clean up
file.remove(annual_files)
file.remove("sum_annual_max.tif")


###MPIESM12 2045###

# File paths
#hist_file <- "MPIESM12/precip/pr.MPI-ESM1-2-HR.historical.r3i1p1f1.1950-2014.LOCA2_CA_hybrid_v20230115.nc"
future_file <- "MPIESM12/precip/pr.MPI-ESM1-2-HR.ssp370.r3i1p1f1.2045-2074.LOCA2_CA_hybrid_v20230115.nc"

library(terra)
library(ncdf4)

# Create temp directory
if (!dir.exists("terra_tmp")) dir.create("terra_tmp")

terraOptions(
  progress = 1,
  memfrac = 0.6,
  tempdir = "terra_tmp"
)

# Load precipitation data
r_future <- rast(future_file)

# Convert units: kg m-2 s-1 -> inches/day
r_future <- r_future * 86400 / 25.4

# Extract dates and years
dates <- as.Date(time(r_future))
years <- format(dates, "%Y")
unique_years <- unique(years)

# Number of years
n_years <- length(unique_years)

# Store filenames instead of rasters
annual_files <- c()

for (y in unique_years) {
  
  cat("Processing year:", y, "\n")
  
  # Subset layers for that year
  r_year <- r_future[[which(years == y)]]
  
  # Output filename
  out_file <- paste0("annual_max_", y, ".tif")
  
  # Compute yearly max and write immediately
  annual_max <- app(
    r_year,
    max,
    na.rm = TRUE,
    filename = out_file,
    overwrite = TRUE
  )
  
  annual_files <- c(annual_files, out_file)
  
  rm(r_year, annual_max)
  gc()
}

# Load yearly max rasters
annual_max_stack <- rast(annual_files)

# Sum annual maxima
sum_annual_max <- app(
  annual_max_stack,
  sum,
  na.rm = TRUE,
  filename = "sum_annual_max.tif",
  overwrite = TRUE
)

# Average annual max 1-day precipitation (inches)
avg_max_1day <- sum_annual_max / n_years

# Save final NetCDF
writeRaster(
  avg_max_1day,
  filename = "Precip_AvgMax1Day_2045-2074_MPIESM12.nc",
  overwrite = TRUE,
  filetype = "netcdf"
)

#File Clean up
file.remove(annual_files)
file.remove("sum_annual_max.tif")



###MPIESM12 2015###

#File paths
future_file <- "MPIESM12/precip/pr.MPI-ESM1-2-HR.ssp370.r3i1p1f1.2015-2044.LOCA2_CA_hybrid_v20230115.nc"


library(terra)
library(ncdf4)

# Create temp directory
if (!dir.exists("terra_tmp")) dir.create("terra_tmp")

terraOptions(
  progress = 1,
  memfrac = 0.6,
  tempdir = "terra_tmp"
)

# Load precipitation data
r_future <- rast(future_file)

# Convert units: kg m-2 s-1 -> inches/day
r_future <- r_future * 86400 / 25.4

# Extract dates and years
dates <- as.Date(time(r_future))
years <- format(dates, "%Y")
unique_years <- unique(years)

# Number of years
n_years <- length(unique_years)

# Store filenames instead of rasters
annual_files <- c()

for (y in unique_years) {
  
  cat("Processing year:", y, "\n")
  
  # Subset layers for that year
  r_year <- r_future[[which(years == y)]]
  
  # Output filename
  out_file <- paste0("annual_max_", y, ".tif")
  
  # Compute yearly max and write immediately
  annual_max <- app(
    r_year,
    max,
    na.rm = TRUE,
    filename = out_file,
    overwrite = TRUE
  )
  
  annual_files <- c(annual_files, out_file)
  
  rm(r_year, annual_max)
  gc()
}

# Load yearly max rasters
annual_max_stack <- rast(annual_files)

# Sum annual maxima
sum_annual_max <- app(
  annual_max_stack,
  sum,
  na.rm = TRUE,
  filename = "sum_annual_max.tif",
  overwrite = TRUE
)

# Average annual max 1-day precipitation (inches)
avg_max_1day <- sum_annual_max / n_years

# Save final NetCDF
writeRaster(
  avg_max_1day,
  filename = "Precip_AvgMax1Day_2015-2044_MPIESM12.nc",
  overwrite = TRUE,
  filetype = "netcdf"
)

#File Clean up
file.remove(annual_files)
file.remove("sum_annual_max.tif")



## Calculating average of the 5 general use projections 2015-2044

library(stars)
library(terra)

# Define NetCDF files
nc_files <- c(
  "Precip_AvgMax1Day_2015-2044_ACCESS.nc", 
  "Precip_AvgMax1Day_2015-2044_ECEARTH.nc", 
  "Precip_AvgMax1Day_2015-2044_FGOALS.nc",
  "Precip_AvgMax1Day_2015-2044_MIROC6.nc",
  "Precip_AvgMax1Day_2015-2044_MPIESM12.nc"
)

# Load files
stars_list <- lapply(nc_files, read_stars)

# Rename the variable in each object to the same name
for(i in seq_along(stars_list)){
  names(stars_list[[i]]) <- "precip"
}

#Check dimensions
#lapply(stars_list, st_dimensions)

# Use first dataset as template grid
template <- stars_list[[1]]

# Resample all others to match
for(i in seq_along(stars_list)){
  if(i != 1){
    stars_list[[i]] <- st_warp(stars_list[[i]], template)
  }
}

# Add model dimension
for(i in seq_along(stars_list)){
  stars_list[[i]] <- st_redimension(stars_list[[i]], model = i)
}

# Combine
combined <- do.call(c, stars_list)

# Average across models
averaged_data <- st_apply(combined, c("x","y"), mean, na.rm = TRUE)

# Save NetCDF
write_stars(averaged_data, "Precip_AvgMax1Day_2015-2044_GENUSE.nc")

#Convert raster to GeoTIFF file
library(terra)

r <- rast("Precip_AvgMax1Day_2015-2044_GENUSE.nc")

writeRaster(r, "Precip_AvgMax1Day_2015-2044_GENUSE.tif", overwrite = TRUE)



## Calculating average of the 5 general use projections 2045-2074

library(stars)
library(terra)

# Define NetCDF files
nc_files <- c(
  "Precip_AvgMax1Day_2045-2074_ACCESS.nc", 
  "Precip_AvgMax1Day_2045-2074_ECEARTH.nc", 
  "Precip_AvgMax1Day_2045-2074_FGOALS.nc",
  "Precip_AvgMax1Day_2045-2074_MIROC6.nc",
  "Precip_AvgMax1Day_2045-2074_MPIESM12.nc"
)

# Load files
stars_list <- lapply(nc_files, read_stars)

# Rename the variable in each object to the same name
for(i in seq_along(stars_list)){
  names(stars_list[[i]]) <- "precip"
}

# Use first dataset as template grid
template <- stars_list[[1]]

# Resample all others to match
for(i in seq_along(stars_list)){
  if(i != 1){
    stars_list[[i]] <- st_warp(stars_list[[i]], template)
  }
}


# Add model dimension
for(i in seq_along(stars_list)){
  stars_list[[i]] <- st_redimension(stars_list[[i]], model = i)
}

# Combine
combined <- do.call(c, stars_list)

# Average across models
averaged_data <- st_apply(combined, c("x","y"), mean, na.rm = TRUE)

# Save NetCDF
write_stars(averaged_data, "Precip_AvgMax1Day_2045-2074_GENUSE.nc")

#Convert raster to GeoTIFF file
library(terra)

r <- rast("Precip_AvgMax1Day_2045-2074_GENUSE.nc")

writeRaster(r, "Precip_AvgMax1Day_2045-2074_GENUSE.tif", overwrite = TRUE)


###Reworked Calculative average of 5 projects for 2045-2074###
library(terra)

# NetCDF files
nc_files <- c(
  "Precip_AvgMax1Day_2045-2074_ACCESS.nc", 
  "Precip_AvgMax1Day_2045-2074_ECEARTH.nc", 
  "Precip_AvgMax1Day_2045-2074_FGOALS.nc",
  "Precip_AvgMax1Day_2045-2074_MIROC6.nc",
  "Precip_AvgMax1Day_2045-2074_MPIESM12.nc"
)

# Load rasters
rasters <- lapply(nc_files, rast)

# Use first raster as template
template <- rasters[[1]]

# Resample all rasters to same grid
rasters <- lapply(rasters, function(r){
  resample(r, template, method="bilinear")
})

# Stack them
stacked <- rast(rasters)

# Average across models
avg_raster <- mean(stacked, na.rm=TRUE)

# Save outputs
writeRaster(avg_raster, "Precip_AvgMax1Day_2045-2074_GENUSE.tif", overwrite=TRUE)
writeCDF(avg_raster, "Precip_AvgMax1Day_2045-2074_GENUSE.nc", overwrite=TRUE)


###Reworked Calculative average of 5 projects for 2015-2044###
library(terra)

# NetCDF files
nc_files <- c(
  "Precip_AvgMax1Day_2015-2044_ACCESS.nc", 
  "Precip_AvgMax1Day_2015-2044_ECEARTH.nc", 
  "Precip_AvgMax1Day_2015-2044_FGOALS.nc",
  "Precip_AvgMax1Day_2015-2044_MIROC6.nc",
  "Precip_AvgMax1Day_2015-2044_MPIESM12.nc"
)

# Load rasters
rasters <- lapply(nc_files, rast)

# Use first raster as template
template <- rasters[[1]]

# Resample all rasters to same grid
rasters <- lapply(rasters, function(r){
  resample(r, template, method="bilinear")
})

# Stack them
stacked <- rast(rasters)

# Average across models
avg_raster <- mean(stacked, na.rm=TRUE)

# Save outputs
writeRaster(avg_raster, "Precip_AvgMax1Day_2015-2044_GENUSE.tif", overwrite=TRUE)
writeCDF(avg_raster, "Precip_AvgMax1Day_2015-2044_GENUSE.nc", overwrite=TRUE)