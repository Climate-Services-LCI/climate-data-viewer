---
  title: "Max consecutive dry days and count"
output: html_document
date: "2026-04-07"
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


## Dry days count and max consecutive length for 2045-2074 ACCESS-CM2

#Counts the number of dry days, which is where precipitation is <1 mm
#Gives the max length of consecutive dry days over the time period

###ACCESS 2045###

# File paths
#hist_file <- "ACCESSCM2/precip/pr.ACCESS-CM2.historical.r1i1p1f1.1950-2014.LOCA2_CA_hybrid_v20230115.nc"
future_file <- "ACCESSCM2/precip/pr.ACCESS-CM2.ssp370.r1i1p1f1.2045-2074.LOCA2_CA_hybrid_v20230115.nc"

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

# Convert units: kg m-2 s-1 -> mm/day
r_future <- r_future * 86400

# Extract dates and years
time_vals <- time(r_future)
years <- format(time_vals, "%Y")
unique_years <- unique(years)

# Create dry-day mask (TRUE/FALSE)
dry_mask <- r_future < 1


#Count number of dry days
#dry_days <- app(
 # dry_mask,
 # fun = function(x) sum(x, na.rm = TRUE),
#  filename = "total_dry_days.tif",
 # overwrite = TRUE
#)

#Define function for max consecutive dry
max_dry_run <- function(dry) {
  if (all(is.na(dry))) return(NA)
  
  dry[is.na(dry)] <- FALSE  # treat NA as non-dry - this means runs that contain NA days aren't artificially broken!!
  
  r <- rle(dry)
  if (any(r$values)) {
    return(max(r$lengths[r$values]))
  } else {
    return(0)
  }
}

# Max consecutive dry days over entire period
max_dry_spell <- app(
  dry_mask,
  fun = max_dry_run,
  filename = "max_dry_spell.tif",
  overwrite = TRUE
)

#Save outputs
#writeRaster(dry_days,
           # filename = "DryDays_2045-2074_ACCESS.nc",
           # overwrite = TRUE,
           # filetype = "netcdf")

writeRaster(max_dry_spell,
            filename = "MaxDrySpell_2045-2074_ACCESS.nc",
            overwrite = TRUE,
            filetype = "netcdf")





#ACCESS 2015#

future_file <- "ACCESSCM2/precip/pr.ACCESS-CM2.ssp370.r1i1p1f1.2015-2044.LOCA2_CA_hybrid_v20230115.nc"

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

# Convert units: kg m-2 s-1 -> mm/day
r_future <- r_future * 86400

# Extract dates and years
time_vals <- time(r_future)
years <- format(time_vals, "%Y")
unique_years <- unique(years)

# Create dry-day mask (TRUE/FALSE)
dry_mask <- r_future < 1


#Count number of dry days
#dry_days <- app(
# dry_mask,
# fun = function(x) sum(x, na.rm = TRUE),
#  filename = "total_dry_days.tif",
# overwrite = TRUE
#)

#Define function for max consecutive dry
max_dry_run <- function(dry) {
  if (all(is.na(dry))) return(NA)
  
  dry[is.na(dry)] <- FALSE  # treat NA as non-dry - this means runs that contain NA days aren't artificially broken!!
  
  r <- rle(dry)
  if (any(r$values)) {
    return(max(r$lengths[r$values]))
  } else {
    return(0)
  }
}

# Max consecutive dry days over entire period
max_dry_spell <- app(
  dry_mask,
  fun = max_dry_run,
  filename = "max_dry_spell.tif",
  overwrite = TRUE
)

#Save outputs
#writeRaster(dry_days,
# filename = "DryDays_2015-2044_ACCESS.nc",
# overwrite = TRUE,
# filetype = "netcdf")

writeRaster(max_dry_spell,
            filename = "MaxDrySpell_2015-2044_ACCESS.nc",
            overwrite = TRUE,
            filetype = "netcdf")





###ECEARTH 2045###
install.packages("terra")
install.packages("ncdf4")
library(terra)
library(ncdf4)

# File paths
#hist_file <- "ECEARTH/precip/pr.EC-Earth3.historical.r1i1p1f1.1950-2014.LOCA2_CA_hybrid_v20230115.nc"
future_file <- "ECEARTH/precip/pr.EC-Earth3.ssp370.r1i1p1f1.2045-2074.LOCA2_CA_hybrid_v20230115.nc"


# Create temp directory
if (!dir.exists("terra_tmp")) dir.create("terra_tmp")

terraOptions(
  progress = 1,
  memfrac = 0.6,
  tempdir = "terra_tmp"
)

# Load precipitation data
r_future <- rast(future_file)

# Convert units: kg m-2 s-1 -> mm/day
r_future <- r_future * 86400

# Extract dates and years
time_vals <- time(r_future)
years <- format(time_vals, "%Y")
unique_years <- unique(years)

# Create dry-day mask (TRUE/FALSE)
dry_mask <- r_future < 1


#Count number of dry days
#dry_days <- app(
# dry_mask,
# fun = function(x) sum(x, na.rm = TRUE),
#  filename = "total_dry_days.tif",
# overwrite = TRUE
#)

#Define function for max consecutive dry
max_dry_run <- function(dry) {
  if (all(is.na(dry))) return(NA)
  
  dry[is.na(dry)] <- FALSE  # treat NA as non-dry - this means runs that contain NA days aren't artificially broken!!
  
  r <- rle(dry)
  if (any(r$values)) {
    return(max(r$lengths[r$values]))
  } else {
    return(0)
  }
}

# Max consecutive dry days over entire period
max_dry_spell <- app(
  dry_mask,
  fun = max_dry_run,
  filename = "max_dry_spell.tif",
  overwrite = TRUE
)

#Save outputs
#writeRaster(dry_days,
# filename = "DryDays_2015-2044_ACCESS.nc",
# overwrite = TRUE,
# filetype = "netcdf")

writeRaster(max_dry_spell,
            filename = "MaxDrySpell_2045-2074_ECEARTH.nc",
            overwrite = TRUE,
            filetype = "netcdf")






###ECEARTH 2015###

# File paths
#hist_file <- "ECEARTH/precip/pr.EC-Earth3.historical.r1i1p1f1.1950-2014.LOCA2_CA_hybrid_v20230115.nc"
future_file <- "ECEARTH/precip/pr.EC-Earth3.ssp370.r1i1p1f1.2015-2044.LOCA2_CA_hybrid_v20230115.nc"

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

# Convert units: kg m-2 s-1 -> mm/day
r_future <- r_future * 86400

# Extract dates and years
time_vals <- time(r_future)
years <- format(time_vals, "%Y")
unique_years <- unique(years)

# Create dry-day mask (TRUE/FALSE)
dry_mask <- r_future < 1


#Count number of dry days
dry_days <- app(
 dry_mask,
 fun = function(x) sum(x, na.rm = TRUE),
  filename = "total_dry_days.tif",
 overwrite = TRUE
)

#Define function for max consecutive dry
max_dry_run <- function(dry) {
  if (all(is.na(dry))) return(NA)
  
  dry[is.na(dry)] <- FALSE  # treat NA as non-dry - this means runs that contain NA days aren't artificially broken!!
  
  r <- rle(dry)
  if (any(r$values)) {
    return(max(r$lengths[r$values]))
  } else {
    return(0)
  }
}

# Max consecutive dry days over entire period
max_dry_spell <- app(
  dry_mask,
  fun = max_dry_run,
  filename = "max_dry_spell.tif",
  overwrite = TRUE
)

#Save outputs
writeRaster(dry_days,
 filename = "DryDays_2015-2044_ECEARTH.nc",
 overwrite = TRUE,
 filetype = "netcdf")

writeRaster(max_dry_spell,
            filename = "MaxDrySpell_2015-2044_ECEARTH.nc",
            overwrite = TRUE,
            filetype = "netcdf")

###FGOALS 2045###

# File paths
#hist_file <- "FGOALS/precip/pr.FGOALS-g3.historical.r1i1p1f1.1950-2014.LOCA2_CA_hybrid_v20230115.nc"
future_file <- "FGOALS/precip/pr.FGOALS-g3.ssp370.r1i1p1f1.2045-2074.LOCA2_CA_hybrid_v20230115.nc"


library(terra)
library(ncdf4)

if (!dir.exists("terra_tmp")) dir.create("terra_tmp")

terraOptions(
  progress = 1,
  memfrac = 0.6,
  tempdir = "terra_tmp"
)

# Load precipitation data
r_future <- rast(future_file)

# Convert units: kg m-2 s-1 -> mm/day
r_future <- r_future * 86400

# Extract dates and years
time_vals <- time(r_future)
years <- format(time_vals, "%Y")
unique_years <- unique(years)

# Create dry-day mask (TRUE/FALSE)
dry_mask <- r_future < 1


#Count number of dry days
dry_days <- app(
  dry_mask,
  fun = function(x) sum(x, na.rm = TRUE),
  filename = "total_dry_days.tif",
  overwrite = TRUE
)

#Define function for max consecutive dry
max_dry_run <- function(dry) {
  if (all(is.na(dry))) return(NA)
  
  dry[is.na(dry)] <- FALSE  # treat NA as non-dry - this means runs that contain NA days aren't artificially broken!!
  
  r <- rle(dry)
  if (any(r$values)) {
    return(max(r$lengths[r$values]))
  } else {
    return(0)
  }
}

# Max consecutive dry days over entire period
max_dry_spell <- app(
  dry_mask,
  fun = max_dry_run,
  filename = "max_dry_spell.tif",
  overwrite = TRUE
)

#Save outputs
writeRaster(dry_days,
            filename = "DryDays_2045-2074_FGOALS.nc",
            overwrite = TRUE,
            filetype = "netcdf")

writeRaster(max_dry_spell,
            filename = "MaxDrySpell_2045-2074_FGOALS.nc",
            overwrite = TRUE,
            filetype = "netcdf")


###FGOALS 2015###

#File paths
future_file <- "FGOALS/precip/pr.FGOALS-g3.ssp370.r1i1p1f1.2015-2044.LOCA2_CA_hybrid_v20230115.nc"

library(terra)
library(ncdf4)


if (!dir.exists("terra_tmp")) dir.create("terra_tmp")

terraOptions(
  progress = 1,
  memfrac = 0.6,
  tempdir = "terra_tmp"
)

# Load precipitation data
r_future <- rast(future_file)

# Convert units: kg m-2 s-1 -> mm/day
r_future <- r_future * 86400

# Extract dates and years
time_vals <- time(r_future)
years <- format(time_vals, "%Y")
unique_years <- unique(years)

# Create dry-day mask (TRUE/FALSE)
dry_mask <- r_future < 1


#Count number of dry days
dry_days <- app(
  dry_mask,
  fun = function(x) sum(x, na.rm = TRUE),
  filename = "total_dry_days.tif",
  overwrite = TRUE
)

#Define function for max consecutive dry
max_dry_run <- function(dry) {
  if (all(is.na(dry))) return(NA)
  
  dry[is.na(dry)] <- FALSE  # treat NA as non-dry - this means runs that contain NA days aren't artificially broken!!
  
  r <- rle(dry)
  if (any(r$values)) {
    return(max(r$lengths[r$values]))
  } else {
    return(0)
  }
}

# Max consecutive dry days over entire period
max_dry_spell <- app(
  dry_mask,
  fun = max_dry_run,
  filename = "max_dry_spell.tif",
  overwrite = TRUE
)

#Save outputs
writeRaster(dry_days,
            filename = "DryDays_2015-2044_FGOALS.nc",
            overwrite = TRUE,
            filetype = "netcdf")

writeRaster(max_dry_spell,
            filename = "MaxDrySpell_2015-2044_FGOALS.nc",
            overwrite = TRUE,
            filetype = "netcdf")


###MIROC6 2045###

library(terra)
library(ncdf4)

# File paths
#hist_file <- "MIROC6/precip/pr.MIROC6.historical.r1i1p1f1.1950-2014.LOCA2_CA_hybrid_v20230115.nc"
future_file <- "MIROC6/precip/pr.MIROC6.ssp370.r1i1p1f1.2045-2074.LOCA2_CA_hybrid_v20230115.nc"



if (!dir.exists("terra_tmp")) dir.create("terra_tmp")

terraOptions(
  progress = 1,
  memfrac = 0.6,
  tempdir = "terra_tmp"
)

# Load precipitation data
r_future <- rast(future_file)

# Convert units: kg m-2 s-1 -> mm/day
r_future <- r_future * 86400

# Extract dates and years
time_vals <- time(r_future)
years <- format(time_vals, "%Y")
unique_years <- unique(years)

# Create dry-day mask (TRUE/FALSE)
dry_mask <- r_future < 1


#Count number of dry days
dry_days <- app(
  dry_mask,
  fun = function(x) sum(x, na.rm = TRUE),
  filename = "total_dry_days.tif",
  overwrite = TRUE
)

#Define function for max consecutive dry
max_dry_run <- function(dry) {
  if (all(is.na(dry))) return(NA)
  
  dry[is.na(dry)] <- FALSE  # treat NA as non-dry - this means runs that contain NA days aren't artificially broken!!
  
  r <- rle(dry)
  if (any(r$values)) {
    return(max(r$lengths[r$values]))
  } else {
    return(0)
  }
}

# Max consecutive dry days over entire period
max_dry_spell <- app(
  dry_mask,
  fun = max_dry_run,
  filename = "max_dry_spell.tif",
  overwrite = TRUE
)

#Save outputs
writeRaster(dry_days,
            filename = "DryDays_2045-2074_MIROC6.nc",
            overwrite = TRUE,
            filetype = "netcdf")

writeRaster(max_dry_spell,
            filename = "MaxDrySpell_2045-2074_MIROC6.nc",
            overwrite = TRUE,
            filetype = "netcdf")


###MIROC6 2015###

#File paths
future_file <- "MIROC6/precip/pr.MIROC6.ssp370.r1i1p1f1.2015-2044.LOCA2_CA_hybrid_v20230115.nc"


library(terra)
library(ncdf4)

if (!dir.exists("terra_tmp")) dir.create("terra_tmp")

terraOptions(
  progress = 1,
  memfrac = 0.6,
  tempdir = "terra_tmp"
)

# Load precipitation data
r_future <- rast(future_file)

# Convert units: kg m-2 s-1 -> mm/day
r_future <- r_future * 86400

# Extract dates and years
time_vals <- time(r_future)
years <- format(time_vals, "%Y")
unique_years <- unique(years)

# Create dry-day mask (TRUE/FALSE)
dry_mask <- r_future < 1


#Count number of dry days
dry_days <- app(
  dry_mask,
  fun = function(x) sum(x, na.rm = TRUE),
  filename = "total_dry_days.tif",
  overwrite = TRUE
)

#Define function for max consecutive dry
max_dry_run <- function(dry) {
  if (all(is.na(dry))) return(NA)
  
  dry[is.na(dry)] <- FALSE  # treat NA as non-dry - this means runs that contain NA days aren't artificially broken!!
  
  r <- rle(dry)
  if (any(r$values)) {
    return(max(r$lengths[r$values]))
  } else {
    return(0)
  }
}

# Max consecutive dry days over entire period
max_dry_spell <- app(
  dry_mask,
  fun = max_dry_run,
  filename = "max_dry_spell.tif",
  overwrite = TRUE
)

#Save outputs
writeRaster(dry_days,
            filename = "DryDays_2015-2044_MIROC6.nc",
            overwrite = TRUE,
            filetype = "netcdf")

writeRaster(max_dry_spell,
            filename = "MaxDrySpell_2015-2044_MIROC6.nc",
            overwrite = TRUE,
            filetype = "netcdf")


###MPIESM12 2045###

# File paths
#hist_file <- "MPIESM12/precip/pr.MPI-ESM1-2-HR.historical.r3i1p1f1.1950-2014.LOCA2_CA_hybrid_v20230115.nc"
future_file <- "MPIESM12/precip/pr.MPI-ESM1-2-HR.ssp370.r3i1p1f1.2045-2074.LOCA2_CA_hybrid_v20230115.nc"


library(terra)
library(ncdf4)

if (!dir.exists("terra_tmp")) dir.create("terra_tmp")

terraOptions(
  progress = 1,
  memfrac = 0.6,
  tempdir = "terra_tmp"
)

# Load precipitation data
r_future <- rast(future_file)

# Convert units: kg m-2 s-1 -> mm/day
r_future <- r_future * 86400

# Extract dates and years
time_vals <- time(r_future)
years <- format(time_vals, "%Y")
unique_years <- unique(years)

# Create dry-day mask (TRUE/FALSE)
dry_mask <- r_future < 1


#Count number of dry days
dry_days <- app(
  dry_mask,
  fun = function(x) sum(x, na.rm = TRUE),
  filename = "total_dry_days.tif",
  overwrite = TRUE
)

#Define function for max consecutive dry
max_dry_run <- function(dry) {
  if (all(is.na(dry))) return(NA)
  
  dry[is.na(dry)] <- FALSE  # treat NA as non-dry - this means runs that contain NA days aren't artificially broken!!
  
  r <- rle(dry)
  if (any(r$values)) {
    return(max(r$lengths[r$values]))
  } else {
    return(0)
  }
}

# Max consecutive dry days over entire period
max_dry_spell <- app(
  dry_mask,
  fun = max_dry_run,
  filename = "max_dry_spell.tif",
  overwrite = TRUE
)

#Save outputs
writeRaster(dry_days,
            filename = "DryDays_2045-2074_MPIESM12.nc",
            overwrite = TRUE,
            filetype = "netcdf")

writeRaster(max_dry_spell,
            filename = "MaxDrySpell_2045-2074_MPIESM12.nc",
            overwrite = TRUE,
            filetype = "netcdf")


###MPIESM12 2015###

#File paths
future_file <- "MPIESM12/precip/pr.MPI-ESM1-2-HR.ssp370.r3i1p1f1.2015-2044.LOCA2_CA_hybrid_v20230115.nc"


library(terra)
library(ncdf4)


if (!dir.exists("terra_tmp")) dir.create("terra_tmp")

terraOptions(
  progress = 1,
  memfrac = 0.6,
  tempdir = "terra_tmp"
)

# Load precipitation data
r_future <- rast(future_file)

# Convert units: kg m-2 s-1 -> mm/day
r_future <- r_future * 86400

# Extract dates and years
time_vals <- time(r_future)
years <- format(time_vals, "%Y")
unique_years <- unique(years)

# Create dry-day mask (TRUE/FALSE)
dry_mask <- r_future < 1


#Count number of dry days
dry_days <- app(
  dry_mask,
  fun = function(x) sum(x, na.rm = TRUE),
  filename = "total_dry_days.tif",
  overwrite = TRUE
)

#Define function for max consecutive dry
max_dry_run <- function(dry) {
  if (all(is.na(dry))) return(NA)
  
  dry[is.na(dry)] <- FALSE  # treat NA as non-dry - this means runs that contain NA days aren't artificially broken!!
  
  r <- rle(dry)
  if (any(r$values)) {
    return(max(r$lengths[r$values]))
  } else {
    return(0)
  }
}

# Max consecutive dry days over entire period
max_dry_spell <- app(
  dry_mask,
  fun = max_dry_run,
  filename = "max_dry_spell.tif",
  overwrite = TRUE
)

#Save outputs
writeRaster(dry_days,
            filename = "DryDays_2015-2044_MPIESM12.nc",
            overwrite = TRUE,
            filetype = "netcdf")

writeRaster(max_dry_spell,
            filename = "MaxDrySpell_2015-2044_MPIESM12.nc",
            overwrite = TRUE,
            filetype = "netcdf")



### Calculating average for Dry Days Count - 2045-2074 ###
library(terra)

# NetCDF files
nc_files <- c(
  "DryDays_2045-2074_ACCESS.nc", 
  "DryDays_2045-2074_ECEARTH.nc", 
  "DryDays_2045-2074_FGOALS.nc",
  "DryDays_2045-2074_MIROC6.nc",
  "DryDays_2045-2074_MPIESM12.nc"
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
writeRaster(avg_raster, "DryDays_2045-2074_GENUSE.tif", overwrite=TRUE)
writeCDF(avg_raster, "DryDays_2045-2074_GENUSE.nc", overwrite=TRUE)

### Calculating average for Days over 2 - 2045-2074 ###
library(terra)

# NetCDF files
nc_files <- c(
  "MaxDrySpell_2045-2074_ACCESS.nc", 
  "MaxDrySpell_2045-2074_ECEARTH.nc", 
  "MaxDrySpell_2045-2074_FGOALS.nc",
  "MaxDrySpell_2045-2074_MIROC6.nc",
  "MaxDrySpell_2045-2074_MPIESM12.nc"
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
writeRaster(avg_raster, "MaxDrySpell_2045-2074_GENUSE.tif", overwrite=TRUE)
writeCDF(avg_raster, "MaxDrySpell_2045-2074_GENUSE.nc", overwrite=TRUE)


### Calculating average for Days over 1 - 2015-2044 ###
library(terra)

# NetCDF files
nc_files <- c(
  "DryDays_2015-2044_ACCESS.nc", 
  "DryDays_2015-2044_ECEARTH.nc", 
  "DryDays_2015-2044_FGOALS.nc",
  "DryDays_2015-2044_MIROC6.nc",
  "DryDays_2015-2044_MPIESM12.nc"
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
writeRaster(avg_raster, "DryDays_2015-2044_GENUSE.tif", overwrite=TRUE)
writeCDF(avg_raster, "DryDays_2015-2044_GENUSE.nc", overwrite=TRUE)


### Calculating average for Days over 2 - 2015-2044 ###
library(terra)

# NetCDF files
nc_files <- c(
  "MaxDrySpell_2015-2044_ACCESS.nc", 
  "MaxDrySpell_2015-2044_ECEARTH.nc", 
  "MaxDrySpell_2015-2044_FGOALS.nc",
  "MaxDrySpell_2015-2044_MIROC6.nc",
  "MaxDrySpell_2015-2044_MPIESM12.nc"
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
writeRaster(avg_raster, "MaxDrySpell_2015-2044_GENUSE.tif", overwrite=TRUE)
writeCDF(avg_raster, "MaxDrySpell_2015-2044_GENUSE.nc", overwrite=TRUE)
