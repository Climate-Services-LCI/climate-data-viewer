---
  title: "Precip average annual"
output: html_document
date: "2026-03-19"
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

## Annual Avg precip for 2045-2074 ACCESS-CM2

#Determines the average precipitation (inches) each year during the time period


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
  
  # Output filename (UPDATED)
  out_file <- paste0("annual_total_", y, ".tif")
  
  # Compute yearly TOTAL precipitation (UPDATED: max -> sum)
  annual_total <- app(
    r_year,
    sum,
    na.rm = TRUE,
    filename = out_file,
    overwrite = TRUE
  )
  
  annual_files <- c(annual_files, out_file)
  
  rm(r_year, annual_total)
  gc()
}


# Load yearly total rasters
annual_total_stack <- rast(annual_files)

# Sum all annual totals
sum_annual_total <- app(
  annual_total_stack,
  sum,
  na.rm = TRUE,
  filename = "sum_annual_total.tif",
  overwrite = TRUE
)

# Average annual precipitation (inches/year)
avg_annual_precip <- sum_annual_total / n_years


# Save final NetCDF
writeRaster(
  avg_annual_precip,
  filename = "Precip_AvgAnnual_2045-2074_ACCESS.nc",
  overwrite = TRUE,
  filetype = "netcdf"
)

#File Clean up
file.remove(annual_files)
file.remove("sum_annual_total.tif")


#ACCESS 2015#

var <- nc_open(future_file)
print(var$var[[1]]$units)

length(which(years == "2015"))

global(r_future, range, na.rm=TRUE)

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
  
  # Output filename (UPDATED)
  out_file <- paste0("annual_total_", y, ".tif")
  
  # Compute yearly TOTAL precipitation (UPDATED: max -> sum)
  annual_total <- app(
    r_year,
    sum,
    na.rm = TRUE,
    filename = out_file,
    overwrite = TRUE
  )
  
  annual_files <- c(annual_files, out_file)
  
  rm(r_year, annual_total)
  gc()
}


# Load yearly total rasters
annual_total_stack <- rast(annual_files)

# Sum all annual totals
sum_annual_total <- app(
  annual_total_stack,
  sum,
  na.rm = TRUE,
  filename = "sum_annual_total.tif",
  overwrite = TRUE
)

# Average annual precipitation (inches/year)
avg_annual_precip <- sum_annual_total / n_years


# Save final NetCDF
writeRaster(
  avg_annual_precip,
  filename = "Precip_AvgAnnual_2015-2044_ACCESS.nc",
  overwrite = TRUE,
  filetype = "netcdf"
)

#File Clean up
file.remove(annual_files)
file.remove("sum_annual_total.tif")


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
  
  # Output filename (UPDATED)
  out_file <- paste0("annual_total_", y, ".tif")
  
  # Compute yearly TOTAL precipitation (UPDATED: max -> sum)
  annual_total <- app(
    r_year,
    sum,
    na.rm = TRUE,
    filename = out_file,
    overwrite = TRUE
  )
  
  annual_files <- c(annual_files, out_file)
  
  rm(r_year, annual_total)
  gc()
}


# Load yearly total rasters
annual_total_stack <- rast(annual_files)

# Sum all annual totals
sum_annual_total <- app(
  annual_total_stack,
  sum,
  na.rm = TRUE,
  filename = "sum_annual_total.tif",
  overwrite = TRUE
)

# Average annual precipitation (inches/year)
avg_annual_precip <- sum_annual_total / n_years


# Save final NetCDF
writeRaster(
  avg_annual_precip,
  filename = "Precip_AvgAnnual_2045-2074_ECEARTH.nc",
  overwrite = TRUE,
  filetype = "netcdf"
)

#File Clean up
file.remove(annual_files)
file.remove("sum_annual_total.tif")





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
  
  # Output filename (UPDATED)
  out_file <- paste0("annual_total_", y, ".tif")
  
  # Compute yearly TOTAL precipitation (UPDATED: max -> sum)
  annual_total <- app(
    r_year,
    sum,
    na.rm = TRUE,
    filename = out_file,
    overwrite = TRUE
  )
  
  annual_files <- c(annual_files, out_file)
  
  rm(r_year, annual_total)
  gc()
}


# Load yearly total rasters
annual_total_stack <- rast(annual_files)

# Sum all annual totals
sum_annual_total <- app(
  annual_total_stack,
  sum,
  na.rm = TRUE,
  filename = "sum_annual_total.tif",
  overwrite = TRUE
)

# Average annual precipitation (inches/year)
avg_annual_precip <- sum_annual_total / n_years


# Save final NetCDF
writeRaster(
  avg_annual_precip,
  filename = "Precip_AvgAnnual_2015-2044_ECEARTH.nc",
  overwrite = TRUE,
  filetype = "netcdf"
)

#File Clean up
file.remove(annual_files)
file.remove("sum_annual_total.tif")



###FGOALS 2045###

# File paths
#hist_file <- "FGOALS/precip/pr.FGOALS-g3.historical.r1i1p1f1.1950-2014.LOCA2_CA_hybrid_v20230115.nc"
future_file <- "FGOALS/precip/pr.FGOALS-g3.ssp370.r1i1p1f1.2045-2074.LOCA2_CA_hybrid_v20230115.nc"


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
  
  # Output filename (UPDATED)
  out_file <- paste0("annual_total_", y, ".tif")
  
  # Compute yearly TOTAL precipitation (UPDATED: max -> sum)
  annual_total <- app(
    r_year,
    sum,
    na.rm = TRUE,
    filename = out_file,
    overwrite = TRUE
  )
  
  annual_files <- c(annual_files, out_file)
  
  rm(r_year, annual_total)
  gc()
}


# Load yearly total rasters
annual_total_stack <- rast(annual_files)

# Sum all annual totals
sum_annual_total <- app(
  annual_total_stack,
  sum,
  na.rm = TRUE,
  filename = "sum_annual_total.tif",
  overwrite = TRUE
)

# Average annual precipitation (inches/year)
avg_annual_precip <- sum_annual_total / n_years


# Save final NetCDF
writeRaster(
  avg_annual_precip,
  filename = "Precip_AvgAnnual_2045-2074_FGOALS.nc",
  overwrite = TRUE,
  filetype = "netcdf"
)

#File Clean up
file.remove(annual_files)
file.remove("sum_annual_total.tif")


###FGOALS 2015###

#File paths
future_file <- "FGOALS/precip/pr.FGOALS-g3.ssp370.r1i1p1f1.2015-2044.LOCA2_CA_hybrid_v20230115.nc"

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
  
  # Output filename (UPDATED)
  out_file <- paste0("annual_total_", y, ".tif")
  
  # Compute yearly TOTAL precipitation (UPDATED: max -> sum)
  annual_total <- app(
    r_year,
    sum,
    na.rm = TRUE,
    filename = out_file,
    overwrite = TRUE
  )
  
  annual_files <- c(annual_files, out_file)
  
  rm(r_year, annual_total)
  gc()
}


# Load yearly total rasters
annual_total_stack <- rast(annual_files)

# Sum all annual totals
sum_annual_total <- app(
  annual_total_stack,
  sum,
  na.rm = TRUE,
  filename = "sum_annual_total.tif",
  overwrite = TRUE
)

# Average annual precipitation (inches/year)
avg_annual_precip <- sum_annual_total / n_years


# Save final NetCDF
writeRaster(
  avg_annual_precip,
  filename = "Precip_AvgAnnual_2015-2044_FGOALS.nc",
  overwrite = TRUE,
  filetype = "netcdf"
)

#File Clean up
file.remove(annual_files)
file.remove("sum_annual_total.tif")


###MIROC6 2045###

library(terra)
library(ncdf4)

# File paths
#hist_file <- "MIROC6/precip/pr.MIROC6.historical.r1i1p1f1.1950-2014.LOCA2_CA_hybrid_v20230115.nc"
future_file <- "MIROC6/precip/pr.MIROC6.ssp370.r1i1p1f1.2045-2074.LOCA2_CA_hybrid_v20230115.nc"


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
  
  # Output filename (UPDATED)
  out_file <- paste0("annual_total_", y, ".tif")
  
  # Compute yearly TOTAL precipitation (UPDATED: max -> sum)
  annual_total <- app(
    r_year,
    sum,
    na.rm = TRUE,
    filename = out_file,
    overwrite = TRUE
  )
  
  annual_files <- c(annual_files, out_file)
  
  rm(r_year, annual_total)
  gc()
}


# Load yearly total rasters
annual_total_stack <- rast(annual_files)

# Sum all annual totals
sum_annual_total <- app(
  annual_total_stack,
  sum,
  na.rm = TRUE,
  filename = "sum_annual_total.tif",
  overwrite = TRUE
)

# Average annual precipitation (inches/year)
avg_annual_precip <- sum_annual_total / n_years


# Save final NetCDF
writeRaster(
  avg_annual_precip,
  filename = "Precip_AvgAnnual_2045-2074_MIROC6.nc",
  overwrite = TRUE,
  filetype = "netcdf"
)

#File Clean up
file.remove(annual_files)
file.remove("sum_annual_total.tif")


###MIROC6 2015###

#File paths
future_file <- "MIROC6/precip/pr.MIROC6.ssp370.r1i1p1f1.2015-2044.LOCA2_CA_hybrid_v20230115.nc"


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
  
  # Output filename (UPDATED)
  out_file <- paste0("annual_total_", y, ".tif")
  
  # Compute yearly TOTAL precipitation (UPDATED: max -> sum)
  annual_total <- app(
    r_year,
    sum,
    na.rm = TRUE,
    filename = out_file,
    overwrite = TRUE
  )
  
  annual_files <- c(annual_files, out_file)
  
  rm(r_year, annual_total)
  gc()
}


# Load yearly total rasters
annual_total_stack <- rast(annual_files)

# Sum all annual totals
sum_annual_total <- app(
  annual_total_stack,
  sum,
  na.rm = TRUE,
  filename = "sum_annual_total.tif",
  overwrite = TRUE
)

# Average annual precipitation (inches/year)
avg_annual_precip <- sum_annual_total / n_years


# Save final NetCDF
writeRaster(
  avg_annual_precip,
  filename = "Precip_AvgAnnual_2015-2044_MIROC6.nc",
  overwrite = TRUE,
  filetype = "netcdf"
)

#File Clean up
file.remove(annual_files)
file.remove("sum_annual_total.tif")



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
  
  # Output filename (UPDATED)
  out_file <- paste0("annual_total_", y, ".tif")
  
  # Compute yearly TOTAL precipitation (UPDATED: max -> sum)
  annual_total <- app(
    r_year,
    sum,
    na.rm = TRUE,
    filename = out_file,
    overwrite = TRUE
  )
  
  annual_files <- c(annual_files, out_file)
  
  rm(r_year, annual_total)
  gc()
}


# Load yearly total rasters
annual_total_stack <- rast(annual_files)

# Sum all annual totals
sum_annual_total <- app(
  annual_total_stack,
  sum,
  na.rm = TRUE,
  filename = "sum_annual_total.tif",
  overwrite = TRUE
)

# Average annual precipitation (inches/year)
avg_annual_precip <- sum_annual_total / n_years


# Save final NetCDF
writeRaster(
  avg_annual_precip,
  filename = "Precip_AvgAnnual_2045-2074_MPIESM12.nc",
  overwrite = TRUE,
  filetype = "netcdf"
)

#File Clean up
file.remove(annual_files)
file.remove("sum_annual_total.tif")




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
  
  # Output filename (UPDATED)
  out_file <- paste0("annual_total_", y, ".tif")
  
  # Compute yearly TOTAL precipitation (UPDATED: max -> sum)
  annual_total <- app(
    r_year,
    sum,
    na.rm = TRUE,
    filename = out_file,
    overwrite = TRUE
  )
  
  annual_files <- c(annual_files, out_file)
  
  rm(r_year, annual_total)
  gc()
}


# Load yearly total rasters
annual_total_stack <- rast(annual_files)

# Sum all annual totals
sum_annual_total <- app(
  annual_total_stack,
  sum,
  na.rm = TRUE,
  filename = "sum_annual_total.tif",
  overwrite = TRUE
)

# Average annual precipitation (inches/year)
avg_annual_precip <- sum_annual_total / n_years


# Save final NetCDF
writeRaster(
  avg_annual_precip,
  filename = "Precip_AvgAnnual_2015-2044_MPIESM12.nc",
  overwrite = TRUE,
  filetype = "netcdf"
)

#File Clean up
file.remove(annual_files)
file.remove("sum_annual_total.tif")



### Reworked - Calculating average of 5 projects for 2045-2074 ###
library(terra)

# NetCDF files
nc_files <- c(
  "Precip_AvgAnnual_2045-2074_ACCESS.nc", 
  "Precip_AvgAnnual_2045-2074_ECEARTH.nc", 
  "Precip_AvgAnnual_2045-2074_FGOALS.nc",
  "Precip_AvgAnnual_2045-2074_MIROC6.nc",
  "Precip_AvgAnnual_2045-2074_MPIESM12.nc"
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
writeRaster(avg_raster, "Precip_AvgAnnual_2045-2074_GENUSE.tif", overwrite=TRUE)
writeCDF(avg_raster, "Precip_AvgAnnual_2045-2074_GENUSE.nc", overwrite=TRUE)


### Reworked - Calculating average of 5 projects for 2015-2044 ###
library(terra)

# NetCDF files
nc_files <- c(
  "Precip_AvgAnnual_2015-2044_ACCESS.nc", 
  "Precip_AvgAnnual_2015-2044_ECEARTH.nc", 
  "Precip_AvgAnnual_2015-2044_FGOALS.nc",
  "Precip_AvgAnnual_2015-2044_MIROC6.nc",
  "Precip_AvgAnnual_2015-2044_MPIESM12.nc"
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
writeRaster(avg_raster, "Precip_AvgAnnual_2015-2044_GENUSE.tif", overwrite=TRUE)
writeCDF(avg_raster, "Precip_AvgAnnual_2015-2044_GENUSE.nc", overwrite=TRUE)