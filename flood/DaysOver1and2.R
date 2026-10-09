---
  title: "Precip Days over 1 and 2 "
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


## Days over 1 and 2 for 2045-2074 ACCESS-CM2

#Counts the days where precipitation is over 1 inch and 2 inches during the time period

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

# Store filenames
gt1_files <- c()
gt2_files <- c()

for (y in unique_years) {
  
  cat("Processing year:", y, "\n")
  
  # Subset layers for that year
  r_year <- r_future[[which(years == y)]]
  
  # Output filenames
  out_gt1 <- paste0("days_gt1in_", y, ".tif")
  out_gt2 <- paste0("days_gt2in_", y, ".tif")
  
  # Count days > 1 inch
  days_gt1 <- app(
    r_year,
    fun = function(x) sum(x > 1, na.rm = TRUE),
    filename = out_gt1,
    overwrite = TRUE
  )
  
  # Count days > 2 inches
  days_gt2 <- app(
    r_year,
    fun = function(x) sum(x > 2, na.rm = TRUE),
    filename = out_gt2,
    overwrite = TRUE
  )
  
  gt1_files <- c(gt1_files, out_gt1)
  gt2_files <- c(gt2_files, out_gt2)
  
  rm(r_year, days_gt1, days_gt2)
  gc()
}


# Load total rasters
gt1_stack <- rast(gt1_files)
gt2_stack <- rast(gt2_files)

total_days_gt1 <- app(gt1_stack, sum, na.rm = TRUE,
                      filename = "total_days_gt1in.tif", overwrite = TRUE)

total_days_gt2 <- app(gt2_stack, sum, na.rm = TRUE,
                      filename = "total_days_gt2in.tif", overwrite = TRUE)


# Save final NetCDF
writeRaster(total_days_gt1,
            filename = "Precip_DaysOver1_2045-2074_ACCESS.nc",
            overwrite = TRUE,
            filetype = "netcdf")

writeRaster(total_days_gt2,
            filename = "Precip_DaysOver2_2045-2074_ACCESS.nc",
            overwrite = TRUE,
            filetype = "netcdf")

#File Clean up
file.remove(gt1_files)
file.remove(gt2_files)



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

# Convert units: kg m-2 s-1 -> inches/day
r_future <- r_future * 86400 / 25.4

# =========================
# COUNT TOTAL DAYS (30-YEAR TOTAL)
# =========================

total_days_gt1 <- app(
  r_future,
  fun = function(x) sum(x > 1, na.rm = TRUE),
  filename = "total_days_gt1in.tif",
  overwrite = TRUE
)

total_days_gt2 <- app(
  r_future,
  fun = function(x) sum(x > 2, na.rm = TRUE),
  filename = "total_days_gt2in.tif",
  overwrite = TRUE
)

# =========================
# SAVE FINAL OUTPUT
# =========================

writeRaster(total_days_gt1,
            filename = "Precip_DaysOver1_2015-2044_ACCESS.nc",
            overwrite = TRUE,
            filetype = "netcdf")

writeRaster(total_days_gt2,
            filename = "Precip_DaysOver2_2015-2044_ACCESS.nc",
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

# Convert units: kg m-2 s-1 -> inches/day
r_future <- r_future * 86400 / 25.4

# =========================
# COUNT TOTAL DAYS (30-YEAR TOTAL)
# =========================

total_days_gt1 <- app(
  r_future,
  fun = function(x) sum(x > 1, na.rm = TRUE),
  filename = "total_days_gt1in.tif",
  overwrite = TRUE
)

total_days_gt2 <- app(
  r_future,
  fun = function(x) sum(x > 2, na.rm = TRUE),
  filename = "total_days_gt2in.tif",
  overwrite = TRUE
)

# =========================
# SAVE FINAL OUTPUT
# =========================

writeRaster(total_days_gt1,
            filename = "Precip_DaysOver1_2045-2074_ECEARTH.nc",
            overwrite = TRUE,
            filetype = "netcdf")

writeRaster(total_days_gt2,
            filename = "Precip_DaysOver2_2045-2074_ECEARTH.nc",
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

# Convert units: kg m-2 s-1 -> inches/day
r_future <- r_future * 86400 / 25.4

# =========================
# COUNT TOTAL DAYS (30-YEAR TOTAL)
# =========================

total_days_gt1 <- app(
  r_future,
  fun = function(x) sum(x > 1, na.rm = TRUE),
  filename = "total_days_gt1in.tif",
  overwrite = TRUE
)

total_days_gt2 <- app(
  r_future,
  fun = function(x) sum(x > 2, na.rm = TRUE),
  filename = "total_days_gt2in.tif",
  overwrite = TRUE
)

# =========================
# SAVE FINAL OUTPUT
# =========================

writeRaster(total_days_gt1,
            filename = "Precip_DaysOver1_2015-2044_ECEARTH.nc",
            overwrite = TRUE,
            filetype = "netcdf")

writeRaster(total_days_gt2,
            filename = "Precip_DaysOver2_2015-2044_ECEARTH.nc",
            overwrite = TRUE,
            filetype = "netcdf")


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

# =========================
# COUNT TOTAL DAYS (30-YEAR TOTAL)
# =========================

total_days_gt1 <- app(
  r_future,
  fun = function(x) sum(x > 1, na.rm = TRUE),
  filename = "total_days_gt1in.tif",
  overwrite = TRUE
)

total_days_gt2 <- app(
  r_future,
  fun = function(x) sum(x > 2, na.rm = TRUE),
  filename = "total_days_gt2in.tif",
  overwrite = TRUE
)

# =========================
# SAVE FINAL OUTPUT
# =========================

writeRaster(total_days_gt1,
            filename = "Precip_DaysOver1_2045-2074_FGOALS.nc",
            overwrite = TRUE,
            filetype = "netcdf")

writeRaster(total_days_gt2,
            filename = "Precip_DaysOver2_2045-2074_FGOALS.nc",
            overwrite = TRUE,
            filetype = "netcdf")


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

# =========================
# COUNT TOTAL DAYS (30-YEAR TOTAL)
# =========================

total_days_gt1 <- app(
  r_future,
  fun = function(x) sum(x > 1, na.rm = TRUE),
  filename = "total_days_gt1in.tif",
  overwrite = TRUE
)

total_days_gt2 <- app(
  r_future,
  fun = function(x) sum(x > 2, na.rm = TRUE),
  filename = "total_days_gt2in.tif",
  overwrite = TRUE
)

# =========================
# SAVE FINAL OUTPUT
# =========================

writeRaster(total_days_gt1,
            filename = "Precip_DaysOver1_2015-2044_FGOALS.nc",
            overwrite = TRUE,
            filetype = "netcdf")

writeRaster(total_days_gt2,
            filename = "Precip_DaysOver2_2015-2044_FGOALS.nc",
            overwrite = TRUE,
            filetype = "netcdf")


###MIROC6 2045###

library(terra)
library(ncdf4)

# File paths
#hist_file <- "MIROC6/precip/pr.MIROC6.historical.r1i1p1f1.1950-2014.LOCA2_CA_hybrid_v20230115.nc"
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

# =========================
# COUNT TOTAL DAYS (30-YEAR TOTAL)
# =========================

total_days_gt1 <- app(
  r_future,
  fun = function(x) sum(x > 1, na.rm = TRUE),
  filename = "total_days_gt1in.tif",
  overwrite = TRUE
)

total_days_gt2 <- app(
  r_future,
  fun = function(x) sum(x > 2, na.rm = TRUE),
  filename = "total_days_gt2in.tif",
  overwrite = TRUE
)

# =========================
# SAVE FINAL OUTPUT
# =========================

writeRaster(total_days_gt1,
            filename = "Precip_DaysOver1_2045-2074_MIROC6.nc",
            overwrite = TRUE,
            filetype = "netcdf")

writeRaster(total_days_gt2,
            filename = "Precip_DaysOver2_2045-2074_MIROC6.nc",
            overwrite = TRUE,
            filetype = "netcdf")


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

# =========================
# COUNT TOTAL DAYS (30-YEAR TOTAL)
# =========================

total_days_gt1 <- app(
  r_future,
  fun = function(x) sum(x > 1, na.rm = TRUE),
  filename = "total_days_gt1in.tif",
  overwrite = TRUE
)

total_days_gt2 <- app(
  r_future,
  fun = function(x) sum(x > 2, na.rm = TRUE),
  filename = "total_days_gt2in.tif",
  overwrite = TRUE
)

# =========================
# SAVE FINAL OUTPUT
# =========================

writeRaster(total_days_gt1,
            filename = "Precip_DaysOver1_2015-2044_MIROC6.nc",
            overwrite = TRUE,
            filetype = "netcdf")

writeRaster(total_days_gt2,
            filename = "Precip_DaysOver2_2015-2044_MIROC6.nc",
            overwrite = TRUE,
            filetype = "netcdf")



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

# =========================
# COUNT TOTAL DAYS (30-YEAR TOTAL)
# =========================

total_days_gt1 <- app(
  r_future,
  fun = function(x) sum(x > 1, na.rm = TRUE),
  filename = "total_days_gt1in.tif",
  overwrite = TRUE
)

total_days_gt2 <- app(
  r_future,
  fun = function(x) sum(x > 2, na.rm = TRUE),
  filename = "total_days_gt2in.tif",
  overwrite = TRUE
)

# =========================
# SAVE FINAL OUTPUT
# =========================

writeRaster(total_days_gt1,
            filename = "Precip_DaysOver1_2045-2074_MPIESM12.nc",
            overwrite = TRUE,
            filetype = "netcdf")

writeRaster(total_days_gt2,
            filename = "Precip_DaysOver2_2045-2074_MPIESM12.nc",
            overwrite = TRUE,
            filetype = "netcdf")


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

# =========================
# COUNT TOTAL DAYS (30-YEAR TOTAL)
# =========================

total_days_gt1 <- app(
  r_future,
  fun = function(x) sum(x > 1, na.rm = TRUE),
  filename = "total_days_gt1in.tif",
  overwrite = TRUE
)

total_days_gt2 <- app(
  r_future,
  fun = function(x) sum(x > 2, na.rm = TRUE),
  filename = "total_days_gt2in.tif",
  overwrite = TRUE
)

# =========================
# SAVE FINAL OUTPUT
# =========================

writeRaster(total_days_gt1,
            filename = "Precip_DaysOver1_2015-2044_MPIESM12.nc",
            overwrite = TRUE,
            filetype = "netcdf")

writeRaster(total_days_gt2,
            filename = "Precip_DaysOver2_2015-2044_MPIESM12.nc",
            overwrite = TRUE,
            filetype = "netcdf")



### Calculating average for Days over 1 - 2045-2074 ###
library(terra)

# NetCDF files
nc_files <- c(
  "Precip_DaysOver1_2045-2074_ACCESS.nc", 
  "Precip_DaysOver1_2045-2074_ECEARTH.nc", 
  "Precip_DaysOver1_2045-2074_FGOALS.nc",
  "Precip_DaysOver1_2045-2074_MIROC6.nc",
  "Precip_DaysOver1_2045-2074_MPIESM12.nc"
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
writeRaster(avg_raster, "Precip_DaysOver1_2045-2074_GENUSE.tif", overwrite=TRUE)
writeCDF(avg_raster, "Precip_DaysOver1_2045-2074_GENUSE.nc", overwrite=TRUE)

### Calculating average for Days over 2 - 2045-2074 ###
library(terra)

# NetCDF files
nc_files <- c(
  "Precip_DaysOver2_2045-2074_ACCESS.nc", 
  "Precip_DaysOver2_2045-2074_ECEARTH.nc", 
  "Precip_DaysOver2_2045-2074_FGOALS.nc",
  "Precip_DaysOver2_2045-2074_MIROC6.nc",
  "Precip_DaysOver2_2045-2074_MPIESM12.nc"
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
writeRaster(avg_raster, "Precip_DaysOver2_2045-2074_GENUSE.tif", overwrite=TRUE)
writeCDF(avg_raster, "Precip_DaysOver2_2045-2074_GENUSE.nc", overwrite=TRUE)


### Calculating average for Days over 1 - 2015-2044 ###
library(terra)

# NetCDF files
nc_files <- c(
  "Precip_DaysOver1_2015-2044_ACCESS.nc", 
  "Precip_DaysOver1_2015-2044_ECEARTH.nc", 
  "Precip_DaysOver1_2015-2044_FGOALS.nc",
  "Precip_DaysOver1_2015-2044_MIROC6.nc",
  "Precip_DaysOver1_2015-2044_MPIESM12.nc"
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
writeRaster(avg_raster, "Precip_DaysOver1_2015-2044_GENUSE.tif", overwrite=TRUE)
writeCDF(avg_raster, "Precip_DaysOver1_2015-2044_GENUSE.nc", overwrite=TRUE)


### Calculating average for Days over 2 - 2015-2044 ###
library(terra)

# NetCDF files
nc_files <- c(
  "Precip_DaysOver2_2015-2044_ACCESS.nc", 
  "Precip_DaysOver2_2015-2044_ECEARTH.nc", 
  "Precip_DaysOver2_2015-2044_FGOALS.nc",
  "Precip_DaysOver2_2015-2044_MIROC6.nc",
  "Precip_DaysOver2_2015-2044_MPIESM12.nc"
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
writeRaster(avg_raster, "Precip_DaysOver2_2015-2044_GENUSE.tif", overwrite=TRUE)
writeCDF(avg_raster, "Precip_DaysOver2_2015-2044_GENUSE.nc", overwrite=TRUE)