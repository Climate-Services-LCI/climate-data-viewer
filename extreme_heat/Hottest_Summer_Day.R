---
  title: "VCP_EXHEAT_AnnualMaxTMAX"
output: html_document
date: "2026-01-07"
author: "Climate Services, CA Governor's Office of Land Use and Climate Innovation
editor_options: 
  chunk_output_type: console
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Extreme Heat Analysis for Vulnerable Communities Platform

## Data Description

This code utilizes downscaled climate data available from the Cal-Adapt Analytics Engine to generate an indicator of extreme heat for the Vulnerable Communities Platform (CA Governor's Office of Land Use and Climate Innovation). Specifically, we use downscaled tmaxdata from the five general use projections under SSP370 for two different time scales (2015-2044 and 2045-2074).

The data are available from the following links:

https://cadcat.s3.amazonaws.com/index.html#loca2/aaa-ca-hybrid/



#### (VCP 2050 timescale) ACCESS-CM2 ####

Identifies the maximum temperature max each year from 2045-2074 
and outputs the average of the maximums at each location in the grid.

```{r}
library(ncdf4)

# Path to NetCDF file
ncfile <- "ACCESS-CM2/tasmax.ACCESS-CM2.ssp370.r1i1p1f1.2045-2074.LOCA2_CA_hybrid_v20230115.nc"

# Open the NetCDF file
nc <- nc_open(ncfile)

# Extract coordinates and time
lon  <- ncvar_get(nc, "lon")
lat  <- ncvar_get(nc, "lat")
time <- ncvar_get(nc, "time")

# Convert NetCDF time to Date
dates <- as.Date("1900-01-01") + time

# Years of interest
years <- 2045:2074

# Array: lon × lat × year
annual_max_array <- array(NA, dim = c(length(lon), length(lat), length(years)))

# Loop over years
for (y in seq_along(years)) {
  year <- years[y]
  
  # JJA indices for this year
  year_indices <- which(
    format(dates, "%Y") == year &
    format(dates, "%m") %in% c("06", "07", "08")
)

# Skip if no JJA data (safety)
if (length(year_indices) == 0) next

# Read tasmax for all JJA days at once
daily_temps <- array(NA, dim = c(length(lon), length(lat), length(year_indices)))

for (i in seq_along(year_indices)) {
  daily_temps[,,i] <- ncvar_get(
    nc, "tasmax",
    start = c(1, 1, year_indices[i]),
    count = c(length(lon), length(lat), 1)
  )
}

# Annual JJA maximum (safe against all-NA)
annual_max_array[,,y] <-
  apply(daily_temps, c(1, 2), function(x) {
    if (all(is.na(x))) NA else max(x, na.rm = TRUE)
  })
}

# Close input NetCDF
nc_close(nc)

# FINAL RESULT: mean of annual JJA maxima (2045–2074)

mean_jja_max <- apply(
  annual_max_array, c(1, 2),
  function(x) if (all(is.na(x))) NA else mean(x, na.rm = TRUE)
)

#Convert final grid from Kelvin to Fahrenheit
mean_jja_max_F <- (mean_jja_max - 273.15) * 9/5 + 32


# WRITE OUTPUT TO NETCDF

# Define dimensions
lon_dim <- ncdim_def("lon", "degrees_east", lon)
lat_dim <- ncdim_def("lat", "degrees_north", lat)

#Define Variable in Fahrenheit
var_def <- ncvar_def(
  name  = "tasmax_jja_mean_F",
  units = "degF",
  dim   = list(lon_dim, lat_dim),
  missval = NA,
  longname = "Mean JJA maximum temperature (2045–2074, Fahrenheit)"
)

# Create output file
nc_out <- nc_create("VCP_EXHEAT_AnnualMaxTMAX_2045-2074_ACCESS.nc", var_def)

# Write Fahrenheit data
ncvar_put(nc_out, var_def, mean_jja_max_F)

# Close output NetCDF
nc_close(nc_out)

#### VCP current timescale ACCESS-CM2 #####

Identifies the maximum temperature max each year from 2015-2044
and outputs the average of the maximums at each location in the grid.

```{r}
library(ncdf4)

# Path to NetCDF file
ncfile <- "ACCESS-CM2/tasmax.ACCESS-CM2.ssp370.r1i1p1f1.2015-2044.LOCA2_CA_hybrid_v20230115.nc"

# Open the NetCDF file
nc <- nc_open(ncfile)

# Extract coordinates and time
lon  <- ncvar_get(nc, "lon")
lat  <- ncvar_get(nc, "lat")
time <- ncvar_get(nc, "time")

# Convert NetCDF time to Date
dates <- as.Date("1900-01-01") + time

# Years of interest
years <- 2015:2044

# Array: lon × lat × year
annual_max_array <- array(NA, dim = c(length(lon), length(lat), length(years)))

# Loop over years
for (y in seq_along(years)) {
  year <- years[y]
  
  # JJA indices for this year
  year_indices <- which(
    format(dates, "%Y") == year &
      format(dates, "%m") %in% c("06", "07", "08")
  )
  
  # Skip if no JJA data (safety)
  if (length(year_indices) == 0) next
  
  # Read tasmax for all JJA days at once
  daily_temps <- array(NA, dim = c(length(lon), length(lat), length(year_indices)))
  
  for (i in seq_along(year_indices)) {
    daily_temps[,,i] <- ncvar_get(
      nc, "tasmax",
      start = c(1, 1, year_indices[i]),
      count = c(length(lon), length(lat), 1)
    )
  }
  
  # Annual JJA maximum (safe against all-NA)
  annual_max_array[,,y] <-
    apply(daily_temps, c(1, 2), function(x) {
      if (all(is.na(x))) NA else max(x, na.rm = TRUE)
    })
}

# Close input NetCDF
nc_close(nc)

# FINAL RESULT: mean of annual JJA maxima

mean_jja_max <- apply(
  annual_max_array, c(1, 2),
  function(x) if (all(is.na(x))) NA else mean(x, na.rm = TRUE)
)

#Convert final grid from Kelvin to Fahrenheit
mean_jja_max_F <- (mean_jja_max - 273.15) * 9/5 + 32


# WRITE OUTPUT TO NETCDF

# Define dimensions
lon_dim <- ncdim_def("lon", "degrees_east", lon)
lat_dim <- ncdim_def("lat", "degrees_north", lat)

#Define Variable in Fahrenheit
var_def <- ncvar_def(
  name  = "tasmax_jja_mean_F",
  units = "degF",
  dim   = list(lon_dim, lat_dim),
  missval = NA,
  longname = "Mean JJA maximum temperature Fahrenheit)"
)

# Create output file
nc_out <- nc_create("VCP_EXHEAT_AnnualMaxTMAX_2015-2044_ACCESS.nc", var_def)

# Write Fahrenheit data
ncvar_put(nc_out, var_def, mean_jja_max_F)

# Close output NetCDF
nc_close(nc_out)

#### (VCP 2050 timescale) EC-EARTH ####

Identifies the maximum temperature max each year from 2045-2074
and outputs the average of the maximums at each location in the grid.

```{r}
library(ncdf4)

# Path to NetCDF file
ncfile <- "EC-Earth3/tasmax.EC-Earth3.ssp370.r1i1p1f1.2045-2074.LOCA2_CA_hybrid_v20230115.nc"

# Open the NetCDF file
nc <- nc_open(ncfile)

# Extract coordinates and time
lon  <- ncvar_get(nc, "lon")
lat  <- ncvar_get(nc, "lat")
time <- ncvar_get(nc, "time")

# Convert NetCDF time to Date
dates <- as.Date("1900-01-01") + time

# Years of interest
years <- 2045:2074

# Array: lon × lat × year
annual_max_array <- array(NA, dim = c(length(lon), length(lat), length(years)))

# Loop over years
for (y in seq_along(years)) {
  year <- years[y]
  
  # JJA indices for this year
  year_indices <- which(
    format(dates, "%Y") == year &
      format(dates, "%m") %in% c("06", "07", "08")
  )
  
  # Skip if no JJA data (safety)
  if (length(year_indices) == 0) next
  
  # Read tasmax for all JJA days at once
  daily_temps <- array(NA, dim = c(length(lon), length(lat), length(year_indices)))
  
  for (i in seq_along(year_indices)) {
    daily_temps[,,i] <- ncvar_get(
      nc, "tasmax",
      start = c(1, 1, year_indices[i]),
      count = c(length(lon), length(lat), 1)
    )
  }
  
  # Annual JJA maximum (safe against all-NA)
  annual_max_array[,,y] <-
    apply(daily_temps, c(1, 2), function(x) {
      if (all(is.na(x))) NA else max(x, na.rm = TRUE)
    })
}

# Close input NetCDF
nc_close(nc)

# FINAL RESULT: mean of annual JJA maxima 
mean_jja_max <- apply(
  annual_max_array, c(1, 2),
  function(x) if (all(is.na(x))) NA else mean(x, na.rm = TRUE)
)

#Convert final grid from Kelvin to Fahrenheit
mean_jja_max_F <- (mean_jja_max - 273.15) * 9/5 + 32


# WRITE OUTPUT TO NETCDF

# Define dimensions
lon_dim <- ncdim_def("lon", "degrees_east", lon)
lat_dim <- ncdim_def("lat", "degrees_north", lat)

#Define Variable in Fahrenheit
var_def <- ncvar_def(
  name  = "tasmax_jja_mean_F",
  units = "degF",
  dim   = list(lon_dim, lat_dim),
  missval = NA,
  longname = "Mean JJA maximum temperature Fahrenheit)"
)

# Create output file
nc_out <- nc_create("VCP_EXHEAT_AnnualMaxTMAX_2045-2074_ECEARTH.nc", var_def)

# Write Fahrenheit data
ncvar_put(nc_out, var_def, mean_jja_max_F)

# Close output NetCDF
nc_close(nc_out)

#### VCP current timescale EC-EARTH #####

Identifies the maximum temperature max each year from 2015-2044
and outputs the average of the maximums at each location in the grid.


```{r}
library(ncdf4)

# Path to NetCDF file
ncfile <- "EC-Earth3/tasmax.EC-Earth3.ssp370.r1i1p1f1.2015-2044.LOCA2_CA_hybrid_v20230115.nc"

# Open the NetCDF file
nc <- nc_open(ncfile)

# Extract coordinates and time
lon  <- ncvar_get(nc, "lon")
lat  <- ncvar_get(nc, "lat")
time <- ncvar_get(nc, "time")

# Convert NetCDF time to Date
dates <- as.Date("1900-01-01") + time

# Years of interest
years <- 2015:2044

# Array: lon × lat × year
annual_max_array <- array(NA, dim = c(length(lon), length(lat), length(years)))

# Loop over years
for (y in seq_along(years)) {
  year <- years[y]
  
  # JJA indices for this year
  year_indices <- which(
    format(dates, "%Y") == year &
      format(dates, "%m") %in% c("06", "07", "08")
  )
  
  # Skip if no JJA data (safety)
  if (length(year_indices) == 0) next
  
  # Read tasmax for all JJA days at once
  daily_temps <- array(NA, dim = c(length(lon), length(lat), length(year_indices)))
  
  for (i in seq_along(year_indices)) {
    daily_temps[,,i] <- ncvar_get(
      nc, "tasmax",
      start = c(1, 1, year_indices[i]),
      count = c(length(lon), length(lat), 1)
    )
  }
  
  # Annual JJA maximum (safe against all-NA)
  annual_max_array[,,y] <-
    apply(daily_temps, c(1, 2), function(x) {
      if (all(is.na(x))) NA else max(x, na.rm = TRUE)
    })
}

# Close input NetCDF
nc_close(nc)

# FINAL RESULT: mean of annual JJA maxima 

mean_jja_max <- apply(
  annual_max_array, c(1, 2),
  function(x) if (all(is.na(x))) NA else mean(x, na.rm = TRUE)
)

#Convert final grid from Kelvin to Fahrenheit
mean_jja_max_F <- (mean_jja_max - 273.15) * 9/5 + 32


# WRITE OUTPUT TO NETCDF

# Define dimensions
lon_dim <- ncdim_def("lon", "degrees_east", lon)
lat_dim <- ncdim_def("lat", "degrees_north", lat)

#Define Variable in Fahrenheit
var_def <- ncvar_def(
  name  = "tasmax_jja_mean_F",
  units = "degF",
  dim   = list(lon_dim, lat_dim),
  missval = NA,
  longname = "Mean JJA maximum temperature Fahrenheit)"
)

# Create output file
nc_out <- nc_create("VCP_EXHEAT_AnnualMaxTMAX_2015-2044_ECEARTH.nc", var_def)

# Write Fahrenheit data
ncvar_put(nc_out, var_def, mean_jja_max_F)

# Close output NetCDF
nc_close(nc_out)


#### (VCP 2050 timescale) FGOALS ####

Identifies the maximum temperature max each year from 2045-2074
and outputs the average of the maximums at each location in the grid.

```{r}
library(ncdf4)

# Path to NetCDF file
ncfile <- "FGOALS-g3/tasmax.FGOALS-g3.ssp370.r1i1p1f1.2045-2074.LOCA2_CA_hybrid_v20230115.nc"

# Open the NetCDF file
nc <- nc_open(ncfile)

# Extract coordinates and time
lon  <- ncvar_get(nc, "lon")
lat  <- ncvar_get(nc, "lat")
time <- ncvar_get(nc, "time")

# Convert NetCDF time to Date
dates <- as.Date("1900-01-01") + time

# Years of interest
years <- 2045:2074

# Array: lon × lat × year
annual_max_array <- array(NA, dim = c(length(lon), length(lat), length(years)))

# Loop over years
for (y in seq_along(years)) {
  year <- years[y]
  
  # JJA indices for this year
  year_indices <- which(
    format(dates, "%Y") == year &
      format(dates, "%m") %in% c("06", "07", "08")
  )
  
  # Skip if no JJA data (safety)
  if (length(year_indices) == 0) next
  
  # Read tasmax for all JJA days at once
  daily_temps <- array(NA, dim = c(length(lon), length(lat), length(year_indices)))
  
  for (i in seq_along(year_indices)) {
    daily_temps[,,i] <- ncvar_get(
      nc, "tasmax",
      start = c(1, 1, year_indices[i]),
      count = c(length(lon), length(lat), 1)
    )
  }
  
  # Annual JJA maximum (safe against all-NA)
  annual_max_array[,,y] <-
    apply(daily_temps, c(1, 2), function(x) {
      if (all(is.na(x))) NA else max(x, na.rm = TRUE)
    })
}

# Close input NetCDF
nc_close(nc)

# FINAL RESULT: mean of annual JJA maxima 

mean_jja_max <- apply(
  annual_max_array, c(1, 2),
  function(x) if (all(is.na(x))) NA else mean(x, na.rm = TRUE)
)

#Convert final grid from Kelvin to Fahrenheit
mean_jja_max_F <- (mean_jja_max - 273.15) * 9/5 + 32


# WRITE OUTPUT TO NETCDF

# Define dimensions
lon_dim <- ncdim_def("lon", "degrees_east", lon)
lat_dim <- ncdim_def("lat", "degrees_north", lat)

#Define Variable in Fahrenheit
var_def <- ncvar_def(
  name  = "tasmax_jja_mean_F",
  units = "degF",
  dim   = list(lon_dim, lat_dim),
  missval = NA,
  longname = "Mean JJA maximum temperature Fahrenheit)"
)

# Create output file
nc_out <- nc_create("VCP_EXHEAT_AnnualMaxTMAX_2045-2074_FGOALS.nc", var_def)

# Write Fahrenheit data
ncvar_put(nc_out, var_def, mean_jja_max_F)

# Close output NetCDF
nc_close(nc_out)




#### VCP current timescale FGOALS ####

Identifies the maximum temperature max each year from 2015-2044
and outputs the average of the maximums at each location in the grid.

```{r}
library(ncdf4)

# Path to NetCDF file
ncfile <- "FGOALS-g3/tasmax.FGOALS-g3.ssp370.r1i1p1f1.2015-2044.LOCA2_CA_hybrid_v20230115.nc"

# Open the NetCDF file
nc <- nc_open(ncfile)

# Extract coordinates and time
lon  <- ncvar_get(nc, "lon")
lat  <- ncvar_get(nc, "lat")
time <- ncvar_get(nc, "time")

# Convert NetCDF time to Date
dates <- as.Date("1900-01-01") + time

# Years of interest
years <- 2015:2044

# Array: lon × lat × year
annual_max_array <- array(NA, dim = c(length(lon), length(lat), length(years)))

# Loop over years
for (y in seq_along(years)) {
  year <- years[y]
  
  # JJA indices for this year
  year_indices <- which(
    format(dates, "%Y") == year &
      format(dates, "%m") %in% c("06", "07", "08")
  )
  
  # Skip if no JJA data (safety)
  if (length(year_indices) == 0) next
  
  # Read tasmax for all JJA days at once
  daily_temps <- array(NA, dim = c(length(lon), length(lat), length(year_indices)))
  
  for (i in seq_along(year_indices)) {
    daily_temps[,,i] <- ncvar_get(
      nc, "tasmax",
      start = c(1, 1, year_indices[i]),
      count = c(length(lon), length(lat), 1)
    )
  }
  
  # Annual JJA maximum (safe against all-NA)
  annual_max_array[,,y] <-
    apply(daily_temps, c(1, 2), function(x) {
      if (all(is.na(x))) NA else max(x, na.rm = TRUE)
    })
}

# Close input NetCDF
nc_close(nc)

# FINAL RESULT: mean of annual JJA maxima 

mean_jja_max <- apply(
  annual_max_array, c(1, 2),
  function(x) if (all(is.na(x))) NA else mean(x, na.rm = TRUE)
)

#Convert final grid from Kelvin to Fahrenheit
mean_jja_max_F <- (mean_jja_max - 273.15) * 9/5 + 32


# WRITE OUTPUT TO NETCDF

# Define dimensions
lon_dim <- ncdim_def("lon", "degrees_east", lon)
lat_dim <- ncdim_def("lat", "degrees_north", lat)

#Define Variable in Fahrenheit
var_def <- ncvar_def(
  name  = "tasmax_jja_mean_F",
  units = "degF",
  dim   = list(lon_dim, lat_dim),
  missval = NA,
  longname = "Mean JJA maximum temperature Fahrenheit)"
)

# Create output file
nc_out <- nc_create("VCP_EXHEAT_AnnualMaxTMAX_2015-2044_FGOALS.nc", var_def)

# Write Fahrenheit data
ncvar_put(nc_out, var_def, mean_jja_max_F)

# Close output NetCDF
nc_close(nc_out)



#### (VCP 2050 timescale) MIROC6 ####

Identifies the maximum temperature max each year from 2045-2074
and outputs the average of the maximums at each location in the grid.

```{r}
library(ncdf4)

# Path to NetCDF file
ncfile <- "MIROC6/tasmax.MIROC6.ssp370.r1i1p1f1.2045-2074.LOCA2_CA_hybrid_v20230115.nc"

# Open the NetCDF file
nc <- nc_open(ncfile)

# Extract coordinates and time
lon  <- ncvar_get(nc, "lon")
lat  <- ncvar_get(nc, "lat")
time <- ncvar_get(nc, "time")

# Convert NetCDF time to Date
dates <- as.Date("1900-01-01") + time

# Years of interest
years <- 2045:2074

# Array: lon × lat × year
annual_max_array <- array(NA, dim = c(length(lon), length(lat), length(years)))

# Loop over years
for (y in seq_along(years)) {
  year <- years[y]
  
  # JJA indices for this year
  year_indices <- which(
    format(dates, "%Y") == year &
      format(dates, "%m") %in% c("06", "07", "08")
  )
  
  # Skip if no JJA data (safety)
  if (length(year_indices) == 0) next
  
  # Read tasmax for all JJA days at once
  daily_temps <- array(NA, dim = c(length(lon), length(lat), length(year_indices)))
  
  for (i in seq_along(year_indices)) {
    daily_temps[,,i] <- ncvar_get(
      nc, "tasmax",
      start = c(1, 1, year_indices[i]),
      count = c(length(lon), length(lat), 1)
    )
  }
  
  # Annual JJA maximum (safe against all-NA)
  annual_max_array[,,y] <-
    apply(daily_temps, c(1, 2), function(x) {
      if (all(is.na(x))) NA else max(x, na.rm = TRUE)
    })
}

# Close input NetCDF
nc_close(nc)

# FINAL RESULT: mean of annual JJA maxima 

mean_jja_max <- apply(
  annual_max_array, c(1, 2),
  function(x) if (all(is.na(x))) NA else mean(x, na.rm = TRUE)
)

#Convert final grid from Kelvin to Fahrenheit
mean_jja_max_F <- (mean_jja_max - 273.15) * 9/5 + 32


# WRITE OUTPUT TO NETCDF

# Define dimensions
lon_dim <- ncdim_def("lon", "degrees_east", lon)
lat_dim <- ncdim_def("lat", "degrees_north", lat)

#Define Variable in Fahrenheit
var_def <- ncvar_def(
  name  = "tasmax_jja_mean_F",
  units = "degF",
  dim   = list(lon_dim, lat_dim),
  missval = NA,
  longname = "Mean JJA maximum temperature Fahrenheit)"
)

# Create output file
nc_out <- nc_create("VCP_EXHEAT_AnnualMaxTMAX_2045-2074_MIROC6.nc", var_def)

# Write Fahrenheit data
ncvar_put(nc_out, var_def, mean_jja_max_F)

# Close output NetCDF
nc_close(nc_out)
```


#### VCP current timescale MIROC6 ####

Identifies the maximum temperature max each year from 2015-2044
and outputs the average of the maximums at each location in the grid.

```{r}
library(ncdf4)

# Path to NetCDF file
ncfile <- "MIROC6/tasmax.MIROC6.ssp370.r1i1p1f1.2015-2044.LOCA2_CA_hybrid_v20230115.nc"

# Open the NetCDF file
nc <- nc_open(ncfile)

# Extract coordinates and time
lon  <- ncvar_get(nc, "lon")
lat  <- ncvar_get(nc, "lat")
time <- ncvar_get(nc, "time")

# Convert NetCDF time to Date
dates <- as.Date("1900-01-01") + time

# Years of interest
years <- 2015:2044

# Array: lon × lat × year
annual_max_array <- array(NA, dim = c(length(lon), length(lat), length(years)))

# Loop over years
for (y in seq_along(years)) {
  year <- years[y]
  
  # JJA indices for this year
  year_indices <- which(
    format(dates, "%Y") == year &
      format(dates, "%m") %in% c("06", "07", "08")
  )
  
  # Skip if no JJA data (safety)
  if (length(year_indices) == 0) next
  
  # Read tasmax for all JJA days at once
  daily_temps <- array(NA, dim = c(length(lon), length(lat), length(year_indices)))
  
  for (i in seq_along(year_indices)) {
    daily_temps[,,i] <- ncvar_get(
      nc, "tasmax",
      start = c(1, 1, year_indices[i]),
      count = c(length(lon), length(lat), 1)
    )
  }
  
  # Annual JJA maximum (safe against all-NA)
  annual_max_array[,,y] <-
    apply(daily_temps, c(1, 2), function(x) {
      if (all(is.na(x))) NA else max(x, na.rm = TRUE)
    })
}

# Close input NetCDF
nc_close(nc)

# FINAL RESULT: mean of annual JJA maxima 

mean_jja_max <- apply(
  annual_max_array, c(1, 2),
  function(x) if (all(is.na(x))) NA else mean(x, na.rm = TRUE)
)

#Convert final grid from Kelvin to Fahrenheit
mean_jja_max_F <- (mean_jja_max - 273.15) * 9/5 + 32


# WRITE OUTPUT TO NETCDF

# Define dimensions
lon_dim <- ncdim_def("lon", "degrees_east", lon)
lat_dim <- ncdim_def("lat", "degrees_north", lat)

#Define Variable in Fahrenheit
var_def <- ncvar_def(
  name  = "tasmax_jja_mean_F",
  units = "degF",
  dim   = list(lon_dim, lat_dim),
  missval = NA,
  longname = "Mean JJA maximum temperature Fahrenheit)"
)

# Create output file
nc_out <- nc_create("VCP_EXHEAT_AnnualMaxTMAX_2015-2044_MIROC6.nc", var_def)

# Write Fahrenheit data
ncvar_put(nc_out, var_def, mean_jja_max_F)

# Close output NetCDF
nc_close(nc_out)

#### (VCP 2050 timescale) MPI-ESM1-2-HR ####

Identifies the maximum temperature max each year from 2045-2074
and outputs the average of the maximums at each location in the grid.

```{r}
library(ncdf4)

# Path to NetCDF file
ncfile <- "MPI-ESM1-2-HR/tasmax.MPI-ESM1-2-HR.ssp370.r3i1p1f1.2045-2074.LOCA2_CA_hybrid_v20230115.nc"

# Open the NetCDF file
nc <- nc_open(ncfile)

# Extract coordinates and time
lon  <- ncvar_get(nc, "lon")
lat  <- ncvar_get(nc, "lat")
time <- ncvar_get(nc, "time")

# Convert NetCDF time to Date
dates <- as.Date("1900-01-01") + time

# Years of interest
years <- 2045:2074

# Array: lon × lat × year
annual_max_array <- array(NA, dim = c(length(lon), length(lat), length(years)))

# Loop over years
for (y in seq_along(years)) {
  year <- years[y]
  
  # JJA indices for this year
  year_indices <- which(
    format(dates, "%Y") == year &
      format(dates, "%m") %in% c("06", "07", "08")
  )
  
  # Skip if no JJA data (safety)
  if (length(year_indices) == 0) next
  
  # Read tasmax for all JJA days at once
  daily_temps <- array(NA, dim = c(length(lon), length(lat), length(year_indices)))
  
  for (i in seq_along(year_indices)) {
    daily_temps[,,i] <- ncvar_get(
      nc, "tasmax",
      start = c(1, 1, year_indices[i]),
      count = c(length(lon), length(lat), 1)
    )
  }
  
  # Annual JJA maximum (safe against all-NA)
  annual_max_array[,,y] <-
    apply(daily_temps, c(1, 2), function(x) {
      if (all(is.na(x))) NA else max(x, na.rm = TRUE)
    })
}

# Close input NetCDF
nc_close(nc)

# FINAL RESULT: mean of annual JJA maxima 

mean_jja_max <- apply(
  annual_max_array, c(1, 2),
  function(x) if (all(is.na(x))) NA else mean(x, na.rm = TRUE)
)

#Convert final grid from Kelvin to Fahrenheit
mean_jja_max_F <- (mean_jja_max - 273.15) * 9/5 + 32


# WRITE OUTPUT TO NETCDF

# Define dimensions
lon_dim <- ncdim_def("lon", "degrees_east", lon)
lat_dim <- ncdim_def("lat", "degrees_north", lat)

#Define Variable in Fahrenheit
var_def <- ncvar_def(
  name  = "tasmax_jja_mean_F",
  units = "degF",
  dim   = list(lon_dim, lat_dim),
  missval = NA,
  longname = "Mean JJA maximum temperature Fahrenheit)"
)

# Create output file
nc_out <- nc_create("VCP_EXHEAT_AnnualMaxTMAX_2045-2074_MPI-ESM1-2-HR.nc", var_def)

# Write Fahrenheit data
ncvar_put(nc_out, var_def, mean_jja_max_F)

# Close output NetCDF
nc_close(nc_out)

#### VCP current timescale MPI-ESM1-2-HR ####

Identifies the maximum temperature max each year from 2015-2044
and outputs the average of the maximums at each location in the grid.

```{r}
library(ncdf4)

# Path to NetCDF file
ncfile <- "MPI-ESM1-2-HR/tasmax.MPI-ESM1-2-HR.ssp370.r3i1p1f1.2015-2044.LOCA2_CA_hybrid_v20230115.nc"

# Open the NetCDF file
nc <- nc_open(ncfile)

# Extract coordinates and time
lon  <- ncvar_get(nc, "lon")
lat  <- ncvar_get(nc, "lat")
time <- ncvar_get(nc, "time")

# Convert NetCDF time to Date
dates <- as.Date("1900-01-01") + time

# Years of interest
years <- 2015:2044

# Array: lon × lat × year
annual_max_array <- array(NA, dim = c(length(lon), length(lat), length(years)))

# Loop over years
for (y in seq_along(years)) {
  year <- years[y]
  
  # JJA indices for this year
  year_indices <- which(
    format(dates, "%Y") == year &
      format(dates, "%m") %in% c("06", "07", "08")
  )
  
  # Skip if no JJA data (safety)
  if (length(year_indices) == 0) next
  
  # Read tasmax for all JJA days at once
  daily_temps <- array(NA, dim = c(length(lon), length(lat), length(year_indices)))
  
  for (i in seq_along(year_indices)) {
    daily_temps[,,i] <- ncvar_get(
      nc, "tasmax",
      start = c(1, 1, year_indices[i]),
      count = c(length(lon), length(lat), 1)
    )
  }
  
  # Annual JJA maximum (safe against all-NA)
  annual_max_array[,,y] <-
    apply(daily_temps, c(1, 2), function(x) {
      if (all(is.na(x))) NA else max(x, na.rm = TRUE)
    })
}

# Close input NetCDF
nc_close(nc)

# FINAL RESULT: mean of annual JJA maxima 

mean_jja_max <- apply(
  annual_max_array, c(1, 2),
  function(x) if (all(is.na(x))) NA else mean(x, na.rm = TRUE)
)

#Convert final grid from Kelvin to Fahrenheit
mean_jja_max_F <- (mean_jja_max - 273.15) * 9/5 + 32


# WRITE OUTPUT TO NETCDF

# Define dimensions
lon_dim <- ncdim_def("lon", "degrees_east", lon)
lat_dim <- ncdim_def("lat", "degrees_north", lat)

#Define Variable in Fahrenheit
var_def <- ncvar_def(
  name  = "tasmax_jja_mean_F",
  units = "degF",
  dim   = list(lon_dim, lat_dim),
  missval = NA,
  longname = "Mean JJA maximum temperature Fahrenheit)"
)

# Create output file
nc_out <- nc_create("VCP_EXHEAT_AnnualMaxTMAX_2015-2044_MPI-ESM1-2-HR.nc", var_def)

# Write Fahrenheit data
ncvar_put(nc_out, var_def, mean_jja_max_F)

# Close output NetCDF
nc_close(nc_out)

#### Calculating average of the 5 general use projections 2045-2074 ####

```{r}
library(stars)

# Define the NetCDF file paths
nc_files <- c(
  "VCP_EXHEAT_AnnualMaxTMAX_2045-2074_ACCESS.nc", 
  "VCP_EXHEAT_AnnualMaxTMAX_2045-2074_ECEARTH.nc", 
  "VCP_EXHEAT_AnnualMaxTMAX_2045-2074_FGOALS.nc",
  "VCP_EXHEAT_AnnualMaxTMAX_2045-2074_MIROC6.nc",
  "VCP_EXHEAT_AnnualMaxTMAX_2045-2074_MPI-ESM1-2-HR.nc"
)

# Load all files as stars objects
stars_list <- lapply(nc_files, read_stars)

# Optional: check variable names
# print(names(stars_list[[1]]))  # should show something like "mean_jja_max"

# Optional: ensure all grids match
stopifnot(all(sapply(stars_list, function(x) all.equal(st_dimensions(x), st_dimensions(stars_list[[1]])))))

# Combine along a new "model" dimension
combined <- do.call(c, c(stars_list, along = "model"))

# Average across "model" (i.e., average exceedance at each grid cell)
averaged_data <- st_apply(combined, MARGIN = setdiff(names(st_dimensions(combined)), "model"), FUN = mean, na.rm = TRUE)

# Save the averaged result as a new NetCDF file
write_stars(averaged_data, "VCP_EXHEAT_AnnualMaxTMAX_2045-2074_GENUSE.nc")

```
## NetCDF to Geotiff
# Annual Max TMAX

r <- rast(
  "VCP_EXHEAT_AnnualMaxTMAX_2045-2074_GENUSE.nc",
  subds = "tasmax_jja_mean_F"
)


crs(r)

#crs(r) <- "EPSG:4326"

writeRaster(r, "AnnualMaxTMAX_2045-2074.tif", overwrite = TRUE)



## Calculating average of the 5 general use projections 2015-2044

```{r}
library(stars)

# Define the NetCDF file paths
nc_files <- c(
  "VCP_EXHEAT_AnnualMaxTMAX_2015-2044_ACCESS.nc", 
  "VCP_EXHEAT_AnnualMaxTMAX_2015-2044_ECEARTH.nc", 
  "VCP_EXHEAT_AnnualMaxTMAX_2015-2044_FGOALS.nc",
  "VCP_EXHEAT_AnnualMaxTMAX_2015-2044_MIROC6.nc",
  "VCP_EXHEAT_AnnualMaxTMAX_2015-2044_MPI-ESM1-2-HR.nc"
)

# Load all files as stars objects
stars_list <- lapply(nc_files, read_stars)

# Optional: check variable names
# print(names(stars_list[[1]]))  # should show something like "mean_jja_max"

# Optional: ensure all grids match
stopifnot(all(sapply(stars_list, function(x) all.equal(st_dimensions(x), st_dimensions(stars_list[[1]])))))

# Combine along a new "model" dimension
combined <- do.call(c, c(stars_list, along = "model"))

# Average across "model" (i.e., average exceedance at each grid cell)
averaged_data <- st_apply(combined, MARGIN = setdiff(names(st_dimensions(combined)), "model"), FUN = mean, na.rm = TRUE)

# Save the averaged result as a new NetCDF file
write_stars(averaged_data, "VCP_EXHEAT_AnnualMaxTMAX_2015-2044_GENUSE.nc")
```{r}

```
## NetCDF to Geotiff
# Annual Max TMAX

r <- rast(
  "VCP_EXHEAT_AnnualMaxTMAX_2015-2044_GENUSE.nc",
  subds = "tasmax_jja_mean_F"
)


crs(r)

#crs(r) <- "EPSG:4326"

writeRaster(r, "AnnualMaxTMAX_2015-2044.tif", overwrite = TRUE)


