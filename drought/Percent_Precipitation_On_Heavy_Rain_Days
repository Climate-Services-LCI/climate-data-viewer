---
title: "Percent Precipitation from Heavy Rain Days (Annual)"
output: html_document
date: "2026-07-03"
editor_options: 
  chunk_output_type: console
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Percent Precipitation from Heavy Rain Days for Vulnerable Communities Platform

## Data Description

This code utilizes downscaled climate data available from the Cal-Adapt Analytics Engine to generate an indicator of precipitation intensity for the Vulnerable Communities Platform (CA Governor's Office of Land Use and Climate Innovation). Specifically, we use downscaled daily precipitation (pr) data from the five general use projections under SSP370 for two different time scales (2015-2044 and 2045-2074) and a modeled historical baseline data set for pr from 1950-2014.

**Definition:** the percentage contribution to total precipitation during the selected time frame from heavy rain days, where a heavy rain day is defined as a day with precipitation over the 95th historical (1950-2014) percentile precipitation for that grid location. All days of the year are used (no seasonal filtering). The 95th percentile threshold is computed from historical wet days (precipitation ≥ 1 mm) only, following standard climate extremes index convention.

All precipitation values are converted to inches of precipitation per day at each grid cell. Native LOCA2 `pr` files are assumed to be a flux in `kg m-2 s-1`; each script chunk prints the file's actual `units` attribute the first time it opens a file so this assumption can be verified against your data before trusting the output.

The data are available from the following link:

https://cadcat.s3.amazonaws.com/index.html#loca2/aaa-ca-hybrid/

---


## Percent precipitation from heavy rain days, full year, 2045-2074 (2050 timescale) ACCESS-CM2

Calculates the percentage of total annual precipitation contributed by heavy rain days (days exceeding the 1950-2014 historical 95th percentile) for each grid location, for 2045-2074.

```{r}
library(ncdf4)
setwd("D:/Climate_Data/")

# File paths
historical_ncfile <- "ACCESSCM2/precip/pr.ACCESS-CM2.historical.r1i1p1f1.1950-2014.LOCA2_CA_hybrid_v20230115.nc"
future_ncfile     <- "ACCESSCM2/precip/pr.ACCESS-CM2.ssp370.r1i1p1f1.2045-2074.LOCA2_CA_hybrid_v20230115.nc"

# Native units are assumed to be kg m-2 s-1 (a flux): 1 kg/m2 = 1 mm water depth,
# so *86400 (seconds/day) -> mm/day, then /25.4 -> inches/day
FLUX_TO_INCHES <- 86400 / 25.4

# Wet-day threshold for percentile calc (1 mm, in inches)
WET_DAY_IN <- 1 / 25.4

# Open historical precip NetCDF
nc_historical <- nc_open(historical_ncfile)

# Extract coordinates and time
lon             <- ncvar_get(nc_historical, "lon")
lat             <- ncvar_get(nc_historical, "lat")
time_historical <- ncvar_get(nc_historical, "time")

# Verify the FLUX_TO_INCHES assumption against the actual file metadata
print(ncatt_get(nc_historical, "pr", "units")$value)

# Convert time units to actual dates
time_origin      <- as.Date("1900-01-01")
dates_historical <- time_origin + time_historical

# Use all days of the year (no seasonal filtering)
day_indices_historical <- seq_along(dates_historical)

# --- Compute the 95th percentile threshold (from wet days) per grid cell ---
# Processed in latitude bands to keep memory manageable (percentiles require the
# full distribution per cell, unlike the running-sum approach used for tmax means).
# Using all ~65 years of daily data (vs. JJA-only) roughly quadruples the size of
# each band read, so lat_chunk_size is set lower here than a JJA-only version would need.
lat_chunk_size <- 10
threshold_95 <- array(NA_real_, dim = c(length(lon), length(lat)))

for (lat_start in seq(1, length(lat), by = lat_chunk_size)) {

    lat_count <- min(lat_chunk_size, length(lat) - lat_start + 1)

    band <- array(NA_real_, dim = c(length(lon), lat_count, length(day_indices_historical)))

    for (i in seq_along(day_indices_historical)) {
        day_slice <- ncvar_get(
            nc_historical,
            "pr",
            start = c(1, lat_start, day_indices_historical[i]),
            count = c(length(lon), lat_count, 1)
        )
        band[, , i] <- day_slice * FLUX_TO_INCHES
    }

    threshold_95[, lat_start:(lat_start + lat_count - 1)] <- apply(band, c(1, 2), function(v) {
        wet_vals <- v[!is.na(v) & v >= WET_DAY_IN]
        if (length(wet_vals) > 0) quantile(wet_vals, 0.95, na.rm = TRUE) else NA_real_
    })

    rm(band)
    gc()
}

# Close historical NetCDF
nc_close(nc_historical)

# --- Apply threshold to the target period, compute % contribution from heavy days ---
nc_future <- nc_open(future_ncfile)

time_future  <- ncvar_get(nc_future, "time")
dates_future <- time_origin + time_future

# Use all days of the year (no seasonal filtering)
day_indices_future <- seq_along(dates_future)

total_sum <- array(0, dim = c(length(lon), length(lat)))
heavy_sum <- array(0, dim = c(length(lon), length(lat)))

count <- 0

for (i in seq_along(day_indices_future)) {

    day <- ncvar_get(
        nc_future,
        "pr",
        start = c(1, 1, day_indices_future[i]),
        count = c(length(lon), length(lat), 1)
    ) * FLUX_TO_INCHES

    is_heavy <- !is.na(day) & !is.na(threshold_95) & (day > threshold_95)

    total_sum <- total_sum + ifelse(is.na(day), 0, day)
    heavy_sum <- heavy_sum + ifelse(is_heavy, day, 0)

    count <- count + 1

    rm(day, is_heavy)

    if (i %% 100 == 0) gc()
}

# Close future NetCDF
nc_close(nc_future)

# Percent of total precipitation contributed by heavy rain days
pct_heavy_precip <- (heavy_sum / total_sum) * 100
pct_heavy_precip[total_sum == 0] <- NA

# Define NetCDF dimensions
lon_dim <- ncdim_def("lon", "degrees_east",  lon)
lat_dim <- ncdim_def("lat", "degrees_north", lat)

# Define the output variable
pct_heavy_var <- ncvar_def("pct_heavy_precip", "%", list(lon_dim, lat_dim), missval = NA, prec = "double")

# Create the output NetCDF file
output_ncfile <- "VCP_DROUGHT_PRECIP95_PctHeavy_2045-2074_ACCESS.nc"
nc_new <- nc_create(output_ncfile, list(pct_heavy_var))

# Write data to the NetCDF file
ncvar_put(nc_new, "pct_heavy_precip", pct_heavy_precip)

# Add coordinate variables
ncvar_put(nc_new, "lon", lon)
ncvar_put(nc_new, "lat", lat)

# Close the NetCDF file
nc_close(nc_new)

rm(
    threshold_95,
    total_sum,
    heavy_sum,
    pct_heavy_precip,
    lon,
    lat
)

gc()

print("Percent precip from heavy rain days (annual) saved to NetCDF file.")
```

---


## Percent precipitation from heavy rain days, full year, 2045-2074 (2050 timescale) EC-Earth3

Calculates the percentage of total annual precipitation contributed by heavy rain days (days exceeding the 1950-2014 historical 95th percentile) for each grid location, for 2045-2074.

```{r}
library(ncdf4)
setwd("D:/Climate_Data/")

# File paths
historical_ncfile <- "ECEARTH/precip/pr.EC-Earth3.historical.r1i1p1f1.1950-2014.LOCA2_CA_hybrid_v20230115.nc"
future_ncfile     <- "ECEARTH/precip/pr.EC-Earth3.ssp370.r1i1p1f1.2045-2074.LOCA2_CA_hybrid_v20230115.nc"

# Native units are assumed to be kg m-2 s-1 (a flux): 1 kg/m2 = 1 mm water depth,
# so *86400 (seconds/day) -> mm/day, then /25.4 -> inches/day
FLUX_TO_INCHES <- 86400 / 25.4

# Wet-day threshold for percentile calc (1 mm, in inches)
WET_DAY_IN <- 1 / 25.4

# Open historical precip NetCDF
nc_historical <- nc_open(historical_ncfile)

# Extract coordinates and time
lon             <- ncvar_get(nc_historical, "lon")
lat             <- ncvar_get(nc_historical, "lat")
time_historical <- ncvar_get(nc_historical, "time")

# Verify the FLUX_TO_INCHES assumption against the actual file metadata
print(ncatt_get(nc_historical, "pr", "units")$value)

# Convert time units to actual dates
time_origin      <- as.Date("1900-01-01")
dates_historical <- time_origin + time_historical

# Use all days of the year (no seasonal filtering)
day_indices_historical <- seq_along(dates_historical)

# --- Compute the 95th percentile threshold (from wet days) per grid cell ---
# Processed in latitude bands to keep memory manageable (percentiles require the
# full distribution per cell, unlike the running-sum approach used for tmax means).
# Using all ~65 years of daily data (vs. JJA-only) roughly quadruples the size of
# each band read, so lat_chunk_size is set lower here than a JJA-only version would need.
lat_chunk_size <- 10
threshold_95 <- array(NA_real_, dim = c(length(lon), length(lat)))

for (lat_start in seq(1, length(lat), by = lat_chunk_size)) {

    lat_count <- min(lat_chunk_size, length(lat) - lat_start + 1)

    band <- array(NA_real_, dim = c(length(lon), lat_count, length(day_indices_historical)))

    for (i in seq_along(day_indices_historical)) {
        day_slice <- ncvar_get(
            nc_historical,
            "pr",
            start = c(1, lat_start, day_indices_historical[i]),
            count = c(length(lon), lat_count, 1)
        )
        band[, , i] <- day_slice * FLUX_TO_INCHES
    }

    threshold_95[, lat_start:(lat_start + lat_count - 1)] <- apply(band, c(1, 2), function(v) {
        wet_vals <- v[!is.na(v) & v >= WET_DAY_IN]
        if (length(wet_vals) > 0) quantile(wet_vals, 0.95, na.rm = TRUE) else NA_real_
    })

    rm(band)
    gc()
}

# Close historical NetCDF
nc_close(nc_historical)

# --- Apply threshold to the target period, compute % contribution from heavy days ---
nc_future <- nc_open(future_ncfile)

time_future  <- ncvar_get(nc_future, "time")
dates_future <- time_origin + time_future

# Use all days of the year (no seasonal filtering)
day_indices_future <- seq_along(dates_future)

total_sum <- array(0, dim = c(length(lon), length(lat)))
heavy_sum <- array(0, dim = c(length(lon), length(lat)))

count <- 0

for (i in seq_along(day_indices_future)) {

    day <- ncvar_get(
        nc_future,
        "pr",
        start = c(1, 1, day_indices_future[i]),
        count = c(length(lon), length(lat), 1)
    ) * FLUX_TO_INCHES

    is_heavy <- !is.na(day) & !is.na(threshold_95) & (day > threshold_95)

    total_sum <- total_sum + ifelse(is.na(day), 0, day)
    heavy_sum <- heavy_sum + ifelse(is_heavy, day, 0)

    count <- count + 1

    rm(day, is_heavy)

    if (i %% 100 == 0) gc()
}

# Close future NetCDF
nc_close(nc_future)

# Percent of total precipitation contributed by heavy rain days
pct_heavy_precip <- (heavy_sum / total_sum) * 100
pct_heavy_precip[total_sum == 0] <- NA

# Define NetCDF dimensions
lon_dim <- ncdim_def("lon", "degrees_east",  lon)
lat_dim <- ncdim_def("lat", "degrees_north", lat)

# Define the output variable
pct_heavy_var <- ncvar_def("pct_heavy_precip", "%", list(lon_dim, lat_dim), missval = NA, prec = "double")

# Create the output NetCDF file
output_ncfile <- "VCP_DROUGHT_PRECIP95_PctHeavy_2045-2074_ECEARTH.nc"
nc_new <- nc_create(output_ncfile, list(pct_heavy_var))

# Write data to the NetCDF file
ncvar_put(nc_new, "pct_heavy_precip", pct_heavy_precip)

# Add coordinate variables
ncvar_put(nc_new, "lon", lon)
ncvar_put(nc_new, "lat", lat)

# Close the NetCDF file
nc_close(nc_new)

rm(
    threshold_95,
    total_sum,
    heavy_sum,
    pct_heavy_precip,
    lon,
    lat
)

gc()

print("Percent precip from heavy rain days (annual) saved to NetCDF file.")
```

---


## Percent precipitation from heavy rain days, full year, 2045-2074 (2050 timescale) FGOALS-g3

Calculates the percentage of total annual precipitation contributed by heavy rain days (days exceeding the 1950-2014 historical 95th percentile) for each grid location, for 2045-2074.

```{r}
library(ncdf4)

# File paths
historical_ncfile <- "FGOALS/precip/pr.FGOALS-g3.historical.r1i1p1f1.1950-2014.LOCA2_CA_hybrid_v20230115.nc"
future_ncfile     <- "FGOALS/precip/pr.FGOALS-g3.ssp370.r1i1p1f1.2045-2074.LOCA2_CA_hybrid_v20230115.nc"

# Native units are assumed to be kg m-2 s-1 (a flux): 1 kg/m2 = 1 mm water depth,
# so *86400 (seconds/day) -> mm/day, then /25.4 -> inches/day
FLUX_TO_INCHES <- 86400 / 25.4

# Wet-day threshold for percentile calc (1 mm, in inches)
WET_DAY_IN <- 1 / 25.4

# Open historical precip NetCDF
nc_historical <- nc_open(historical_ncfile)

# Extract coordinates and time
lon             <- ncvar_get(nc_historical, "lon")
lat             <- ncvar_get(nc_historical, "lat")
time_historical <- ncvar_get(nc_historical, "time")

# Verify the FLUX_TO_INCHES assumption against the actual file metadata
print(ncatt_get(nc_historical, "pr", "units")$value)

# Convert time units to actual dates
time_origin      <- as.Date("1900-01-01")
dates_historical <- time_origin + time_historical

# Use all days of the year (no seasonal filtering)
day_indices_historical <- seq_along(dates_historical)

# --- Compute the 95th percentile threshold (from wet days) per grid cell ---
# Processed in latitude bands to keep memory manageable (percentiles require the
# full distribution per cell, unlike the running-sum approach used for tmax means).
# Using all ~65 years of daily data (vs. JJA-only) roughly quadruples the size of
# each band read, so lat_chunk_size is set lower here than a JJA-only version would need.
lat_chunk_size <- 10
threshold_95 <- array(NA_real_, dim = c(length(lon), length(lat)))

for (lat_start in seq(1, length(lat), by = lat_chunk_size)) {

    lat_count <- min(lat_chunk_size, length(lat) - lat_start + 1)

    band <- array(NA_real_, dim = c(length(lon), lat_count, length(day_indices_historical)))

    for (i in seq_along(day_indices_historical)) {
        day_slice <- ncvar_get(
            nc_historical,
            "pr",
            start = c(1, lat_start, day_indices_historical[i]),
            count = c(length(lon), lat_count, 1)
        )
        band[, , i] <- day_slice * FLUX_TO_INCHES
    }

    threshold_95[, lat_start:(lat_start + lat_count - 1)] <- apply(band, c(1, 2), function(v) {
        wet_vals <- v[!is.na(v) & v >= WET_DAY_IN]
        if (length(wet_vals) > 0) quantile(wet_vals, 0.95, na.rm = TRUE) else NA_real_
    })

    rm(band)
    gc()
}

# Close historical NetCDF
nc_close(nc_historical)

# --- Apply threshold to the target period, compute % contribution from heavy days ---
nc_future <- nc_open(future_ncfile)

time_future  <- ncvar_get(nc_future, "time")
dates_future <- time_origin + time_future

# Use all days of the year (no seasonal filtering)
day_indices_future <- seq_along(dates_future)

total_sum <- array(0, dim = c(length(lon), length(lat)))
heavy_sum <- array(0, dim = c(length(lon), length(lat)))

count <- 0

for (i in seq_along(day_indices_future)) {

    day <- ncvar_get(
        nc_future,
        "pr",
        start = c(1, 1, day_indices_future[i]),
        count = c(length(lon), length(lat), 1)
    ) * FLUX_TO_INCHES

    is_heavy <- !is.na(day) & !is.na(threshold_95) & (day > threshold_95)

    total_sum <- total_sum + ifelse(is.na(day), 0, day)
    heavy_sum <- heavy_sum + ifelse(is_heavy, day, 0)

    count <- count + 1

    rm(day, is_heavy)

    if (i %% 100 == 0) gc()
}

# Close future NetCDF
nc_close(nc_future)

# Percent of total precipitation contributed by heavy rain days
pct_heavy_precip <- (heavy_sum / total_sum) * 100
pct_heavy_precip[total_sum == 0] <- NA

# Define NetCDF dimensions
lon_dim <- ncdim_def("lon", "degrees_east",  lon)
lat_dim <- ncdim_def("lat", "degrees_north", lat)

# Define the output variable
pct_heavy_var <- ncvar_def("pct_heavy_precip", "%", list(lon_dim, lat_dim), missval = NA, prec = "double")

# Create the output NetCDF file
output_ncfile <- "VCP_DROUGHT_PRECIP95_PctHeavy_2045-2074_FGOALS.nc"
nc_new <- nc_create(output_ncfile, list(pct_heavy_var))

# Write data to the NetCDF file
ncvar_put(nc_new, "pct_heavy_precip", pct_heavy_precip)

# Add coordinate variables
ncvar_put(nc_new, "lon", lon)
ncvar_put(nc_new, "lat", lat)

# Close the NetCDF file
nc_close(nc_new)

rm(
    threshold_95,
    total_sum,
    heavy_sum,
    pct_heavy_precip,
    lon,
    lat
)

gc()

print("Percent precip from heavy rain days (annual) saved to NetCDF file.")
```

---


## Percent precipitation from heavy rain days, full year, 2045-2074 (2050 timescale) MIROC6

Calculates the percentage of total annual precipitation contributed by heavy rain days (days exceeding the 1950-2014 historical 95th percentile) for each grid location, for 2045-2074.

```{r}
library(ncdf4)

# File paths
historical_ncfile <- "MIROC6/precip/pr.MIROC6.historical.r1i1p1f1.1950-2014.LOCA2_CA_hybrid_v20230115.nc"
future_ncfile     <- "MIROC6/precip/pr.MIROC6.ssp370.r1i1p1f1.2045-2074.LOCA2_CA_hybrid_v20230115.nc"

# Native units are assumed to be kg m-2 s-1 (a flux): 1 kg/m2 = 1 mm water depth,
# so *86400 (seconds/day) -> mm/day, then /25.4 -> inches/day
FLUX_TO_INCHES <- 86400 / 25.4

# Wet-day threshold for percentile calc (1 mm, in inches)
WET_DAY_IN <- 1 / 25.4

# Open historical precip NetCDF
nc_historical <- nc_open(historical_ncfile)

# Extract coordinates and time
lon             <- ncvar_get(nc_historical, "lon")
lat             <- ncvar_get(nc_historical, "lat")
time_historical <- ncvar_get(nc_historical, "time")

# Verify the FLUX_TO_INCHES assumption against the actual file metadata
print(ncatt_get(nc_historical, "pr", "units")$value)

# Convert time units to actual dates
time_origin      <- as.Date("1900-01-01")
dates_historical <- time_origin + time_historical

# Use all days of the year (no seasonal filtering)
day_indices_historical <- seq_along(dates_historical)

# --- Compute the 95th percentile threshold (from wet days) per grid cell ---
# Processed in latitude bands to keep memory manageable (percentiles require the
# full distribution per cell, unlike the running-sum approach used for tmax means).
# Using all ~65 years of daily data (vs. JJA-only) roughly quadruples the size of
# each band read, so lat_chunk_size is set lower here than a JJA-only version would need.
lat_chunk_size <- 10
threshold_95 <- array(NA_real_, dim = c(length(lon), length(lat)))

for (lat_start in seq(1, length(lat), by = lat_chunk_size)) {

    lat_count <- min(lat_chunk_size, length(lat) - lat_start + 1)

    band <- array(NA_real_, dim = c(length(lon), lat_count, length(day_indices_historical)))

    for (i in seq_along(day_indices_historical)) {
        day_slice <- ncvar_get(
            nc_historical,
            "pr",
            start = c(1, lat_start, day_indices_historical[i]),
            count = c(length(lon), lat_count, 1)
        )
        band[, , i] <- day_slice * FLUX_TO_INCHES
    }

    threshold_95[, lat_start:(lat_start + lat_count - 1)] <- apply(band, c(1, 2), function(v) {
        wet_vals <- v[!is.na(v) & v >= WET_DAY_IN]
        if (length(wet_vals) > 0) quantile(wet_vals, 0.95, na.rm = TRUE) else NA_real_
    })

    rm(band)
    gc()
}

# Close historical NetCDF
nc_close(nc_historical)

# --- Apply threshold to the target period, compute % contribution from heavy days ---
nc_future <- nc_open(future_ncfile)

time_future  <- ncvar_get(nc_future, "time")
dates_future <- time_origin + time_future

# Use all days of the year (no seasonal filtering)
day_indices_future <- seq_along(dates_future)

total_sum <- array(0, dim = c(length(lon), length(lat)))
heavy_sum <- array(0, dim = c(length(lon), length(lat)))

count <- 0

for (i in seq_along(day_indices_future)) {

    day <- ncvar_get(
        nc_future,
        "pr",
        start = c(1, 1, day_indices_future[i]),
        count = c(length(lon), length(lat), 1)
    ) * FLUX_TO_INCHES

    is_heavy <- !is.na(day) & !is.na(threshold_95) & (day > threshold_95)

    total_sum <- total_sum + ifelse(is.na(day), 0, day)
    heavy_sum <- heavy_sum + ifelse(is_heavy, day, 0)

    count <- count + 1

    rm(day, is_heavy)

    if (i %% 100 == 0) gc()
}

# Close future NetCDF
nc_close(nc_future)

# Percent of total precipitation contributed by heavy rain days
pct_heavy_precip <- (heavy_sum / total_sum) * 100
pct_heavy_precip[total_sum == 0] <- NA

# Define NetCDF dimensions
lon_dim <- ncdim_def("lon", "degrees_east",  lon)
lat_dim <- ncdim_def("lat", "degrees_north", lat)

# Define the output variable
pct_heavy_var <- ncvar_def("pct_heavy_precip", "%", list(lon_dim, lat_dim), missval = NA, prec = "double")

# Create the output NetCDF file
output_ncfile <- "VCP_DROUGHT_PRECIP95_PctHeavy_2045-2074_MIROC6.nc"
nc_new <- nc_create(output_ncfile, list(pct_heavy_var))

# Write data to the NetCDF file
ncvar_put(nc_new, "pct_heavy_precip", pct_heavy_precip)

# Add coordinate variables
ncvar_put(nc_new, "lon", lon)
ncvar_put(nc_new, "lat", lat)

# Close the NetCDF file
nc_close(nc_new)

rm(
    threshold_95,
    total_sum,
    heavy_sum,
    pct_heavy_precip,
    lon,
    lat
)

gc()

print("Percent precip from heavy rain days (annual) saved to NetCDF file.")
```

---


## Percent precipitation from heavy rain days, full year, 2045-2074 (2050 timescale) MPI-ESM1-2-HR

Calculates the percentage of total annual precipitation contributed by heavy rain days (days exceeding the 1950-2014 historical 95th percentile) for each grid location, for 2045-2074.

```{r}
library(ncdf4)
setwd("D:/Climate_Data")

# File paths
historical_ncfile <- "D:/Climate_Data/MPIESM12/precip/pr.MPI-ESM1-2-HR.historical.r3i1p1f1.1950-2014.LOCA2_CA_hybrid_v20230115.nc"
future_ncfile     <- "MPIESM12/precip/pr.MPI-ESM1-2-HR.ssp370.r3i1p1f1.2045-2074.LOCA2_CA_hybrid_v20230115.nc"

# Native units are assumed to be kg m-2 s-1 (a flux): 1 kg/m2 = 1 mm water depth,
# so *86400 (seconds/day) -> mm/day, then /25.4 -> inches/day
FLUX_TO_INCHES <- 86400 / 25.4

# Wet-day threshold for percentile calc (1 mm, in inches)
WET_DAY_IN <- 1 / 25.4

# Open historical precip NetCDF
nc_historical <- nc_open(historical_ncfile)

# Extract coordinates and time
lon             <- ncvar_get(nc_historical, "lon")
lat             <- ncvar_get(nc_historical, "lat")
time_historical <- ncvar_get(nc_historical, "time")

# Verify the FLUX_TO_INCHES assumption against the actual file metadata
print(ncatt_get(nc_historical, "pr", "units")$value)

# Convert time units to actual dates
time_origin      <- as.Date("1900-01-01")
dates_historical <- time_origin + time_historical

# Use all days of the year (no seasonal filtering)
day_indices_historical <- seq_along(dates_historical)

# --- Compute the 95th percentile threshold (from wet days) per grid cell ---
# Processed in latitude bands to keep memory manageable (percentiles require the
# full distribution per cell, unlike the running-sum approach used for tmax means).
# Using all ~65 years of daily data (vs. JJA-only) roughly quadruples the size of
# each band read, so lat_chunk_size is set lower here than a JJA-only version would need.
lat_chunk_size <- 10
threshold_95 <- array(NA_real_, dim = c(length(lon), length(lat)))

for (lat_start in seq(1, length(lat), by = lat_chunk_size)) {

    lat_count <- min(lat_chunk_size, length(lat) - lat_start + 1)

    band <- array(NA_real_, dim = c(length(lon), lat_count, length(day_indices_historical)))

    for (i in seq_along(day_indices_historical)) {
        day_slice <- ncvar_get(
            nc_historical,
            "pr",
            start = c(1, lat_start, day_indices_historical[i]),
            count = c(length(lon), lat_count, 1)
        )
        band[, , i] <- day_slice * FLUX_TO_INCHES
    }

    threshold_95[, lat_start:(lat_start + lat_count - 1)] <- apply(band, c(1, 2), function(v) {
        wet_vals <- v[!is.na(v) & v >= WET_DAY_IN]
        if (length(wet_vals) > 0) quantile(wet_vals, 0.95, na.rm = TRUE) else NA_real_
    })

    rm(band)
    gc()
}

# Close historical NetCDF
nc_close(nc_historical)

# --- Apply threshold to the target period, compute % contribution from heavy days ---
nc_future <- nc_open(future_ncfile)

time_future  <- ncvar_get(nc_future, "time")
dates_future <- time_origin + time_future

# Use all days of the year (no seasonal filtering)
day_indices_future <- seq_along(dates_future)

total_sum <- array(0, dim = c(length(lon), length(lat)))
heavy_sum <- array(0, dim = c(length(lon), length(lat)))

count <- 0

for (i in seq_along(day_indices_future)) {

    day <- ncvar_get(
        nc_future,
        "pr",
        start = c(1, 1, day_indices_future[i]),
        count = c(length(lon), length(lat), 1)
    ) * FLUX_TO_INCHES

    is_heavy <- !is.na(day) & !is.na(threshold_95) & (day > threshold_95)

    total_sum <- total_sum + ifelse(is.na(day), 0, day)
    heavy_sum <- heavy_sum + ifelse(is_heavy, day, 0)

    count <- count + 1

    rm(day, is_heavy)

    if (i %% 100 == 0) gc()
}

# Close future NetCDF
nc_close(nc_future)

# Percent of total precipitation contributed by heavy rain days
pct_heavy_precip <- (heavy_sum / total_sum) * 100
pct_heavy_precip[total_sum == 0] <- NA

# Define NetCDF dimensions
lon_dim <- ncdim_def("lon", "degrees_east",  lon)
lat_dim <- ncdim_def("lat", "degrees_north", lat)

# Define the output variable
pct_heavy_var <- ncvar_def("pct_heavy_precip", "%", list(lon_dim, lat_dim), missval = NA, prec = "double")

# Create the output NetCDF file
output_ncfile <- "VCP_DROUGHT_PRECIP95_PctHeavy_2045-2074_MPI-ESM1-2-HR.nc"
nc_new <- nc_create(output_ncfile, list(pct_heavy_var))

# Write data to the NetCDF file
ncvar_put(nc_new, "pct_heavy_precip", pct_heavy_precip)

# Add coordinate variables
ncvar_put(nc_new, "lon", lon)
ncvar_put(nc_new, "lat", lat)

# Close the NetCDF file
nc_close(nc_new)

rm(
    threshold_95,
    total_sum,
    heavy_sum,
    pct_heavy_precip,
    lon,
    lat
)

gc()

print("Percent precip from heavy rain days (annual) saved to NetCDF file.")
```

---


## Percent precipitation from heavy rain days, full year, 2015-2044 (current) ACCESS-CM2

Calculates the percentage of total annual precipitation contributed by heavy rain days (days exceeding the 1950-2014 historical 95th percentile) for each grid location, for 2015-2044.

```{r}
library(ncdf4)
setwd("D:/Climate_Data/")

AH# File paths
historical_ncfile <- "ACCESSCM2/precip/pr.ACCESS-CM2.historical.r1i1p1f1.1950-2014.LOCA2_CA_hybrid_v20230115.nc"
future_ncfile     <- "ACCESSCM2/precip/pr.ACCESS-CM2.ssp370.r1i1p1f1.2015-2044.LOCA2_CA_hybrid_v20230115.nc"

# Native units are assumed to be kg m-2 s-1 (a flux): 1 kg/m2 = 1 mm water depth,
# so *86400 (seconds/day) -> mm/day, then /25.4 -> inches/day
FLUX_TO_INCHES <- 86400 / 25.4

# Wet-day threshold for percentile calc (1 mm, in inches)
WET_DAY_IN <- 1 / 25.4

# Open historical precip NetCDF
nc_historical <- nc_open(historical_ncfile)

# Extract coordinates and time
lon             <- ncvar_get(nc_historical, "lon")
lat             <- ncvar_get(nc_historical, "lat")
time_historical <- ncvar_get(nc_historical, "time")

# Verify the FLUX_TO_INCHES assumption against the actual file metadata
print(ncatt_get(nc_historical, "pr", "units")$value)

# Convert time units to actual dates
time_origin      <- as.Date("1900-01-01")
dates_historical <- time_origin + time_historical

# Use all days of the year (no seasonal filtering)
day_indices_historical <- seq_along(dates_historical)

# --- Compute the 95th percentile threshold (from wet days) per grid cell ---
# Processed in latitude bands to keep memory manageable (percentiles require the
# full distribution per cell, unlike the running-sum approach used for tmax means).
# Using all ~65 years of daily data (vs. JJA-only) roughly quadruples the size of
# each band read, so lat_chunk_size is set lower here than a JJA-only version would need.
lat_chunk_size <- 10
threshold_95 <- array(NA_real_, dim = c(length(lon), length(lat)))

for (lat_start in seq(1, length(lat), by = lat_chunk_size)) {

    lat_count <- min(lat_chunk_size, length(lat) - lat_start + 1)

    band <- array(NA_real_, dim = c(length(lon), lat_count, length(day_indices_historical)))

    for (i in seq_along(day_indices_historical)) {
        day_slice <- ncvar_get(
            nc_historical,
            "pr",
            start = c(1, lat_start, day_indices_historical[i]),
            count = c(length(lon), lat_count, 1)
        )
        band[, , i] <- day_slice * FLUX_TO_INCHES
    }

    threshold_95[, lat_start:(lat_start + lat_count - 1)] <- apply(band, c(1, 2), function(v) {
        wet_vals <- v[!is.na(v) & v >= WET_DAY_IN]
        if (length(wet_vals) > 0) quantile(wet_vals, 0.95, na.rm = TRUE) else NA_real_
    })

    rm(band)
    gc()
}

# Close historical NetCDF
nc_close(nc_historical)

# --- Apply threshold to the target period, compute % contribution from heavy days ---
nc_future <- nc_open(future_ncfile)

time_future  <- ncvar_get(nc_future, "time")
dates_future <- time_origin + time_future

# Use all days of the year (no seasonal filtering)
day_indices_future <- seq_along(dates_future)

total_sum <- array(0, dim = c(length(lon), length(lat)))
heavy_sum <- array(0, dim = c(length(lon), length(lat)))

count <- 0

for (i in seq_along(day_indices_future)) {

    day <- ncvar_get(
        nc_future,
        "pr",
        start = c(1, 1, day_indices_future[i]),
        count = c(length(lon), length(lat), 1)
    ) * FLUX_TO_INCHES

    is_heavy <- !is.na(day) & !is.na(threshold_95) & (day > threshold_95)

    total_sum <- total_sum + ifelse(is.na(day), 0, day)
    heavy_sum <- heavy_sum + ifelse(is_heavy, day, 0)

    count <- count + 1

    rm(day, is_heavy)

    if (i %% 100 == 0) gc()
}

# Close future NetCDF
nc_close(nc_future)

# Percent of total precipitation contributed by heavy rain days
pct_heavy_precip <- (heavy_sum / total_sum) * 100
pct_heavy_precip[total_sum == 0] <- NA

# Define NetCDF dimensions
lon_dim <- ncdim_def("lon", "degrees_east",  lon)
lat_dim <- ncdim_def("lat", "degrees_north", lat)

# Define the output variable
pct_heavy_var <- ncvar_def("pct_heavy_precip", "%", list(lon_dim, lat_dim), missval = NA, prec = "double")

# Create the output NetCDF file
output_ncfile <- "VCP_DROUGHT_PRECIP95_PctHeavy_2015-2044_ACCESS.nc"
nc_new <- nc_create(output_ncfile, list(pct_heavy_var))

# Write data to the NetCDF file
ncvar_put(nc_new, "pct_heavy_precip", pct_heavy_precip)

# Add coordinate variables
ncvar_put(nc_new, "lon", lon)
ncvar_put(nc_new, "lat", lat)

# Close the NetCDF file
nc_close(nc_new)

rm(
    threshold_95,
    total_sum,
    heavy_sum,
    pct_heavy_precip,
    lon,
    lat
)

gc()

print("Percent precip from heavy rain days (annual) saved to NetCDF file.")
```

---


## Percent precipitation from heavy rain days, full year, 2015-2044 (current) EC-Earth3

Calculates the percentage of total annual precipitation contributed by heavy rain days (days exceeding the 1950-2014 historical 95th percentile) for each grid location, for 2015-2044.

```{r}
library(ncdf4)

# File paths
historical_ncfile <- "ECEARTH/precip/pr.EC-Earth3.historical.r1i1p1f1.1950-2014.LOCA2_CA_hybrid_v20230115.nc"
future_ncfile     <- "ECEARTH/precip/pr.EC-Earth3.ssp370.r1i1p1f1.2015-2044.LOCA2_CA_hybrid_v20230115.nc"

# Native units are assumed to be kg m-2 s-1 (a flux): 1 kg/m2 = 1 mm water depth,
# so *86400 (seconds/day) -> mm/day, then /25.4 -> inches/day
FLUX_TO_INCHES <- 86400 / 25.4

# Wet-day threshold for percentile calc (1 mm, in inches)
WET_DAY_IN <- 1 / 25.4

# Open historical precip NetCDF
nc_historical <- nc_open(historical_ncfile)

# Extract coordinates and time
lon             <- ncvar_get(nc_historical, "lon")
lat             <- ncvar_get(nc_historical, "lat")
time_historical <- ncvar_get(nc_historical, "time")

# Verify the FLUX_TO_INCHES assumption against the actual file metadata
print(ncatt_get(nc_historical, "pr", "units")$value)

# Convert time units to actual dates
time_origin      <- as.Date("1900-01-01")
dates_historical <- time_origin + time_historical

# Use all days of the year (no seasonal filtering)
day_indices_historical <- seq_along(dates_historical)

# --- Compute the 95th percentile threshold (from wet days) per grid cell ---
# Processed in latitude bands to keep memory manageable (percentiles require the
# full distribution per cell, unlike the running-sum approach used for tmax means).
# Using all ~65 years of daily data (vs. JJA-only) roughly quadruples the size of
# each band read, so lat_chunk_size is set lower here than a JJA-only version would need.
lat_chunk_size <- 10
threshold_95 <- array(NA_real_, dim = c(length(lon), length(lat)))

for (lat_start in seq(1, length(lat), by = lat_chunk_size)) {

    lat_count <- min(lat_chunk_size, length(lat) - lat_start + 1)

    band <- array(NA_real_, dim = c(length(lon), lat_count, length(day_indices_historical)))

    for (i in seq_along(day_indices_historical)) {
        day_slice <- ncvar_get(
            nc_historical,
            "pr",
            start = c(1, lat_start, day_indices_historical[i]),
            count = c(length(lon), lat_count, 1)
        )
        band[, , i] <- day_slice * FLUX_TO_INCHES
    }

    threshold_95[, lat_start:(lat_start + lat_count - 1)] <- apply(band, c(1, 2), function(v) {
        wet_vals <- v[!is.na(v) & v >= WET_DAY_IN]
        if (length(wet_vals) > 0) quantile(wet_vals, 0.95, na.rm = TRUE) else NA_real_
    })

    rm(band)
    gc()
}

# Close historical NetCDF
nc_close(nc_historical)

# --- Apply threshold to the target period, compute % contribution from heavy days ---
nc_future <- nc_open(future_ncfile)

time_future  <- ncvar_get(nc_future, "time")
dates_future <- time_origin + time_future

# Use all days of the year (no seasonal filtering)
day_indices_future <- seq_along(dates_future)

total_sum <- array(0, dim = c(length(lon), length(lat)))
heavy_sum <- array(0, dim = c(length(lon), length(lat)))

count <- 0

for (i in seq_along(day_indices_future)) {

    day <- ncvar_get(
        nc_future,
        "pr",
        start = c(1, 1, day_indices_future[i]),
        count = c(length(lon), length(lat), 1)
    ) * FLUX_TO_INCHES

    is_heavy <- !is.na(day) & !is.na(threshold_95) & (day > threshold_95)

    total_sum <- total_sum + ifelse(is.na(day), 0, day)
    heavy_sum <- heavy_sum + ifelse(is_heavy, day, 0)

    count <- count + 1

    rm(day, is_heavy)

    if (i %% 100 == 0) gc()
}

# Close future NetCDF
nc_close(nc_future)

# Percent of total precipitation contributed by heavy rain days
pct_heavy_precip <- (heavy_sum / total_sum) * 100
pct_heavy_precip[total_sum == 0] <- NA

# Define NetCDF dimensions
lon_dim <- ncdim_def("lon", "degrees_east",  lon)
lat_dim <- ncdim_def("lat", "degrees_north", lat)

# Define the output variable
pct_heavy_var <- ncvar_def("pct_heavy_precip", "%", list(lon_dim, lat_dim), missval = NA, prec = "double")

# Create the output NetCDF file
output_ncfile <- "VCP_DROUGHT_PRECIP95_PctHeavy_2015-2044_ECEARTH.nc"
nc_new <- nc_create(output_ncfile, list(pct_heavy_var))

# Write data to the NetCDF file
ncvar_put(nc_new, "pct_heavy_precip", pct_heavy_precip)

# Add coordinate variables
ncvar_put(nc_new, "lon", lon)
ncvar_put(nc_new, "lat", lat)

# Close the NetCDF file
nc_close(nc_new)

rm(
    threshold_95,
    total_sum,
    heavy_sum,
    pct_heavy_precip,
    lon,
    lat
)

gc()

print("Percent precip from heavy rain days (annual) saved to NetCDF file.")
```

---


## Percent precipitation from heavy rain days, full year, 2015-2044 (current) FGOALS-g3

Calculates the percentage of total annual precipitation contributed by heavy rain days (days exceeding the 1950-2014 historical 95th percentile) for each grid location, for 2015-2044.

```{r}
library(ncdf4)

# File paths
historical_ncfile <- "FGOALS/precip/pr.FGOALS-g3.historical.r1i1p1f1.1950-2014.LOCA2_CA_hybrid_v20230115.nc"
future_ncfile     <- "FGOALS/precip/pr.FGOALS-g3.ssp370.r1i1p1f1.2015-2044.LOCA2_CA_hybrid_v20230115.nc"

# Native units are assumed to be kg m-2 s-1 (a flux): 1 kg/m2 = 1 mm water depth,
# so *86400 (seconds/day) -> mm/day, then /25.4 -> inches/day
FLUX_TO_INCHES <- 86400 / 25.4

# Wet-day threshold for percentile calc (1 mm, in inches)
WET_DAY_IN <- 1 / 25.4

# Open historical precip NetCDF
nc_historical <- nc_open(historical_ncfile)

# Extract coordinates and time
lon             <- ncvar_get(nc_historical, "lon")
lat             <- ncvar_get(nc_historical, "lat")
time_historical <- ncvar_get(nc_historical, "time")

# Verify the FLUX_TO_INCHES assumption against the actual file metadata
print(ncatt_get(nc_historical, "pr", "units")$value)

# Convert time units to actual dates
time_origin      <- as.Date("1900-01-01")
dates_historical <- time_origin + time_historical

# Use all days of the year (no seasonal filtering)
day_indices_historical <- seq_along(dates_historical)

# --- Compute the 95th percentile threshold (from wet days) per grid cell ---
# Processed in latitude bands to keep memory manageable (percentiles require the
# full distribution per cell, unlike the running-sum approach used for tmax means).
# Using all ~65 years of daily data (vs. JJA-only) roughly quadruples the size of
# each band read, so lat_chunk_size is set lower here than a JJA-only version would need.
lat_chunk_size <- 10
threshold_95 <- array(NA_real_, dim = c(length(lon), length(lat)))

for (lat_start in seq(1, length(lat), by = lat_chunk_size)) {

    lat_count <- min(lat_chunk_size, length(lat) - lat_start + 1)

    band <- array(NA_real_, dim = c(length(lon), lat_count, length(day_indices_historical)))

    for (i in seq_along(day_indices_historical)) {
        day_slice <- ncvar_get(
            nc_historical,
            "pr",
            start = c(1, lat_start, day_indices_historical[i]),
            count = c(length(lon), lat_count, 1)
        )
        band[, , i] <- day_slice * FLUX_TO_INCHES
    }

    threshold_95[, lat_start:(lat_start + lat_count - 1)] <- apply(band, c(1, 2), function(v) {
        wet_vals <- v[!is.na(v) & v >= WET_DAY_IN]
        if (length(wet_vals) > 0) quantile(wet_vals, 0.95, na.rm = TRUE) else NA_real_
    })

    rm(band)
    gc()
}

# Close historical NetCDF
nc_close(nc_historical)

# --- Apply threshold to the target period, compute % contribution from heavy days ---
nc_future <- nc_open(future_ncfile)

time_future  <- ncvar_get(nc_future, "time")
dates_future <- time_origin + time_future

# Use all days of the year (no seasonal filtering)
day_indices_future <- seq_along(dates_future)

total_sum <- array(0, dim = c(length(lon), length(lat)))
heavy_sum <- array(0, dim = c(length(lon), length(lat)))

count <- 0

for (i in seq_along(day_indices_future)) {

    day <- ncvar_get(
        nc_future,
        "pr",
        start = c(1, 1, day_indices_future[i]),
        count = c(length(lon), length(lat), 1)
    ) * FLUX_TO_INCHES

    is_heavy <- !is.na(day) & !is.na(threshold_95) & (day > threshold_95)

    total_sum <- total_sum + ifelse(is.na(day), 0, day)
    heavy_sum <- heavy_sum + ifelse(is_heavy, day, 0)

    count <- count + 1

    rm(day, is_heavy)

    if (i %% 100 == 0) gc()
}

# Close future NetCDF
nc_close(nc_future)

# Percent of total precipitation contributed by heavy rain days
pct_heavy_precip <- (heavy_sum / total_sum) * 100
pct_heavy_precip[total_sum == 0] <- NA

# Define NetCDF dimensions
lon_dim <- ncdim_def("lon", "degrees_east",  lon)
lat_dim <- ncdim_def("lat", "degrees_north", lat)

# Define the output variable
pct_heavy_var <- ncvar_def("pct_heavy_precip", "%", list(lon_dim, lat_dim), missval = NA, prec = "double")

# Create the output NetCDF file
output_ncfile <- "VCP_DROUGHT_PRECIP95_PctHeavy_2015-2044_FGOALS.nc"
nc_new <- nc_create(output_ncfile, list(pct_heavy_var))

# Write data to the NetCDF file
ncvar_put(nc_new, "pct_heavy_precip", pct_heavy_precip)

# Add coordinate variables
ncvar_put(nc_new, "lon", lon)
ncvar_put(nc_new, "lat", lat)

# Close the NetCDF file
nc_close(nc_new)

rm(
    threshold_95,
    total_sum,
    heavy_sum,
    pct_heavy_precip,
    lon,
    lat
)

gc()

print("Percent precip from heavy rain days (annual) saved to NetCDF file.")
```

---


## Percent precipitation from heavy rain days, full year, 2015-2044 (current) MIROC6

Calculates the percentage of total annual precipitation contributed by heavy rain days (days exceeding the 1950-2014 historical 95th percentile) for each grid location, for 2015-2044.

```{r}
library(ncdf4)

# File paths
historical_ncfile <- "MIROC6/precip/pr.MIROC6.historical.r1i1p1f1.1950-2014.LOCA2_CA_hybrid_v20230115.nc"
future_ncfile     <- "MIROC6/precip/pr.MIROC6.ssp370.r1i1p1f1.2015-2044.LOCA2_CA_hybrid_v20230115.nc"

# Native units are assumed to be kg m-2 s-1 (a flux): 1 kg/m2 = 1 mm water depth,
# so *86400 (seconds/day) -> mm/day, then /25.4 -> inches/day
FLUX_TO_INCHES <- 86400 / 25.4

# Wet-day threshold for percentile calc (1 mm, in inches)
WET_DAY_IN <- 1 / 25.4

# Open historical precip NetCDF
nc_historical <- nc_open(historical_ncfile)

# Extract coordinates and time
lon             <- ncvar_get(nc_historical, "lon")
lat             <- ncvar_get(nc_historical, "lat")
time_historical <- ncvar_get(nc_historical, "time")

# Verify the FLUX_TO_INCHES assumption against the actual file metadata
print(ncatt_get(nc_historical, "pr", "units")$value)

# Convert time units to actual dates
time_origin      <- as.Date("1900-01-01")
dates_historical <- time_origin + time_historical

# Use all days of the year (no seasonal filtering)
day_indices_historical <- seq_along(dates_historical)

# --- Compute the 95th percentile threshold (from wet days) per grid cell ---
# Processed in latitude bands to keep memory manageable (percentiles require the
# full distribution per cell, unlike the running-sum approach used for tmax means).
# Using all ~65 years of daily data (vs. JJA-only) roughly quadruples the size of
# each band read, so lat_chunk_size is set lower here than a JJA-only version would need.
lat_chunk_size <- 10
threshold_95 <- array(NA_real_, dim = c(length(lon), length(lat)))

for (lat_start in seq(1, length(lat), by = lat_chunk_size)) {

    lat_count <- min(lat_chunk_size, length(lat) - lat_start + 1)

    band <- array(NA_real_, dim = c(length(lon), lat_count, length(day_indices_historical)))

    for (i in seq_along(day_indices_historical)) {
        day_slice <- ncvar_get(
            nc_historical,
            "pr",
            start = c(1, lat_start, day_indices_historical[i]),
            count = c(length(lon), lat_count, 1)
        )
        band[, , i] <- day_slice * FLUX_TO_INCHES
    }

    threshold_95[, lat_start:(lat_start + lat_count - 1)] <- apply(band, c(1, 2), function(v) {
        wet_vals <- v[!is.na(v) & v >= WET_DAY_IN]
        if (length(wet_vals) > 0) quantile(wet_vals, 0.95, na.rm = TRUE) else NA_real_
    })

    rm(band)
    gc()
}

# Close historical NetCDF
nc_close(nc_historical)

# --- Apply threshold to the target period, compute % contribution from heavy days ---
nc_future <- nc_open(future_ncfile)

time_future  <- ncvar_get(nc_future, "time")
dates_future <- time_origin + time_future

# Use all days of the year (no seasonal filtering)
day_indices_future <- seq_along(dates_future)

total_sum <- array(0, dim = c(length(lon), length(lat)))
heavy_sum <- array(0, dim = c(length(lon), length(lat)))

count <- 0

for (i in seq_along(day_indices_future)) {

    day <- ncvar_get(
        nc_future,
        "pr",
        start = c(1, 1, day_indices_future[i]),
        count = c(length(lon), length(lat), 1)
    ) * FLUX_TO_INCHES

    is_heavy <- !is.na(day) & !is.na(threshold_95) & (day > threshold_95)

    total_sum <- total_sum + ifelse(is.na(day), 0, day)
    heavy_sum <- heavy_sum + ifelse(is_heavy, day, 0)

    count <- count + 1

    rm(day, is_heavy)

    if (i %% 100 == 0) gc()
}

# Close future NetCDF
nc_close(nc_future)

# Percent of total precipitation contributed by heavy rain days
pct_heavy_precip <- (heavy_sum / total_sum) * 100
pct_heavy_precip[total_sum == 0] <- NA

# Define NetCDF dimensions
lon_dim <- ncdim_def("lon", "degrees_east",  lon)
lat_dim <- ncdim_def("lat", "degrees_north", lat)

# Define the output variable
pct_heavy_var <- ncvar_def("pct_heavy_precip", "%", list(lon_dim, lat_dim), missval = NA, prec = "double")

# Create the output NetCDF file
output_ncfile <- "VCP_DROUGHT_PRECIP95_PctHeavy_2015-2044_MIROC6.nc"
nc_new <- nc_create(output_ncfile, list(pct_heavy_var))

# Write data to the NetCDF file
ncvar_put(nc_new, "pct_heavy_precip", pct_heavy_precip)

# Add coordinate variables
ncvar_put(nc_new, "lon", lon)
ncvar_put(nc_new, "lat", lat)

# Close the NetCDF file
nc_close(nc_new)

rm(
    threshold_95,
    total_sum,
    heavy_sum,
    pct_heavy_precip,
    lon,
    lat
)

gc()

print("Percent precip from heavy rain days (annual) saved to NetCDF file.")
```

---


## Percent precipitation from heavy rain days, full year, 2015-2044 (current) MPI-ESM1-2-HR

Calculates the percentage of total annual precipitation contributed by heavy rain days (days exceeding the 1950-2014 historical 95th percentile) for each grid location, for 2015-2044.

```{r}
library(ncdf4)
setwd("D:/Climate_Data/")

# File paths
historical_ncfile <- "D:\\Climate_Data\\MPIESM12\\precip\\pr.MPI-ESM1-2-HR.historical.r3i1p1f1.1950-2014.LOCA2_CA_hybrid_v20230115.nc"
future_ncfile     <- "MPIESM12/precip/pr.MPI-ESM1-2-HR.ssp370.r3i1p1f1.2015-2044.LOCA2_CA_hybrid_v20230115.nc"

# Native units are assumed to be kg m-2 s-1 (a flux): 1 kg/m2 = 1 mm water depth,
# so *86400 (seconds/day) -> mm/day, then /25.4 -> inches/day
FLUX_TO_INCHES <- 86400 / 25.4

# Wet-day threshold for percentile calc (1 mm, in inches)
WET_DAY_IN <- 1 / 25.4

# Open historical precip NetCDF
nc_historical <- nc_open(historical_ncfile)

# Extract coordinates and time
lon             <- ncvar_get(nc_historical, "lon")
lat             <- ncvar_get(nc_historical, "lat")
time_historical <- ncvar_get(nc_historical, "time")

# Verify the FLUX_TO_INCHES assumption against the actual file metadata
print(ncatt_get(nc_historical, "pr", "units")$value)

# Convert time units to actual dates
time_origin      <- as.Date("1900-01-01")
dates_historical <- time_origin + time_historical

# Use all days of the year (no seasonal filtering)
day_indices_historical <- seq_along(dates_historical)

# --- Compute the 95th percentile threshold (from wet days) per grid cell ---
# Processed in latitude bands to keep memory manageable (percentiles require the
# full distribution per cell, unlike the running-sum approach used for tmax means).
# Using all ~65 years of daily data (vs. JJA-only) roughly quadruples the size of
# each band read, so lat_chunk_size is set lower here than a JJA-only version would need.
lat_chunk_size <- 10
threshold_95 <- array(NA_real_, dim = c(length(lon), length(lat)))

for (lat_start in seq(1, length(lat), by = lat_chunk_size)) {

    lat_count <- min(lat_chunk_size, length(lat) - lat_start + 1)

    band <- array(NA_real_, dim = c(length(lon), lat_count, length(day_indices_historical)))

    for (i in seq_along(day_indices_historical)) {
        day_slice <- ncvar_get(
            nc_historical,
            "pr",
            start = c(1, lat_start, day_indices_historical[i]),
            count = c(length(lon), lat_count, 1)
        )
        band[, , i] <- day_slice * FLUX_TO_INCHES
    }

    threshold_95[, lat_start:(lat_start + lat_count - 1)] <- apply(band, c(1, 2), function(v) {
        wet_vals <- v[!is.na(v) & v >= WET_DAY_IN]
        if (length(wet_vals) > 0) quantile(wet_vals, 0.95, na.rm = TRUE) else NA_real_
    })

    rm(band)
    gc()
}

# Close historical NetCDF
nc_close(nc_historical)

# --- Apply threshold to the target period, compute % contribution from heavy days ---
nc_future <- nc_open(future_ncfile)

time_future  <- ncvar_get(nc_future, "time")
dates_future <- time_origin + time_future

# Use all days of the year (no seasonal filtering)
day_indices_future <- seq_along(dates_future)

total_sum <- array(0, dim = c(length(lon), length(lat)))
heavy_sum <- array(0, dim = c(length(lon), length(lat)))

count <- 0

for (i in seq_along(day_indices_future)) {

    day <- ncvar_get(
        nc_future,
        "pr",
        start = c(1, 1, day_indices_future[i]),
        count = c(length(lon), length(lat), 1)
    ) * FLUX_TO_INCHES

    is_heavy <- !is.na(day) & !is.na(threshold_95) & (day > threshold_95)

    total_sum <- total_sum + ifelse(is.na(day), 0, day)
    heavy_sum <- heavy_sum + ifelse(is_heavy, day, 0)

    count <- count + 1

    rm(day, is_heavy)

    if (i %% 100 == 0) gc()
}

# Close future NetCDF
nc_close(nc_future)

# Percent of total precipitation contributed by heavy rain days
pct_heavy_precip <- (heavy_sum / total_sum) * 100
pct_heavy_precip[total_sum == 0] <- NA

# Define NetCDF dimensions
lon_dim <- ncdim_def("lon", "degrees_east",  lon)
lat_dim <- ncdim_def("lat", "degrees_north", lat)

# Define the output variable
pct_heavy_var <- ncvar_def("pct_heavy_precip", "%", list(lon_dim, lat_dim), missval = NA, prec = "double")

# Create the output NetCDF file
output_ncfile <- "VCP_DROUGHT_PRECIP95_PctHeavy_2015-2044_MPI-ESM1-2-HR.nc"
nc_new <- nc_create(output_ncfile, list(pct_heavy_var))

# Write data to the NetCDF file
ncvar_put(nc_new, "pct_heavy_precip", pct_heavy_precip)

# Add coordinate variables
ncvar_put(nc_new, "lon", lon)
ncvar_put(nc_new, "lat", lat)

# Close the NetCDF file
nc_close(nc_new)

rm(
    threshold_95,
    total_sum,
    heavy_sum,
    pct_heavy_precip,
    lon,
    lat
)

gc()

print("Percent precip from heavy rain days (annual) saved to NetCDF file.")
```

---


## Calculating average of the 5 general use projections 2045-2074

```{r}
library(stars)

# Define the NetCDF file paths
nc_files <- c(
  "VCP_DROUGHT_PRECIP95_PctHeavy_2045-2074_ACCESS.nc",
  "VCP_DROUGHT_PRECIP95_PctHeavy_2045-2074_ECEARTH.nc",
  "VCP_DROUGHT_PRECIP95_PctHeavy_2045-2074_FGOALS.nc",
  "VCP_DROUGHT_PRECIP95_PctHeavy_2045-2074_MIROC6.nc",
  "VCP_DROUGHT_PRECIP95_PctHeavy_2045-2074_MPI-ESM1-2-HR.nc")

# Load all files as stars objects
stars_list <- lapply(nc_files, read_stars)

# Ensure all grids match, and all files share the same units before merging
stopifnot(all(sapply(stars_list, function(x) all.equal(st_dimensions(x), st_dimensions(stars_list[[1]])))))

# Combine along a new "model" dimension
combined <- do.call(c, c(stars_list, along = "model"))

# Average across "model" (i.e., average % contribution at each grid cell)
# No unit conversion needed here -- the per-model outputs are already in percent
averaged_data <- st_apply(combined, MARGIN = setdiff(names(st_dimensions(combined)), "model"), FUN = mean, na.rm = TRUE)

# Save the averaged result as a new NetCDF file
write_stars(averaged_data, "VCP_DROUGHT_PRECIP95_PctHeavy_2045-2074_GENUSE.nc")
write_stars(averaged_data, "VCP_DROUGHT_PRECIP95_PctHeavy_2045-2074_GENUSE.tif")

print("Multi-model average (annual percent heavy-rain precip 2045-2074) saved.")
```

---


## Calculating average of the 5 general use projections 2015-2044

```{r}
library(stars)

# Define the NetCDF file paths
nc_files <- c(
  "VCP_DROUGHT_PRECIP95_PctHeavy_2015-2044_ACCESS.nc",
  "VCP_DROUGHT_PRECIP95_PctHeavy_2015-2044_ECEARTH.nc",
  "VCP_DROUGHT_PRECIP95_PctHeavy_2015-2044_FGOALS.nc",
  "VCP_DROUGHT_PRECIP95_PctHeavy_2015-2044_MIROC6.nc",
  "VCP_DROUGHT_PRECIP95_PctHeavy_2015-2044_MPI-ESM1-2-HR.nc")

# Load all files as stars objects
stars_list <- lapply(nc_files, read_stars)

# Ensure all grids match, and all files share the same units before merging
stopifnot(all(sapply(stars_list, function(x) all.equal(st_dimensions(x), st_dimensions(stars_list[[1]])))))

# Combine along a new "model" dimension
combined <- do.call(c, c(stars_list, along = "model"))

# Average across "model" (i.e., average % contribution at each grid cell)
# No unit conversion needed here -- the per-model outputs are already in percent
averaged_data <- st_apply(combined, MARGIN = setdiff(names(st_dimensions(combined)), "model"), FUN = mean, na.rm = TRUE)

# Save the averaged result as a new NetCDF file
write_stars(averaged_data, "VCP_DROUGHT_PRECIP95_PctHeavy_2015-2044_GENUSE.nc")
write_stars(averaged_data, "VCP_DROUGHT_PRECIP95_PctHeavy_2015-2044_GENUSE.tif")

print("Multi-model average (annual percent heavy-rain precip 2015-2044) saved.")
```

---


```{r}
library(terra)
library(ggplot2)
setwd("D:/Climate_Data/")
# Read NetCDF files

r1 <- rast("VCP_DROUGHT_PRECIP95_PctHeavy_2045-2074_GENUSE.nc")[[1]]
r2 <- rast("D:/Climate_Data/VCP_FLOOD_pctExtreme95_01A_GENUSE-AVE/VCP_FLOOD_PctExtreme95_01B_GENUSE-AVE_2045-2074.nc")[[1]]

# Extract values
x <- values(r1)
y <- values(r2)

# Remove NAs
keep <- complete.cases(x, y)
x <- x[keep]
y <- y[keep]

# Compute R²
r_squared <- cor(x, y)^2

# Plot
ggplot(data.frame(x, y), aes(x, y)) +
  geom_point(size = 0.3, alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  ggtitle(paste("R² =", round(r_squared, 4))) +
  xlab("NetCDF 1") +
  ylab("NetCDF 2") +
  theme_bw()
```

```{r}
library(terra)
library(ggplot2)

# Read NetCDF files

r1 <- rast("VCP_DROUGHT_PRECIP95_PctHeavy_2015-2044_GENUSE.nc")[[1]]
r2 <- rast("D:/Climate_Data/VCP_FLOOD_pctExtreme95_01A_GENUSE-AVE/VCP_FLOOD_PctExtreme95_01A_GENUSE-AVE_2015-2044.nc")[[1]]

# Extract values
x <- values(r1)
y <- values(r2)

# Remove NAs
keep <- complete.cases(x, y)
x <- x[keep]
y <- y[keep]

# Compute R²
r_squared <- cor(x, y)^2

# Plot
ggplot(data.frame(x, y), aes(x, y)) +
  geom_point(size = 0.3, alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  ggtitle(paste("R² =", round(r_squared, 4))) +
  xlab("NetCDF 1") +
  ylab("NetCDF 2") +
  theme_bw()
```


