---
  title: "Warm Nights without filter"
output: html_document
date: "2026-02-04"
author: "Sara Yen, Climate Services, CA Governor's Office of Land Use and Climate Innovation
editor_options: 
  chunk_output_type: console
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Extreme Heat Analysis for Vulnerable Communities Platform

## Data Description

This code utilizes downscaled climate data available from the Cal-Adapt Analytics Engine to generate indicators of extreme heat for the Vulnerable Communities Platform (CA Governor's Office of Land Use and Climate Innovation). Specifically, we use downscaled tmin data from from the five general use projections under SSP370 for two different time scales (2015-2044 and 2045-2074) and modeled historical baseline data sets for tmin and tmax from 1950-2014.

The data are available from the following links:

https://cadcat.s3.amazonaws.com/index.html#loca2/aaa-ca-hybrid/ 

## Frequency of extreme heat nights (as defined by the 98th percentile) for 2045-2074 (VCP 2050 timescale) ACCESS-CM2

Counts the number of extreme heat nights over June, July, and August of 2045-2074 for each location in the grid, where extreme heat nights are defined by an exceedance of the 98th historical (1950-2014) percentile of minimum temperature. 
Removed filter for only locations that have a historical 98th percentile of tmax >= 90 deg F are considered.

```{r}
library(terra)
library(ncdf4)

# File paths
#historical_tasmax_ncfile <- "ACCESSCM2/temp/tasmax.ACCESS-CM2.historical.r1i1p1f1.1950-2014.LOCA2_CA_hybrid_v20230115.nc"
historical_tasmin_ncfile <- "ACCESSCM2/temp/tasmin.ACCESS-CM2.historical.r1i1p1f1.1950-2014.LOCA2_CA_hybrid_v20230115.nc"
future_tasmin_ncfile <- "ACCESSCM2/temp/tasmin.ACCESS-CM2.ssp370.r1i1p1f1.2045-2074.LOCA2_CA_hybrid_v20230115.nc"

# Load historical tasmax and tasmin
r_tasmax_hist <- rast(historical_tasmax_ncfile)
r_tasmin_hist <- rast(historical_tasmin_ncfile)

# Open files
nc_hist_max <- nc_open(historical_tasmax_ncfile)

# Identify JJA months
time_origin <- as.Date("1900-01-01")
time_vals <- ncvar_get(nc_hist_max, "time")
dates <- time_origin + time_vals
jja_indices <- which(format(dates, "%m") %in% c("06", "07", "08"))

# Compute 98th percentile of tasmax for JJA (1950-2014)
#p90_tasmax <- app(r_tasmax_hist[[jja_indices]], fun = function(x) quantile(x, 0.98, na.rm = TRUE))

# Compute 98th percentile of tasmin for JJA (1950-2014)
p98_tasmin <- app(r_tasmin_hist[[jja_indices]], fun = function(x) quantile(x, 0.98, na.rm = TRUE))

# Filter locations where 98th percentile tasmax >= 90°F (305.4K)
##NOTE: I removed this filter for the Climate Tab
#valid_locations <- p90_tasmax >= 305.4

# Load future tasmin
r_tasmin_future <- rast(future_tasmin_ncfile)

# Open files
nc_fut_min <- nc_open(future_tasmin_ncfile)

# Get future JJA indices
time_vals_future <- ncvar_get(nc_fut_min, "time")
print(time_vals_future)
dates_future <- as.Date(time_vals_future, origin = "1900-01-01")
print(dates_future)
jja_indices_future <- which(format(dates_future, "%m") %in% c("06", "07", "08"))
print(jja_indices_future)

# Exceedance count
exceedance_count <- deepcopy(r_tasmin_future[[1]])
values(exceedance_count) <- 0

# Process in chunks
for (i in jja_indices_future) {
  chunk <- r_tasmin_future[[i]]
  exceedance_chunk <- chunk > p98_tasmin #removed valid locations filter here
  exceedance_count <- exceedance_count + exceedance_chunk
}

# Save exceedance count
out_ncfile <- "VCP_WarmNights_2045-2074_ACCESS.nc"
writeRaster(exceedance_count, out_ncfile, overwrite = TRUE, filetype="netcdf")
```

## Frequency of extreme heat nights (as defined by the 98th percentile) for 2015-2044 (VCP current timescale) ACCESS-CM2

Counts the number of extreme heat nights over June, July, and August of 2015-2044 for each location in the grid, where extreme heat nights are defined by an exceedance of the 98th historical (1950-2014) percentile of minimum temperature. Only locations that have a historical 90th percentile of tmax >= 90 deg F are considered.

```{r}
library(terra)
library(ncdf4)

# File paths
#historical_tasmax_ncfile <- "ACCESSCM2/temp/tasmax.ACCESS-CM2.historical.r1i1p1f1.1950-2014.LOCA2_CA_hybrid_v20230115.nc"
historical_tasmin_ncfile <- "ACCESSCM2/temp/tasmin.ACCESS-CM2.historical.r1i1p1f1.1950-2014.LOCA2_CA_hybrid_v20230115.nc"
future_tasmin_ncfile <- "ACCESSCM2/temp/tasmin.ACCESS-CM2.ssp370.r1i1p1f1.2015-2044.LOCA2_CA_hybrid_v20230115.nc"

# ------------------------------------------------------------------
# Terra memory options (safer for large NetCDF stacks)
# ------------------------------------------------------------------
dir.create("terra_tmp", showWarnings = FALSE)

terraOptions(
  progress = 1,
  memfrac = 0.4,       # reduce RAM pressure
  tempdir = "terra_tmp",
  todisk = TRUE        # force chunked disk processing
)

# Load historical tasmin
# ------------------------------------------------------------------
r_tasmin_hist <- rast(historical_tasmin_ncfile)

# ------------------------------------------------------------------
# Identify JJA months (historical period)
# ------------------------------------------------------------------
nc_hist_min <- nc_open(historical_tasmin_ncfile)

time_vals_hist <- ncvar_get(nc_hist_min, "time")
dates_hist <- as.Date(time_vals_hist, origin = "1900-01-01")

jja_indices_hist <- which(format(dates_hist, "%m") %in% c("06", "07", "08"))

nc_close(nc_hist_min)

# ------------------------------------------------------------------
# Subset historical JJA layers
# ------------------------------------------------------------------
r_hist_jja <- r_tasmin_hist[[jja_indices_hist]]

# ------------------------------------------------------------------
# Compute 98th percentile of historical JJA tasmin (1950–2014)
# ------------------------------------------------------------------
p98_tasmin <- quantile(
  r_hist_jja,
  probs = 0.98,
  na.rm = TRUE,
  filename = "p98_tasmin.tif",
  overwrite = TRUE
)

# ------------------------------------------------------------------
# Load future tasmin
# ------------------------------------------------------------------
r_tasmin_future <- rast(future_tasmin_ncfile)

# ------------------------------------------------------------------
# Identify JJA months (future period)
# ------------------------------------------------------------------
nc_fut_min <- nc_open(future_tasmin_ncfile)

time_vals_future <- ncvar_get(nc_fut_min, "time")
dates_future <- as.Date(time_vals_future, origin = "1900-01-01")

jja_indices_future <- which(format(dates_future, "%m") %in% c("06", "07", "08"))

nc_close(nc_fut_min)

# ------------------------------------------------------------------
# Subset future JJA layers
# ------------------------------------------------------------------
r_future_jja <- r_tasmin_future[[jja_indices_future]]

# ------------------------------------------------------------------
# Calculate exceedances (future > historical 98th percentile)
# ------------------------------------------------------------------
exceedance_count <- sum(
  r_future_jja > p98_tasmin,
  filename = "VCP_WarmNights_2015-2044_ACCESS.nc",
  overwrite = TRUE
)
```

```

## Frequency of extreme heat nights (as defined by the 98th percentile) for 2045-2074 (VCP 2050 timescale) EC-EARTH

#Counts the number of extreme heat nights over June, July, and August of 2045-2074 for each location in the grid, where extreme heat nights are defined by an exceedance of the 98th historical (1950-2014) percentile of minimum temperature. Only locations that have a historical 98th percentile of tmax >= 90 deg F are considered.

```{r}
library(terra)
library(ncdf4)

# File paths
#historical_tasmax_ncfile <- "ECEARTH/temp/tasmax.EC-Earth3.historical.r1i1p1f1.1950-2014.LOCA2_CA_hybrid_v20230115.nc"
historical_tasmin_ncfile <- "ECEARTH/temp/tasmin.EC-Earth3.historical.r1i1p1f1.1950-2014.LOCA2_CA_hybrid_v20230115.nc"
future_tasmin_ncfile <- "ECEARTH/temp/tasmin.EC-Earth3.ssp370.r1i1p1f1.2045-2074.LOCA2_CA_hybrid_v20230115.nc"

# ------------------------------------------------------------------
# Terra memory options (safer for large NetCDF stacks)
# ------------------------------------------------------------------
dir.create("terra_tmp", showWarnings = FALSE)

terraOptions(
  progress = 1,
  memfrac = 0.4,       # reduce RAM pressure
  tempdir = "terra_tmp",
  todisk = TRUE        # force chunked disk processing
)

# Load historical tasmin
# ------------------------------------------------------------------
r_tasmin_hist <- rast(historical_tasmin_ncfile)

# ------------------------------------------------------------------
# Identify JJA months (historical period)
# ------------------------------------------------------------------
nc_hist_min <- nc_open(historical_tasmin_ncfile)

time_vals_hist <- ncvar_get(nc_hist_min, "time")
dates_hist <- as.Date(time_vals_hist, origin = "1900-01-01")

jja_indices_hist <- which(format(dates_hist, "%m") %in% c("06", "07", "08"))

nc_close(nc_hist_min)

# ------------------------------------------------------------------
# Subset historical JJA layers
# ------------------------------------------------------------------
r_hist_jja <- r_tasmin_hist[[jja_indices_hist]]

# ------------------------------------------------------------------
# Compute 98th percentile of historical JJA tasmin (1950–2014)
# ------------------------------------------------------------------
p98_tasmin <- quantile(
  r_hist_jja,
  probs = 0.98,
  na.rm = TRUE,
  filename = "p98_tasmin.tif",
  overwrite = TRUE
)

# ------------------------------------------------------------------
# Load future tasmin
# ------------------------------------------------------------------
r_tasmin_future <- rast(future_tasmin_ncfile)

# ------------------------------------------------------------------
# Identify JJA months (future period)
# ------------------------------------------------------------------
nc_fut_min <- nc_open(future_tasmin_ncfile)

time_vals_future <- ncvar_get(nc_fut_min, "time")
dates_future <- as.Date(time_vals_future, origin = "1900-01-01")

jja_indices_future <- which(format(dates_future, "%m") %in% c("06", "07", "08"))

nc_close(nc_fut_min)

# ------------------------------------------------------------------
# Subset future JJA layers
# ------------------------------------------------------------------
r_future_jja <- r_tasmin_future[[jja_indices_future]]

# ------------------------------------------------------------------
# Calculate exceedances (future > historical 98th percentile)
# ------------------------------------------------------------------
exceedance_count <- sum(
  r_future_jja > p98_tasmin,
  filename = "VCP_WarmNights_2045-2074_ECEARTH.nc",
  overwrite = TRUE
)
```

## Frequency of extreme heat nights (98th percentile)
## 2015–2044 (VCP timescale) – EC-EARTH
## Counts JJA exceedances of historical (1950–2014) 98th percentile tasmin

library(terra)
library(ncdf4)

# ------------------------------------------------------------------
# File paths
# ------------------------------------------------------------------
historical_tasmin_ncfile <- "ECEARTH/temp/tasmin.EC-Earth3.historical.r1i1p1f1.1950-2014.LOCA2_CA_hybrid_v20230115.nc"
future_tasmin_ncfile     <- "ECEARTH/temp/tasmin.EC-Earth3.ssp370.r1i1p1f1.2015-2044.LOCA2_CA_hybrid_v20230115.nc"

# ------------------------------------------------------------------
# Terra memory options (safer for large NetCDF stacks)
# ------------------------------------------------------------------
dir.create("terra_tmp", showWarnings = FALSE)

terraOptions(
  progress = 1,
  memfrac = 0.4,       # reduce RAM pressure
  tempdir = "terra_tmp",
  todisk = TRUE        # force chunked disk processing
)

# Load historical tasmin
# ------------------------------------------------------------------
r_tasmin_hist <- rast(historical_tasmin_ncfile)

# ------------------------------------------------------------------
# Identify JJA months (historical period)
# ------------------------------------------------------------------
nc_hist_min <- nc_open(historical_tasmin_ncfile)

time_vals_hist <- ncvar_get(nc_hist_min, "time")
dates_hist <- as.Date(time_vals_hist, origin = "1900-01-01")

jja_indices_hist <- which(format(dates_hist, "%m") %in% c("06", "07", "08"))

nc_close(nc_hist_min)

# ------------------------------------------------------------------
# Subset historical JJA layers
# ------------------------------------------------------------------
r_hist_jja <- r_tasmin_hist[[jja_indices_hist]]

# ------------------------------------------------------------------
# Compute 98th percentile of historical JJA tasmin (1950–2014)
# ------------------------------------------------------------------
p98_tasmin <- quantile(
  r_hist_jja,
  probs = 0.98,
  na.rm = TRUE,
  filename = "p98_tasmin.tif",
  overwrite = TRUE
)

# ------------------------------------------------------------------
# Load future tasmin
# ------------------------------------------------------------------
r_tasmin_future <- rast(future_tasmin_ncfile)

# ------------------------------------------------------------------
# Identify JJA months (future period)
# ------------------------------------------------------------------
nc_fut_min <- nc_open(future_tasmin_ncfile)

time_vals_future <- ncvar_get(nc_fut_min, "time")
dates_future <- as.Date(time_vals_future, origin = "1900-01-01")

jja_indices_future <- which(format(dates_future, "%m") %in% c("06", "07", "08"))

nc_close(nc_fut_min)

# ------------------------------------------------------------------
# Subset future JJA layers
# ------------------------------------------------------------------
r_future_jja <- r_tasmin_future[[jja_indices_future]]

# ------------------------------------------------------------------
# Calculate exceedances (future > historical 98th percentile)
# ------------------------------------------------------------------
exceedance_count <- sum(
  r_future_jja > p98_tasmin,
  filename = "VCP_WarmNights_2015-2044_ECEARTH.nc",
  overwrite = TRUE
)
```

## Frequency of extreme heat nights (as defined by the 98th percentile) for 2045-2074 (VCP 2050 timescale) FGOALS

Counts the number of extreme heat nights over June, July, and August of 2045-2074 for each location in the grid, where extreme heat nights are defined by an exceedance of the 98th historical (1950-2014) percentile of minimum temperature. Only locations that have a historical 98th percentile of tmax >= 90 deg F are considered.

```{r}
library(terra)
library(ncdf4)

# File paths
#historical_tasmax_ncfile <- "FGOALS/temp/tasmax.FGOALS-g3.historical.r1i1p1f1.1950-2014.LOCA2_CA_hybrid_v20230115.nc"
historical_tasmin_ncfile <- "FGOALS/temp/tasmin.FGOALS-g3.historical.r1i1p1f1.1950-2014.LOCA2_CA_hybrid_v20230115.nc"
future_tasmin_ncfile <- "FGOALS/temp/tasmin.FGOALS-g3.ssp370.r1i1p1f1.2045-2074.LOCA2_CA_hybrid_v20230115.nc"

# ------------------------------------------------------------------
# Terra memory options (safer for large NetCDF stacks)
# ------------------------------------------------------------------
dir.create("terra_tmp", showWarnings = FALSE)

terraOptions(
  progress = 1,
  memfrac = 0.4,       # reduce RAM pressure
  tempdir = "terra_tmp",
  todisk = TRUE        # force chunked disk processing
)

# ------------------------------------------------------------------
# Load historical tasmin
# ------------------------------------------------------------------
r_tasmin_hist <- rast(historical_tasmin_ncfile)

# ------------------------------------------------------------------
# Identify JJA months (historical period)
# ------------------------------------------------------------------
nc_hist_min <- nc_open(historical_tasmin_ncfile)

time_vals_hist <- ncvar_get(nc_hist_min, "time")
dates_hist <- as.Date(time_vals_hist, origin = "1900-01-01")

jja_indices_hist <- which(format(dates_hist, "%m") %in% c("06", "07", "08"))

nc_close(nc_hist_min)

# ------------------------------------------------------------------
# Subset historical JJA layers
# ------------------------------------------------------------------
r_hist_jja <- r_tasmin_hist[[jja_indices_hist]]

# ------------------------------------------------------------------
# Compute 98th percentile of historical JJA tasmin (1950–2014)
# ------------------------------------------------------------------
p98_tasmin <- quantile(
  r_hist_jja,
  probs = 0.98,
  na.rm = TRUE,
  filename = "p98_tasmin.tif",
  overwrite = TRUE
)

# ------------------------------------------------------------------
# Load future tasmin
# ------------------------------------------------------------------
r_tasmin_future <- rast(future_tasmin_ncfile)

# ------------------------------------------------------------------
# Identify JJA months (future period)
# ------------------------------------------------------------------
nc_fut_min <- nc_open(future_tasmin_ncfile)

time_vals_future <- ncvar_get(nc_fut_min, "time")
dates_future <- as.Date(time_vals_future, origin = "1900-01-01")

jja_indices_future <- which(format(dates_future, "%m") %in% c("06", "07", "08"))

nc_close(nc_fut_min)

# ------------------------------------------------------------------
# Subset future JJA layers
# ------------------------------------------------------------------
r_future_jja <- r_tasmin_future[[jja_indices_future]]

# ------------------------------------------------------------------
# Calculate exceedances (future > historical 98th percentile)
# ------------------------------------------------------------------
exceedance_count <- sum(
  r_future_jja > p98_tasmin,
  filename = "VCP_WarmNights_2045-2074_FGOALS.nc",
  overwrite = TRUE
)



## Frequency of extreme heat nights (as defined by the 98th percentile) for 2015-2044 (VCP current timescale) FGOALS

Counts the number of extreme heat nights over June, July, and August of 2015-2044 for each location in the grid, where extreme heat nights are defined by an exceedance of the 98th historical (1950-2014) percentile of minimum temperature. Only locations that have a historical 90th percentile of tmax >= 90 deg F are considered.

```{r}
library(terra)
library(ncdf4)

# File paths
historical_tasmin_ncfile <- "FGOALS/temp/tasmin.FGOALS-g3.historical.r1i1p1f1.1950-2014.LOCA2_CA_hybrid_v20230115.nc"
future_tasmin_ncfile <- "FGOALS/temp/tasmin.FGOALS-g3.ssp370.r1i1p1f1.2015-2044.LOCA2_CA_hybrid_v20230115.nc"

# ------------------------------------------------------------------
# Terra memory options (safer for large NetCDF stacks)
# ------------------------------------------------------------------
dir.create("terra_tmp", showWarnings = FALSE)

terraOptions(
  progress = 1,
  memfrac = 0.4,       # reduce RAM pressure
  tempdir = "terra_tmp",
  todisk = TRUE        # force chunked disk processing
)

# Load historical tasmin
# ------------------------------------------------------------------
r_tasmin_hist <- rast(historical_tasmin_ncfile)

# ------------------------------------------------------------------
# Identify JJA months (historical period)
# ------------------------------------------------------------------
nc_hist_min <- nc_open(historical_tasmin_ncfile)

time_vals_hist <- ncvar_get(nc_hist_min, "time")
dates_hist <- as.Date(time_vals_hist, origin = "1900-01-01")

jja_indices_hist <- which(format(dates_hist, "%m") %in% c("06", "07", "08"))

nc_close(nc_hist_min)

# ------------------------------------------------------------------
# Subset historical JJA layers
# ------------------------------------------------------------------
r_hist_jja <- r_tasmin_hist[[jja_indices_hist]]

# ------------------------------------------------------------------
# Compute 98th percentile of historical JJA tasmin (1950–2014)
# ------------------------------------------------------------------
p98_tasmin <- quantile(
  r_hist_jja,
  probs = 0.98,
  na.rm = TRUE,
  filename = "p98_tasmin.tif",
  overwrite = TRUE
)

# ------------------------------------------------------------------
# Load future tasmin
# ------------------------------------------------------------------
r_tasmin_future <- rast(future_tasmin_ncfile)

# ------------------------------------------------------------------
# Identify JJA months (future period)
# ------------------------------------------------------------------
nc_fut_min <- nc_open(future_tasmin_ncfile)

time_vals_future <- ncvar_get(nc_fut_min, "time")
dates_future <- as.Date(time_vals_future, origin = "1900-01-01")

jja_indices_future <- which(format(dates_future, "%m") %in% c("06", "07", "08"))

nc_close(nc_fut_min)

# ------------------------------------------------------------------
# Subset future JJA layers
# ------------------------------------------------------------------
r_future_jja <- r_tasmin_future[[jja_indices_future]]

# ------------------------------------------------------------------
# Calculate exceedances (future > historical 98th percentile)
# ------------------------------------------------------------------
exceedance_count <- sum(
  r_future_jja > p98_tasmin,
  filename = "VCP_WarmNights_2015-2044_FGOALS.nc",
  overwrite = TRUE
)
```



## Frequency of extreme heat nights (as defined by the 98th percentile) for 2045-2074 (VCP 2050 timescale) MIROC6

Counts the number of extreme heat nights over June, July, and August of 2045-2074 for each location in the grid, where extreme heat nights are defined by an exceedance of the 98th historical (1950-2014) percentile of minimum temperature. Only locations that have a historical 98th percentile of tmax >= 90 deg F are considered.

```{r}
library(terra)
library(ncdf4)

# File paths
historical_tasmin_ncfile <- "MIROC6/temp/tasmin.MIROC6.historical.r1i1p1f1.1950-2014.LOCA2_CA_hybrid_v20230115.nc"
future_tasmin_ncfile <- "MIROC6/temp/tasmin.MIROC6.ssp370.r1i1p1f1.2045-2074.LOCA2_CA_hybrid_v20230115.nc"

# ------------------------------------------------------------------
# Terra memory options (safer for large NetCDF stacks)
# ------------------------------------------------------------------
dir.create("terra_tmp", showWarnings = FALSE)

terraOptions(
  progress = 1,
  memfrac = 0.4,       # reduce RAM pressure
  tempdir = "terra_tmp",
  todisk = TRUE        # force chunked disk processing
)

# Load historical tasmin
# ------------------------------------------------------------------
r_tasmin_hist <- rast(historical_tasmin_ncfile)

# ------------------------------------------------------------------
# Identify JJA months (historical period)
# ------------------------------------------------------------------
nc_hist_min <- nc_open(historical_tasmin_ncfile)

time_vals_hist <- ncvar_get(nc_hist_min, "time")
dates_hist <- as.Date(time_vals_hist, origin = "1900-01-01")

jja_indices_hist <- which(format(dates_hist, "%m") %in% c("06", "07", "08"))

nc_close(nc_hist_min)

# ------------------------------------------------------------------
# Subset historical JJA layers
# ------------------------------------------------------------------
r_hist_jja <- r_tasmin_hist[[jja_indices_hist]]

# ------------------------------------------------------------------
# Compute 98th percentile of historical JJA tasmin (1950–2014)
# ------------------------------------------------------------------
p98_tasmin <- quantile(
  r_hist_jja,
  probs = 0.98,
  na.rm = TRUE,
  filename = "p98_tasmin.tif",
  overwrite = TRUE
)

# ------------------------------------------------------------------
# Load future tasmin
# ------------------------------------------------------------------
r_tasmin_future <- rast(future_tasmin_ncfile)

# ------------------------------------------------------------------
# Identify JJA months (future period)
# ------------------------------------------------------------------
nc_fut_min <- nc_open(future_tasmin_ncfile)

time_vals_future <- ncvar_get(nc_fut_min, "time")
dates_future <- as.Date(time_vals_future, origin = "1900-01-01")

jja_indices_future <- which(format(dates_future, "%m") %in% c("06", "07", "08"))

nc_close(nc_fut_min)

# ------------------------------------------------------------------
# Subset future JJA layers
# ------------------------------------------------------------------
r_future_jja <- r_tasmin_future[[jja_indices_future]]

# ------------------------------------------------------------------
# Calculate exceedances (future > historical 98th percentile)
# ------------------------------------------------------------------
exceedance_count <- sum(
  r_future_jja > p98_tasmin,
  filename = "VCP_WarmNights_2045-2074_MIROC6.nc"
)
```
```

## Frequency of extreme heat nights (as defined by the 98th percentile) for 2015-2044 (VCP current timescale) MIRCOC6

Counts the number of extreme heat nights over June, July, and August of 2015-2044 for each location in the grid, where extreme heat nights are defined by an exceedance of the 98th historical (1950-2014) percentile of minimum temperature. Only locations that have a historical 90th percentile of tmax >= 90 deg F are considered.

```{r}
library(terra)
library(ncdf4)

# File paths
#historical_tasmax_ncfile <- "tasmax.MIROC6.historical.r1i1p1f1.1950-2014.LOCA2_CA_hybrid_v20230115.nc"
historical_tasmin_ncfile <- "MIROC6/temp/tasmin.MIROC6.historical.r1i1p1f1.1950-2014.LOCA2_CA_hybrid_v20230115.nc"
future_tasmin_ncfile <- "MIROC6/temp/tasmin.MIROC6.ssp370.r1i1p1f1.2015-2044.LOCA2_CA_hybrid_v20230115.nc"

# ------------------------------------------------------------------
# Terra memory options (safer for large NetCDF stacks)
# ------------------------------------------------------------------
dir.create("terra_tmp", showWarnings = FALSE)

terraOptions(
  progress = 1,
  memfrac = 0.4,       # reduce RAM pressure
  tempdir = "terra_tmp",
  todisk = TRUE        # force chunked disk processing
)

# Load historical tasmin
# ------------------------------------------------------------------
r_tasmin_hist <- rast(historical_tasmin_ncfile)

# ------------------------------------------------------------------
# Identify JJA months (historical period)
# ------------------------------------------------------------------
nc_hist_min <- nc_open(historical_tasmin_ncfile)

time_vals_hist <- ncvar_get(nc_hist_min, "time")
dates_hist <- as.Date(time_vals_hist, origin = "1900-01-01")

jja_indices_hist <- which(format(dates_hist, "%m") %in% c("06", "07", "08"))

nc_close(nc_hist_min)

# ------------------------------------------------------------------
# Subset historical JJA layers
# ------------------------------------------------------------------
r_hist_jja <- r_tasmin_hist[[jja_indices_hist]]

# ------------------------------------------------------------------
# Compute 98th percentile of historical JJA tasmin (1950–2014)
# ------------------------------------------------------------------
p98_tasmin <- quantile(
  r_hist_jja,
  probs = 0.98,
  na.rm = TRUE,
  filename = "p98_tasmin.tif",
  overwrite = TRUE
)

# ------------------------------------------------------------------
# Load future tasmin
# ------------------------------------------------------------------
r_tasmin_future <- rast(future_tasmin_ncfile)

# ------------------------------------------------------------------
# Identify JJA months (future period)
# ------------------------------------------------------------------
nc_fut_min <- nc_open(future_tasmin_ncfile)

time_vals_future <- ncvar_get(nc_fut_min, "time")
dates_future <- as.Date(time_vals_future, origin = "1900-01-01")

jja_indices_future <- which(format(dates_future, "%m") %in% c("06", "07", "08"))

nc_close(nc_fut_min)

# ------------------------------------------------------------------
# Subset future JJA layers
# ------------------------------------------------------------------
r_future_jja <- r_tasmin_future[[jja_indices_future]]

# ------------------------------------------------------------------
# Calculate exceedances (future > historical 98th percentile)
# ------------------------------------------------------------------
exceedance_count <- sum(
  r_future_jja > p98_tasmin,
  filename = "VCP_WarmNights_2015-2044_MIROC6.nc"
)
```

```

## Frequency of extreme heat nights (as defined by the 98th percentile) for 2045-2074 (VCP 2050 timescale) MPI-ESM1-2-HR

Counts the number of extreme heat nights over June, July, and August of 2045-2074 for each location in the grid, where extreme heat nights are defined by an exceedance of the 98th historical (1950-2014) percentile of minimum temperature. Only locations that have a historical 98th percentile of tmax >= 90 deg F are considered.

```{r}
library(terra)
library(ncdf4)

# File paths
#historical_tasmax_ncfile <- "tasmax.MPI-ESM1-2-HR.historical.r3i1p1f1.1950-2014.LOCA2_CA_hybrid_v20230115.nc"
historical_tasmin_ncfile <- "MPIESM12/temp/tasmin.MPI-ESM1-2-HR.historical.r3i1p1f1.1950-2014.LOCA2_CA_hybrid_v20230115.nc"
future_tasmin_ncfile <- "MPIESM12/temp/tasmin.MPI-ESM1-2-HR.ssp370.r3i1p1f1.2045-2074.LOCA2_CA_hybrid_v20230115.nc"

# ------------------------------------------------------------------
# Terra memory options (safer for large NetCDF stacks)
# ------------------------------------------------------------------
dir.create("terra_tmp", showWarnings = FALSE)

terraOptions(
  progress = 1,
  memfrac = 0.4,       # reduce RAM pressure
  tempdir = "terra_tmp",
  todisk = TRUE        # force chunked disk processing
)

# Load historical tasmin
# ------------------------------------------------------------------
r_tasmin_hist <- rast(historical_tasmin_ncfile)

# ------------------------------------------------------------------
# Identify JJA months (historical period)
# ------------------------------------------------------------------
nc_hist_min <- nc_open(historical_tasmin_ncfile)

time_vals_hist <- ncvar_get(nc_hist_min, "time")
dates_hist <- as.Date(time_vals_hist, origin = "1900-01-01")

jja_indices_hist <- which(format(dates_hist, "%m") %in% c("06", "07", "08"))

nc_close(nc_hist_min)

# ------------------------------------------------------------------
# Subset historical JJA layers
# ------------------------------------------------------------------
r_hist_jja <- r_tasmin_hist[[jja_indices_hist]]

# ------------------------------------------------------------------
# Compute 98th percentile of historical JJA tasmin (1950–2014)
# ------------------------------------------------------------------
p98_tasmin <- quantile(
  r_hist_jja,
  probs = 0.98,
  na.rm = TRUE,
  filename = "p98_tasmin.tif",
  overwrite = TRUE
)

# ------------------------------------------------------------------
# Load future tasmin
# ------------------------------------------------------------------
r_tasmin_future <- rast(future_tasmin_ncfile)

# ------------------------------------------------------------------
# Identify JJA months (future period)
# ------------------------------------------------------------------
nc_fut_min <- nc_open(future_tasmin_ncfile)

time_vals_future <- ncvar_get(nc_fut_min, "time")
dates_future <- as.Date(time_vals_future, origin = "1900-01-01")

jja_indices_future <- which(format(dates_future, "%m") %in% c("06", "07", "08"))

nc_close(nc_fut_min)

# ------------------------------------------------------------------
# Subset future JJA layers
# ------------------------------------------------------------------
r_future_jja <- r_tasmin_future[[jja_indices_future]]

# ------------------------------------------------------------------
# Calculate exceedances (future > historical 98th percentile)
# ------------------------------------------------------------------
exceedance_count <- sum(
  r_future_jja > p98_tasmin,
  filename = "VCP_WarmNights_2045-2074_MPIESM12.nc"
)
```
```

## Frequency of extreme heat nights (as defined by the 98th percentile) for 2015-2044 (VCP current timescale) MPI-ESM1-2-HR

Counts the number of extreme heat nights over June, July, and August of 2015-2044 for each location in the grid, where extreme heat nights are defined by an exceedance of the 98th historical (1950-2014) percentile of minimum temperature. Only locations that have a historical 90th percentile of tmax >= 90 deg F are considered.

```{r}
library(terra)
library(ncdf4)

# File paths
#historical_tasmax_ncfile <- "tasmax.MPI-ESM1-2-HR.historical.r3i1p1f1.1950-2014.LOCA2_CA_hybrid_v20230115.nc"
historical_tasmin_ncfile <- "tasmin.MPI-ESM1-2-HR.historical.r3i1p1f1.1950-2014.LOCA2_CA_hybrid_v20230115.nc"
future_tasmin_ncfile <- "MPIESM12/temp/tasmin.MPI-ESM1-2-HR.ssp370.r3i1p1f1.2015-2044.LOCA2_CA_hybrid_v20230115.nc"

# ------------------------------------------------------------------
# Terra memory options (safer for large NetCDF stacks)
# ------------------------------------------------------------------
dir.create("terra_tmp", showWarnings = FALSE)

terraOptions(
  progress = 1,
  memfrac = 0.4,       # reduce RAM pressure
  tempdir = "terra_tmp",
  todisk = TRUE        # force chunked disk processing
)

# Load historical tasmin
# ------------------------------------------------------------------
r_tasmin_hist <- rast(historical_tasmin_ncfile)

# ------------------------------------------------------------------
# Identify JJA months (historical period)
# ------------------------------------------------------------------
nc_hist_min <- nc_open(historical_tasmin_ncfile)

time_vals_hist <- ncvar_get(nc_hist_min, "time")
dates_hist <- as.Date(time_vals_hist, origin = "1900-01-01")

jja_indices_hist <- which(format(dates_hist, "%m") %in% c("06", "07", "08"))

nc_close(nc_hist_min)

# ------------------------------------------------------------------
# Subset historical JJA layers
# ------------------------------------------------------------------
r_hist_jja <- r_tasmin_hist[[jja_indices_hist]]

# ------------------------------------------------------------------
# Compute 98th percentile of historical JJA tasmin (1950–2014)
# ------------------------------------------------------------------
p98_tasmin <- quantile(
  r_hist_jja,
  probs = 0.98,
  na.rm = TRUE,
  filename = "p98_tasmin.tif",
  overwrite = TRUE
)

# ------------------------------------------------------------------
# Load future tasmin
# ------------------------------------------------------------------
r_tasmin_future <- rast(future_tasmin_ncfile)

# ------------------------------------------------------------------
# Identify JJA months (future period)
# ------------------------------------------------------------------
nc_fut_min <- nc_open(future_tasmin_ncfile)

time_vals_future <- ncvar_get(nc_fut_min, "time")
dates_future <- as.Date(time_vals_future, origin = "1900-01-01")

jja_indices_future <- which(format(dates_future, "%m") %in% c("06", "07", "08"))

nc_close(nc_fut_min)

# ------------------------------------------------------------------
# Subset future JJA layers
# ------------------------------------------------------------------
r_future_jja <- r_tasmin_future[[jja_indices_future]]

# ------------------------------------------------------------------
# Calculate exceedances (future > historical 98th percentile)
# ------------------------------------------------------------------
exceedance_count <- sum(
  r_future_jja > p98_tasmin,
  filename = "VCP_WarmNights_2015-2044_MPIESM12.nc"
)
```


#--------
#Turn nc files into tif files
#--------

r <- rast(
  "VCP_WarmNights_2045-2074_ECEARTH.nc",
  subds = "exceedance_count"
)

crs(r)

#crs(r) <- "EPSG:4326"

writeRaster(r, "WarmNights_2045-2074_ECEARTH.tif", overwrite = TRUE)



## Calculating average of the 5 general use projections 2045-2074

```{r}
install.packages("stars")
library(stars)


# Define the NetCDF file paths
nc_files <- c(
  "VCP_WarmNights_2045-2074_ACCESS.nc", 
  "VCP_WarmNights_2045-2074_ECEARTH.nc", 
  "VCP_WarmNights_2045-2074_FGOALS.nc",
  "VCP_WarmNights_2045-2074_MIROC6.nc",
  "VCP_WarmNights_2045-2074_MPIESM12.nc"
)

# Load all files as stars objects
stars_list <- lapply(nc_files, read_stars)

# Optional: check variable names
# print(names(stars_list[[1]]))  # should show something like "exceedance_count"

# Optional: ensure all grids match
stopifnot(all(sapply(stars_list, function(x) all.equal(st_dimensions(x), st_dimensions(stars_list[[1]])))))

# Combine along a new "model" dimension
combined <- do.call(c, c(stars_list, along = "model"))

# Average across "model" (i.e., average exceedance at each grid cell)
averaged_data <- st_apply(combined, MARGIN = setdiff(names(st_dimensions(combined)), "model"), FUN = mean, na.rm = TRUE)

# Save the averaged result as a new NetCDF file
write_stars(averaged_data, "VCP_WarmNights_2045-2074_GENUSE.nc")
```

## Calculating average of the 5 general use projections 2015-2044

```{r}
library(stars)

# Define the NetCDF file paths
nc_files <- c(
  "VCP_WarmNights_2015-2044_ACCESS.nc", 
  "VCP_WarmNights_2015-2044_ECEARTH.nc", 
  "VCP_WarmNights_2015-2044_FGOALS.nc",
  "VCP_WarmNights_2015-2044_MIROC6.nc",
  "VCP_WarmNights_2015-2044_MPIESM12.nc"
)

# Load all files as stars objects
stars_list <- lapply(nc_files, read_stars)

# Optional: check variable names
# print(names(stars_list[[1]]))  # should show something like "exceedance_count"

# Optional: ensure all grids match
stopifnot(all(sapply(stars_list, function(x) all.equal(st_dimensions(x), st_dimensions(stars_list[[1]])))))

# Combine along a new "model" dimension
combined <- do.call(c, c(stars_list, along = "model"))

# Average across "model" (i.e., average exceedance at each grid cell)
averaged_data <- st_apply(combined, MARGIN = setdiff(names(st_dimensions(combined)), "model"), FUN = mean, na.rm = TRUE)

# Save the averaged result as a new NetCDF file
write_stars(averaged_data, "VCP_WarmNights_2015-2044_GENUSE.nc")
```


#--------
#Turn nc files into tif files
#--------

r <- rast(
  "VCP_WarmNights_2045-2074_GENUSE.nc",
  subds = "exceedance_count"
)

crs(r)

#crs(r) <- "EPSG:4326"

writeRaster(r, "WarmNights_2045-2074_GENUSE.tif", overwrite = TRUE)


r <- rast(
  "VCP_WarmNights_2015-2044_GENUSE.nc",
  subds = "exceedance_count"
)

crs(r)

#crs(r) <- "EPSG:4326"

writeRaster(r, "WarmNights_2015-2044_GENUSE.tif", overwrite = TRUE)
