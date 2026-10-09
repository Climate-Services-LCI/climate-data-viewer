---
  title: "Percent Precip VWD"
output: html_document
date: "2026-02-23"
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

## Contribution to total precipitation from very wet days for 2045-2074 (2050) ACCESS-CM2

#Determines the percent contribution to total precipitation from very wet days 
#(days over 95th percentile historical percentile (1950-2014)) 
#from 2045-2074 at each grid location.


library(terra)
library(ncdf4)

#Read units 
nc_data <- nc_open("ACCESSCM2/precip/pr.ACCESS-CM2.historical.r1i1p1f1.1950-2014.LOCA2_CA_hybrid_v20230115.nc")
head(nc_data)
#units are kg/m^2/s = which is a rate
#units I want: inches of precipitation for each day at each grid location 

###ACCESS 2045###

# File paths
hist_file <- "ACCESSCM2/precip/pr.ACCESS-CM2.historical.r1i1p1f1.1950-2014.LOCA2_CA_hybrid_v20230115.nc"
future_file <- "ACCESSCM2/precip/pr.ACCESS-CM2.ssp370.r1i1p1f1.2045-2074.LOCA2_CA_hybrid_v20230115.nc"

# Load historical and future precipitation data as SpatRaster objects
r_hist <- rast(hist_file)    # Historical daily precip (1950-2014)
r_future <- rast(future_file) # Future daily precip (2045-2074)

# Calculate 95th percentile threshold for each grid cell from historical data
cat("Calculating 95th percentile threshold per grid cell from historical data...\n")
p95_hist <- app(r_hist, fun = function(x) quantile(x, probs=0.95, na.rm=TRUE))

# Calculate total precipitation over the future period at each grid cell
cat("Calculating total precipitation for future period...\n")
total_precip <- app(r_future, fun = sum, na.rm=TRUE)

# Initialize raster to accumulate precipitation from very wet days
very_wet_sum <- rast(r_future, nlyrs=1)
values(very_wet_sum) <- 0

# Loop through each day in future data, add precip on days exceeding threshold
cat("Summing precipitation on very wet days in future period...\n")
for (i in 1:nlyr(r_future)) {
  future_day <- r_future[[i]]
  
  # Mask of where precipitation > historical 95th percentile
  exceed_mask <- future_day > p95_hist
  
  # Keep precip only where mask is TRUE, else NA
  very_wet_day_precip <- mask(future_day, exceed_mask, maskvalue=FALSE)
  
  # Add this day's very wet precip to running sum, ignoring NA
                                                                                                                                                                  very_wet_sum <- very_wet_sum + very_wet_day_precip
                                                                                                                                                                  }

# Calculate percent contribution of very wet days
cat("Calculating percent contribution raster...\n")
percent_contrib <- (very_wet_sum / total_precip) * 100

# Clean up: replace NaN and infinite with NA
percent_contrib[!is.finite(percent_contrib)] <- NA

# Optional: clamp percent contribution between 0 and 100
percent_contrib <- clamp(percent_contrib, lower=0, upper=100, values=FALSE)

# Save the output raster to NetCDF
output_file <- "Precip_VWD_2045-2074_ACCESS.nc"
cat("Saving results to:", output_file, "\n")
writeRaster(percent_contrib, filename=output_file, overwrite=TRUE, filetype="netcdf")

#cat("All done!\n")
```

###TROUBLESHOOTING - ACCESS 2045###

library(terra)

# Improve terra performance
terraOptions(progress=1)      # show progress
terraOptions(memfrac=0.8)     # use up to 80% RAM
terraOptions(tempdir="terra_tmp")  # optional: custom temp dir

# File paths
hist_file <- "ACCESSCM2/precip/pr.ACCESS-CM2.historical.r1i1p1f1.1950-2014.LOCA2_CA_hybrid_v20230115.nc"
future_file <- "ACCESSCM2/precip/pr.ACCESS-CM2.ssp370.r1i1p1f1.2045-2074.LOCA2_CA_hybrid_v20230115.nc"

# Load data
r_hist <- rast(hist_file)
r_future <- rast(future_file)

# -----------------------------------------------------------
# 1️⃣  Compute historical 95th percentile (per grid cell)
#    (Write to disk — avoids recomputing if script crashes)
# -----------------------------------------------------------
cat("Calculating 95th percentile threshold...\n")

p95_hist <- app(
  r_hist,
  quantile,
  probs=0.95,
  na.rm=TRUE,
  filename="p95_hist.tif",
  overwrite=TRUE
)

# -----------------------------------------------------------
# 2️⃣  Compute total future precipitation (write to disk)
# -----------------------------------------------------------
cat("Calculating total future precipitation...\n")

total_precip <- app(
  r_future,
  sum,
  na.rm=TRUE,
  filename="total_future_precip.tif",
  overwrite=TRUE
)

# -----------------------------------------------------------
# 3️⃣  Keep only precipitation above historical 95th percentile
#     (Fastest method: ifel)
# -----------------------------------------------------------
cat("Extracting very wet day precipitation...\n")

very_wet_stack <- ifel(
  r_future > p95_hist,
  r_future,
  0,
  filename="very_wet_stack.tif",
  overwrite=TRUE
)

# -----------------------------------------------------------
# 4️⃣  Sum very wet precipitation over time
# -----------------------------------------------------------
cat("Summing very wet precipitation...\n")

very_wet_sum <- app(
  very_wet_stack,
  sum,
  filename="very_wet_sum.tif",
  overwrite=TRUE
)

# -----------------------------------------------------------
# 5️⃣  Percent contribution
# -----------------------------------------------------------
cat("Calculating percent contribution...\n")

percent_contrib <- (very_wet_sum / total_precip) * 100
percent_contrib[!is.finite(percent_contrib)] <- NA
percent_contrib <- clamp(percent_contrib, 0, 100)

# -----------------------------------------------------------
# 6️⃣  Save final NetCDF
# -----------------------------------------------------------
output_file <- "Precip_VWD_2045-2074_ACCESS.nc"

cat("Saving results to:", output_file, "\n")

writeRaster(
  percent_contrib,
  filename=output_file,
  overwrite=TRUE,
  filetype="netcdf"
)

cat("Done.\n")




###ACCESS 2015###

##OLD CODE - SKIP TO NEW CODE ###

# File paths
#hist_file <- "ACCESSCM2/precip/pr.ACCESS-CM2.historical.r1i1p1f1.1950-2014.LOCA2_CA_hybrid_v20230115.nc"
future_file <- "ACCESSCM2/precip/pr.ACCESS-CM2.ssp370.r1i1p1f1.2015-2044.LOCA2_CA_hybrid_v20230115.nc"

# Load historical and future precipitation data as SpatRaster objects
#r_hist <- rast(hist_file)    # Historical daily precip (1950-2014)
r_future <- rast(future_file) # Future daily precip (2045-2074)

# Calculate 95th percentile threshold for each grid cell from historical data
#at("Calculating 95th percentile threshold per grid cell from historical data...\n")
#p95_hist <- app(r_hist, fun = function(x) quantile(x, probs=0.95, na.rm=TRUE))

# Calculate total precipitation over the future period at each grid cell
cat("Calculating total precipitation for future period...\n")
total_precip <- app(r_future, fun = sum, na.rm=TRUE)

# Initialize raster to accumulate precipitation from very wet days
very_wet_sum <- rast(r_future, nlyrs=1)
values(very_wet_sum) <- 0

# Loop through each day in future data, add precip on days exceeding threshold
cat("Summing precipitation on very wet days in future period...\n")
for (i in 1:nlyr(r_future)) {
  future_day <- r_future[[i]]
  
  # Mask of where precipitation > historical 95th percentile
  exceed_mask <- future_day > p95_hist
  
  # Keep precip only where mask is TRUE, else NA
  very_wet_day_precip <- mask(future_day, exceed_mask, maskvalue=FALSE)
  
  # Add this day's very wet precip to running sum, ignoring NA
  very_wet_sum <- very_wet_sum + very_wet_day_precip
}

# Calculate percent contribution of very wet days
cat("Calculating percent contribution raster...\n")
percent_contrib <- (very_wet_sum / total_precip) * 100

# Clean up: replace NaN and infinite with NA
percent_contrib[!is.finite(percent_contrib)] <- NA

# Optional: clamp percent contribution between 0 and 100
percent_contrib <- clamp(percent_contrib, lower=0, upper=100, values=FALSE)

# Save the output raster to NetCDF
output_file <- "Precip_VWD_2015-2044_ACCESS.nc"
cat("Saving results to:", output_file, "\n")
writeRaster(percent_contrib, filename=output_file, overwrite=TRUE, filetype="netcdf")

#NEW CODE - TROUBLESHOOTING ACCESS 2015#
#-----------------------------------------------------------
  # 2️⃣  Compute total future precipitation (write to disk)
  # -----------------------------------------------------------
cat("Calculating total future precipitation...\n")

total_precip <- app(
  r_future,
  sum,
  na.rm=TRUE,
  filename="total_future_precip.tif",
  overwrite=TRUE
)

# -----------------------------------------------------------
# 3️⃣  Keep only precipitation above historical 95th percentile
#     (Fastest method: ifel)
# -----------------------------------------------------------
cat("Extracting very wet day precipitation...\n")

p95_hist <- rast("p95_hist.tif")


very_wet_stack <- ifel(
  r_future > p95_hist,
  r_future,
  0,
  filename="very_wet_stack.tif",
  overwrite=TRUE
)

# -----------------------------------------------------------
# 4️⃣  Sum very wet precipitation over time
# -----------------------------------------------------------
cat("Summing very wet precipitation...\n")

very_wet_sum <- app(
  very_wet_stack,
  sum,
  filename="very_wet_sum.tif",
  overwrite=TRUE
)

# -----------------------------------------------------------
# 5️⃣  Percent contribution
# -----------------------------------------------------------
cat("Calculating percent contribution...\n")

percent_contrib <- (very_wet_sum / total_precip) * 100
percent_contrib[!is.finite(percent_contrib)] <- NA
percent_contrib <- clamp(percent_contrib, 0, 100)

# -----------------------------------------------------------
# 6️⃣  Save final NetCDF
# -----------------------------------------------------------
output_file <- "Precip_VWD_2015-2044_ACCESS.nc"

cat("Saving results to:", output_file, "\n")

writeRaster(
  percent_contrib,
  filename=output_file,
  overwrite=TRUE,
  filetype="netcdf"
)


###ECEARTH 2045###
install.packages("terra")
install.packages("ncdf4")
library(terra)
library(ncdf4)

# File paths
hist_file <- "ECEARTH/precip/pr.EC-Earth3.historical.r1i1p1f1.1950-2014.LOCA2_CA_hybrid_v20230115.nc"
future_file <- "ECEARTH/precip/pr.EC-Earth3.ssp370.r1i1p1f1.2045-2074.LOCA2_CA_hybrid_v20230115.nc"

library(terra)
library(ncdf4)

# Improve terra performance
terraOptions(progress=1)      # show progress
terraOptions(memfrac=0.8)     # use up to 80% RAM
terraOptions(tempdir="terra_tmp")  # optional: custom temp dir

# Load data
r_hist <- rast(hist_file)
r_future <- rast(future_file)

# -----------------------------------------------------------
# 1️⃣  Compute historical 95th percentile (per grid cell)
#    (Write to disk — avoids recomputing if script crashes)
# -----------------------------------------------------------
cat("Calculating 95th percentile threshold...\n")

p95_hist <- app(
  r_hist,
  quantile,
  probs=0.95,
  na.rm=TRUE,
  filename="p95_hist.tif",
  overwrite=TRUE
)

# -----------------------------------------------------------
# 2️⃣  Compute total future precipitation (write to disk)
# -----------------------------------------------------------
cat("Calculating total future precipitation...\n")

total_precip <- app(
  r_future,
  sum,
  na.rm=TRUE,
  filename="total_future_precip.tif",
  overwrite=TRUE
)

# -----------------------------------------------------------
# 3️⃣  Keep only precipitation above historical 95th percentile
#     (Fastest method: ifel)
# -----------------------------------------------------------
cat("Extracting very wet day precipitation...\n")

very_wet_stack <- ifel(
  r_future > p95_hist,
  r_future,
  0,
  filename="very_wet_stack.tif",
  overwrite=TRUE
)

# -----------------------------------------------------------
# 4️⃣  Sum very wet precipitation over time
# -----------------------------------------------------------
cat("Summing very wet precipitation...\n")

very_wet_sum <- app(
  very_wet_stack,
  sum,
  filename="very_wet_sum.tif",
  overwrite=TRUE
)

# -----------------------------------------------------------
# 5️⃣  Percent contribution
# -----------------------------------------------------------
cat("Calculating percent contribution...\n")

percent_contrib <- (very_wet_sum / total_precip) * 100
percent_contrib[!is.finite(percent_contrib)] <- NA
percent_contrib <- clamp(percent_contrib, 0, 100)

# -----------------------------------------------------------
# 6️⃣  Save final NetCDF
# -----------------------------------------------------------
output_file <- "Precip_VWD_2045-2074_ECEARTH.nc"

cat("Saving results to:", output_file, "\n")

writeRaster(
  percent_contrib,
  filename=output_file,
  overwrite=TRUE,
  filetype="netcdf"
)


###ECEARTH 2015###

# File paths
#hist_file <- "ECEARTH/precip/pr.EC-Earth3.historical.r1i1p1f1.1950-2014.LOCA2_CA_hybrid_v20230115.nc"
future_file <- "ECEARTH/precip/pr.EC-Earth3.ssp370.r1i1p1f1.2015-2044.LOCA2_CA_hybrid_v20230115.nc"

#-----------------------------------------------------------
# 2️⃣  Compute total future precipitation (write to disk)
# -----------------------------------------------------------
cat("Calculating total future precipitation...\n")

total_precip <- app(
  r_future,
  sum,
  na.rm=TRUE,
  filename="total_future_precip.tif",
  overwrite=TRUE
)

# -----------------------------------------------------------
# 3️⃣  Keep only precipitation above historical 95th percentile
#     (Fastest method: ifel)
# -----------------------------------------------------------
cat("Extracting very wet day precipitation...\n")

p95_hist <- rast("p95_hist.tif")


very_wet_stack <- ifel(
  r_future > p95_hist,
  r_future,
  0,
  filename="very_wet_stack.tif",
  overwrite=TRUE
)

# -----------------------------------------------------------
# 4️⃣  Sum very wet precipitation over time
# -----------------------------------------------------------
cat("Summing very wet precipitation...\n")

very_wet_sum <- app(
  very_wet_stack,
  sum,
  filename="very_wet_sum.tif",
  overwrite=TRUE
)

# -----------------------------------------------------------
# 5️⃣  Percent contribution
# -----------------------------------------------------------
cat("Calculating percent contribution...\n")

percent_contrib <- (very_wet_sum / total_precip) * 100
percent_contrib[!is.finite(percent_contrib)] <- NA
percent_contrib <- clamp(percent_contrib, 0, 100)

# -----------------------------------------------------------
# 6️⃣  Save final NetCDF
# -----------------------------------------------------------
output_file <- "Precip_VWD_2015-2044_ECEARTH.nc"

cat("Saving results to:", output_file, "\n")

writeRaster(
  percent_contrib,
  filename=output_file,
  overwrite=TRUE,
  filetype="netcdf"
)


###FGOALS 2045###

# File paths
hist_file <- "FGOALS/precip/pr.FGOALS-g3.historical.r1i1p1f1.1950-2014.LOCA2_CA_hybrid_v20230115.nc"
future_file <- "FGOALS/precip/pr.FGOALS-g3.ssp370.r1i1p1f1.2045-2074.LOCA2_CA_hybrid_v20230115.nc"

library(terra)
library(ncdf4)

# Improve terra performance
terraOptions(progress=1)      # show progress
terraOptions(memfrac=0.8)     # use up to 80% RAM
terraOptions(tempdir="terra_tmp")  # optional: custom temp dir

# Load data
r_hist <- rast(hist_file)
r_future <- rast(future_file)

# -----------------------------------------------------------
# 1️⃣  Compute historical 95th percentile (per grid cell)
#    (Write to disk — avoids recomputing if script crashes)
# -----------------------------------------------------------
cat("Calculating 95th percentile threshold...\n")

p95_hist <- app(
  r_hist,
  quantile,
  probs=0.95,
  na.rm=TRUE,
  filename="p95_hist.tif",
  overwrite=TRUE
)

# -----------------------------------------------------------
# 2️⃣  Compute total future precipitation (write to disk)
# -----------------------------------------------------------
cat("Calculating total future precipitation...\n")

total_precip <- app(
  r_future,
  sum,
  na.rm=TRUE,
  filename="total_future_precip.tif",
  overwrite=TRUE
)

# -----------------------------------------------------------
# 3️⃣  Keep only precipitation above historical 95th percentile
#     (Fastest method: ifel)
# -----------------------------------------------------------
cat("Extracting very wet day precipitation...\n")

very_wet_stack <- ifel(
  r_future > p95_hist,
  r_future,
  0,
  filename="very_wet_stack.tif",
  overwrite=TRUE
)

# -----------------------------------------------------------
# 4️⃣  Sum very wet precipitation over time
# -----------------------------------------------------------
cat("Summing very wet precipitation...\n")

very_wet_sum <- app(
  very_wet_stack,
  sum,
  filename="very_wet_sum.tif",
  overwrite=TRUE
)

# -----------------------------------------------------------
# 5️⃣  Percent contribution
# -----------------------------------------------------------
cat("Calculating percent contribution...\n")

percent_contrib <- (very_wet_sum / total_precip) * 100
percent_contrib[!is.finite(percent_contrib)] <- NA
percent_contrib <- clamp(percent_contrib, 0, 100)

# -----------------------------------------------------------
# 6️⃣  Save final NetCDF
# -----------------------------------------------------------
output_file <- "Precip_VWD_2045-2074_FGOALS.nc"

cat("Saving results to:", output_file, "\n")

writeRaster(
  percent_contrib,
  filename=output_file,
  overwrite=TRUE,
  filetype="netcdf"
)

###FGOALS 2015###

#File paths
future_file <- "FGOALS/precip/pr.FGOALS-g3.ssp370.r1i1p1f1.2015-2044.LOCA2_CA_hybrid_v20230115.nc"

#-----------------------------------------------------------
# 2️⃣  Compute total future precipitation (write to disk)
# -----------------------------------------------------------
cat("Calculating total future precipitation...\n")

total_precip <- app(
  r_future,
  sum,
  na.rm=TRUE,
  filename="total_future_precip.tif",
  overwrite=TRUE
)

# -----------------------------------------------------------
# 3️⃣  Keep only precipitation above historical 95th percentile
#     (Fastest method: ifel)
# -----------------------------------------------------------
cat("Extracting very wet day precipitation...\n")

p95_hist <- rast("p95_hist.tif")


very_wet_stack <- ifel(
  r_future > p95_hist,
  r_future,
  0,
  filename="very_wet_stack.tif",
  overwrite=TRUE
)

# -----------------------------------------------------------
# 4️⃣  Sum very wet precipitation over time
# -----------------------------------------------------------
cat("Summing very wet precipitation...\n")

very_wet_sum <- app(
  very_wet_stack,
  sum,
  filename="very_wet_sum.tif",
  overwrite=TRUE
)

# -----------------------------------------------------------
# 5️⃣  Percent contribution
# -----------------------------------------------------------
cat("Calculating percent contribution...\n")

percent_contrib <- (very_wet_sum / total_precip) * 100
percent_contrib[!is.finite(percent_contrib)] <- NA
percent_contrib <- clamp(percent_contrib, 0, 100)

# -----------------------------------------------------------
# 6️⃣  Save final NetCDF
# -----------------------------------------------------------
output_file <- "Precip_VWD_2015-2044_FGOALS.nc"

cat("Saving results to:", output_file, "\n")

writeRaster(
  percent_contrib,
  filename=output_file,
  overwrite=TRUE,
  filetype="netcdf"
)



###MIROC6 2045###

# File paths
hist_file <- "MIROC6/precip/pr.MIROC6.historical.r1i1p1f1.1950-2014.LOCA2_CA_hybrid_v20230115.nc"
future_file <- "MIROC6/precip/pr.MIROC6.ssp370.r1i1p1f1.2045-2074.LOCA2_CA_hybrid_v20230115.nc"

library(terra)
library(ncdf4)

#Temp directory
if (!dir.exists("terra_tmp")) dir.create("terra_tmp")

terraOptions(progress=1, memfrac=0.8, tempdir="terra_tmp")


# Load data
r_hist <- rast(hist_file)
r_future <- rast(future_file)
stopifnot(compareGeom(r_hist, r_future))

# 1️⃣ Historical 95th percentile
p95_hist <- app(
  r_hist,
  function(x) quantile(x, 0.95, na.rm=TRUE),
  filename="p95_hist.tif",
  overwrite=TRUE
)

p95_hist <- rast("p95_hist.tif")

# 2️⃣ Total future precipitation (memory only)
total_precip <- app(r_future, sum, na.rm=TRUE)

# 3️⃣ Very wet precipitation
wet_mask <- r_future > p95_hist
wet_precip <- r_future * wet_mask

very_wet_sum <- app(
  wet_precip,
  sum,
  na.rm=TRUE,
  filename="very_wet_sum.tif",
  overwrite=TRUE
)

# 4️⃣ Percent contribution
percent_contrib <- (very_wet_sum / total_precip) * 100
percent_contrib[!is.finite(percent_contrib)] <- NA
percent_contrib <- clamp(percent_contrib, 0, 100)

# 5️⃣ Save output

writeRaster(
  percent_contrib,
  filename="Precip_VWD_2045-2074_MIROC6.nc",
  overwrite=TRUE,
  filetype="netcdf"
)



###MIROC6 2015###

#File paths
future_file <- "MIROC6/precip/pr.MIROC6.ssp370.r1i1p1f1.2015-2044.LOCA2_CA_hybrid_v20230115.nc"

# Load future precipitation data 
r_future <- rast(future_file)
stopifnot(compareGeom(r_hist, r_future))

# 2️⃣ Total future precipitation (memory only)
total_precip <- app(r_future, sum, na.rm=TRUE)

# 3️⃣ Very wet precipitation
wet_mask <- r_future > p95_hist
wet_precip <- r_future * wet_mask

very_wet_sum <- app(
  wet_precip,
  sum,
  na.rm=TRUE,
  filename="very_wet_sum.tif",
  overwrite=TRUE
)

# 4️⃣ Percent contribution
percent_contrib <- (very_wet_sum / total_precip) * 100
percent_contrib[!is.finite(percent_contrib)] <- NA
percent_contrib <- clamp(percent_contrib, 0, 100)

# 5️⃣ Save output

writeRaster(
  percent_contrib,
  filename="Precip_VWD_2015-2044_MIROC6.nc",
  overwrite=TRUE,
  filetype="netcdf"
)



###MPIESM12 2045###

# File paths
hist_file <- "MPIESM12/precip/pr.MPI-ESM1-2-HR.historical.r3i1p1f1.1950-2014.LOCA2_CA_hybrid_v20230115.nc"
future_file <- "MPIESM12/precip/pr.MPI-ESM1-2-HR.ssp370.r3i1p1f1.2045-2074.LOCA2_CA_hybrid_v20230115.nc"

library(terra)
library(ncdf4)

#Temp directory
if (!dir.exists("terra_tmp")) dir.create("terra_tmp")

terraOptions(progress=1, memfrac=0.8, tempdir="terra_tmp")


# Load data
r_hist <- rast(hist_file)
r_future <- rast(future_file)
stopifnot(compareGeom(r_hist, r_future))

# 1️⃣ Historical 95th percentile
p95_hist <- app(
  r_hist,
  function(x) quantile(x, 0.95, na.rm=TRUE),
  filename="p95_hist.tif",
  overwrite=TRUE
)

p95_hist <- rast("p95_hist.tif")

# 2️⃣ Total future precipitation (memory only)
total_precip <- app(r_future, sum, na.rm=TRUE)

# 3️⃣ Very wet precipitation
wet_mask <- r_future > p95_hist
wet_precip <- r_future * wet_mask

very_wet_sum <- app(
  wet_precip,
  sum,
  na.rm=TRUE,
  filename="very_wet_sum.tif",
  overwrite=TRUE
)

# 4️⃣ Percent contribution
percent_contrib <- (very_wet_sum / total_precip) * 100
percent_contrib[!is.finite(percent_contrib)] <- NA
percent_contrib <- clamp(percent_contrib, 0, 100)


# Save the output raster to NetCDF
writeRaster(
  percent_contrib,
  filename="Precip_VWD_2045-2074_MPIESM12.nc",
  overwrite=TRUE,
  filetype="netcdf"
)




###MPIESM12 2015###

#File paths
future_file <- "MPIESM12/precip/pr.MPI-ESM1-2-HR.ssp370.r3i1p1f1.2015-2044.LOCA2_CA_hybrid_v20230115.nc"

# Load future precipitation data 
r_future <- rast(future_file)
stopifnot(compareGeom(r_hist, r_future))

# 2️⃣ Total future precipitation (memory only)
total_precip <- app(r_future, sum, na.rm=TRUE)

# 3️⃣ Very wet precipitation
wet_mask <- r_future > p95_hist
wet_precip <- r_future * wet_mask

very_wet_sum <- app(
  wet_precip,
  sum,
  na.rm=TRUE,
  filename="very_wet_sum.tif",
  overwrite=TRUE
)

# 4️⃣ Percent contribution
percent_contrib <- (very_wet_sum / total_precip) * 100
percent_contrib[!is.finite(percent_contrib)] <- NA
percent_contrib <- clamp(percent_contrib, 0, 100)

# Save the output raster to NetCDF
writeRaster(
  percent_contrib,
  filename="Precip_VWD_2015-2044_MPIESM12.nc",
  overwrite=TRUE,
  filetype="netcdf"
)


## Calculating average of the 5 general use projections 2045-2074

```{r}
install.packages("stars")
library(stars)


# Define the NetCDF file paths
nc_files <- c(
  "Precip_VWD_2045-2074_ACCESS.nc", 
  "Precip_VWD_2045-2074_ECEARTH.nc", 
  "Precip_VWD_2045-2074_FGOALS.nc",
  "Precip_VWD_2045-2074_MIROC6.nc",
  "Precip_VWD_2045-2074_MPIESM12.nc"
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
write_stars(averaged_data, "Precip_VWD_2045-2074_GENUSE.nc")
```

## Calculating average of the 5 general use projections 2015-2044

library(stars)
library(terra)

# Define NetCDF files
nc_files <- c(
  "Precip_VWD_2015-2044_ACCESS.nc", 
  "Precip_VWD_2015-2044_ECEARTH.nc", 
  "Precip_VWD_2015-2044_FGOALS.nc",
  "Precip_VWD_2015-2044_MIROC6.nc",
  "Precip_VWD_2015-2044_MPIESM12.nc"
)

# Load files
stars_list <- lapply(nc_files, read_stars)

# Rename the variable in each object to the same name
for(i in seq_along(stars_list)){
  names(stars_list[[i]]) <- "precip"
}

# Ensure grids match
stopifnot(all(sapply(stars_list, function(x)
  all.equal(st_dimensions(x), st_dimensions(stars_list[[1]])))))

# Add model dimension
for(i in seq_along(stars_list)){
  stars_list[[i]] <- st_redimension(stars_list[[i]], model = i)
}

# Combine
combined <- do.call(c, stars_list)

# Average across models
averaged_data <- st_apply(combined, c("x","y"), mean, na.rm = TRUE)

# Save NetCDF
write_stars(averaged_data, "Precip_VWD_2015-2044_GENUSE.nc")

#Convert raster to GeoTIFF file
library(terra)

r <- rast("Precip_VWD_2015-2044_GENUSE.nc")

writeRaster(r, "Precip_VWD_2015-2044_GENUSE.tif", overwrite = TRUE)



## Calculating average of the 5 general use projections 2045-2074

library(stars)
library(terra)

# Define NetCDF files
nc_files <- c(
  "Precip_VWD_2045-2074_ACCESS.nc", 
  "Precip_VWD_2045-2074_ECEARTH.nc", 
  "Precip_VWD_2045-2074_FGOALS.nc",
  "Precip_VWD_2045-2074_MIROC6.nc",
  "Precip_VWD_2045-2074_MPIESM12.nc"
)

# Load files
stars_list <- lapply(nc_files, read_stars)

# Rename the variable in each object to the same name
for(i in seq_along(stars_list)){
  names(stars_list[[i]]) <- "precip"
}

# Ensure grids match
stopifnot(all(sapply(stars_list, function(x)
  all.equal(st_dimensions(x), st_dimensions(stars_list[[1]])))))

# Add model dimension
for(i in seq_along(stars_list)){
  stars_list[[i]] <- st_redimension(stars_list[[i]], model = i)
}

# Combine
combined <- do.call(c, stars_list)

# Average across models
averaged_data <- st_apply(combined, c("x","y"), mean, na.rm = TRUE)

# Save NetCDF
write_stars(averaged_data, "Precip_VWD_2045-2074_GENUSE.nc")

#Convert raster to GeoTIFF file
library(terra)

r <- rast("Precip_VWD_2045-2074_GENUSE.nc")

writeRaster(r, "Precip_VWD_2045-2074_GENUSE.tif", overwrite = TRUE)