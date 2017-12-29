# ------------------------------------------------------------------------------
# Title: Creating vector of gridded population
# Author: Ryan Gan
# Date Created: 2017-12-29
# ------------------------------------------------------------------------------

# library
library(ncdf4)
library(tidyverse)

# extracting population densities from bonne's bluesky grid -----
pop_nc <- ncdf4::nc_open("./data/blueskypopulation.nc")

cali_id <- bluesky_grid$id
# extract population and population density for california grid cells
pop <- as.vector(ncdf4::ncvar_get(pop_nc, varid = "Population"))
popden <- as.vector(ncdf4::ncvar_get(pop_nc, varid ="PopulationDensity"))

# extract latlon
lat <- ncdf4::ncvar_get(pop_nc, varid ="latitude")
lon <- ncdf4::ncvar_get(pop_nc, varid = "longitude")
# expand grid
lonlat <- as.matrix(expand.grid(lon,lat))

# create population dataframe and add names
population_df <- data.frame(cbind(lonlat, pop, popden))
# assign names
names(population_df) <- c("lon", "lat", "pop", "popden")

# sf label starts top left and goes right, then down one row
# sort by desc(lat) then lon to match how i labeled the sf objects
population_df <- population_df %>%  
  arrange(desc(lat), lon) %>% 
  mutate(id = seq(1:94068)) %>% 
  dplyr::select(id, pop, popden)


# saving population density and population vector
write_csv(population_df, paste0("./data/2015-bluesky_grid_population.csv"))
