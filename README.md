# smoke_forecaster
Shiny app of maps for forecasts of smoke and health impacts 

### Notes for Steve:

This app uses BlueSky output from the continental US. I use the extent that includes the entire continental US. This app builds on those BlueSky estimates with a health impact componenet on it using what we know about the relationship between air pollution particulate matter and health endpoints, in this case respiratory emergency room visits.

Every day around 12 PM MST, a cron job initiates the daily bluesky_download R script. Sometimes, BlueSky is not done running it's daily estimates, so I built in a simple if else that if the task fails to find new data, it will download yesterday's data. This should probably be replaced with a while loop to check periodically. I also average the same day daily estimates and tomorrow's forecasted estimate. I believe I start at midnight EST.

A couple big things are done once the daily bluesky netcdf is downloaded. The first is that it's population weighting to the county level. This uses the proportion intersect between the bluesky grid shapefile and county shapefile. I use CENSUS county shapefiles, and I made a shapefile from the netcdf bluesky output. Population density is the estimates from 2015, which are regridded to match the bluesky grid; these data come from SEDAC.

Once the population-weighting is done, I use the simple health impact formula using our beta estimates from the Gan 2017 GeoHealth study. Major pieces of the health impact component are the baseline incident estimates of emergency room visits for respiratory disease. I get these estimates from the CDC. The beta estimates come from our paper. Although I think you could use general air pollution study estimates. The final piece is the estimtated population of the county, which aligns spatially with the population weighted smoke estimates. The other part is the estimated difference of smoke PM2.5 which comes from the county population-weighted estimates above.

I think this approach gives a general idea of an estimate of PM2.5 concentration most likely to impact where most people live in a county. It also provides probably an overestimate of the expected number of emergency room visits due to that concentration of smoke. This doesn't account for behavior modification where people may avoid going outside or what not. Accounting for the confidence intervals around the beta concentration estimates should provide more reasonable estimates.
