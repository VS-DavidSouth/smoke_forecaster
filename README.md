# smoke_forecaster
Shiny app of maps for forecasts of smoke and health impacts. 

The server runing this code can be viewed here: http://rgan.atmos.colostate.edu/ 

We welcome all feedback. 

### Overview of app functionality:

This app uses BlueSky output from the continental US. This app builds on PM2.5 concentration estimates made by BlueSky and estimates health impact using what established relationships between air pollution particulate matter and health endpoints, in this case respiratory emergency room visits.

The source BlueSky smoke data can be viewed here: https://tools.airfire.org/websky/v1/#status

Every day around 12 PM MST, a cron job initiates the daily bluesky_download R script. Sometimes, BlueSky is not done running it's daily estimates,  a simple if else that if the task fails to find new data, it will download yesterday's data (this may be replaced with a while loop to check for new data periodically, this app is still underdevelopment). We also average the same day daily estimates and tomorrow's forecasted estimate, starting at midnight EST (needs to be checked).

A couple big things are done once the daily bluesky netcdf is downloaded. The first is population weighting at the county level. This uses the proportion intersect between the bluesky grid shapefile and county shapefile. CENSUS county shapefiles are used for population, and a shapefile from the netcdf bluesky output. Population density is the estimates from 2015, which are regridded to match the bluesky grid; these data come from SEDAC.

Once the population-weighting is done, a simple health impact formula using  beta estimates from the Gan et al. 2017 GeoHealth study. Major pieces of the health impact component are the baseline incident estimates of emergency room visits for respiratory disease. These estimates are from the US CDC. The beta estimates come from Gan et al. 2017. It may be possible to use general air pollution study estimates. The final piece is the estimtated population of the county, which aligns spatially with the population weighted smoke estimates. The other part is the estimated difference of smoke PM2.5 which comes from the county population-weighted estimates above.

This approach gives a general idea of an estimate of PM2.5 concentration most likely to impact where most people live in a given county. It also likely provides an overestimate of the expected number of emergency room visits due to that concentration of smoke because these methods do not account for behavior modification where people may avoid smoke exposure (e.g. staying inside, HEPA filters, etc.). Accounting for the confidence intervals around the beta concentration estimates should provide more reasonable estimates.

In order to plot the grid and counties on the leaflet map, only values in the shapefile that are above a certain threshold (i.e. excess ED visits > 0 for county shapes, and estimated smoke values >5) are retained.

### Files you need to make this program run and what they do:
1. bluesky_grid: Shapefile used to calculate the proportion intersect between bluesky grid and county shapefile. This file is also used to join smoke PM2.5, which I then limit to values > 5. I save this a new shapefile folder called smk_poly, and this is plotted in the leaflet map.

2. smoke_poly: Made every day, subsetted shapefile from bluesky grid with smoke estimate values.

3. cb_2016_us_count_500k: County shapefile used to calcualte proportion intersect and used to plot the county estimates of population weighted pm2.5 and excess ED visits on the leaflet map. I save joined HIA estimates to a new shape folder called hia_poly. Plotted in the same way as smk_poly.

4. hia_poly: Made from folder above. Created every day after county population weighted estimates are estimated. 

5. us_census_county_population: Came from Census estimates. Contain populations for each county. I think I just used the population estimates in the HIA component.

6. 2015-bluesky_population: CSV file of population density estimates for each bluesky grid. Created using the regridded SEDAC 2015 population density estimates. I believe this file is used to estimate population-weighted pm2.5 each day.

7. bluesky_county_prop_intersect: CSV file used in HIA estimate. Intersection created from bluesky grid shape and county shape. Needed to create only once. 

8. fire_location.csv: Location of current fires. Downloaded from the Bluesky website at the same time netcdf file downloaded. My app used to stall a lot on this step if no fire locations were available for the day. 

9. smoke_dispersion.nc: Downloaded every day from Bluesky. Primary file used to estimate poplation weighted smoke and health impacts. smoke_dispersion_v2.nc made during this process based on code provided by Mazama Science.

10. data_label.Rdata: Rdata file that I save during the bluesky download. It's really only the dates to forecast to make sure the dates displayed on the radio buttons on the shiny app match the data downloaded. There may be a better way to do this.