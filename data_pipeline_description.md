# This file documents the data used and displayed by this application. 

### BlueSky smoke concentrations

- BlueSky smoke concentrations are downloaded with: /support_r_scripts/daily_bluesky_download.R
- Using sys.Date, the script checks to see what the most recent output file is, choosing between 00Z and 12Z forecast. Right now only the GFS-0.15deg field is aquired. The smoke dispersion file downloaded is from the "combined" direcoty, meaning the forecast includes forecast emissions and leftover from the previous period.