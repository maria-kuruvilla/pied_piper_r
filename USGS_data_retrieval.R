#Goal - to install USGS data retrieval packaage and use it to retrieve 
# Nisqually air and flow temperatures

require(dataRetrieval)

siteNumber <- "12082500"
ChoptankInfo <- readNWISsite(siteNumber)
parameterCd <- "00060"

# Raw daily data:
rawDailyData <- readNWISdv(
  siteNumber, parameterCd,
  "2014-01-01", "2015-01-01"
)
