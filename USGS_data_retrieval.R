#Goal - to install USGS data retrieval packaage and use it to retrieve 
# Nisqually air and flow temperatures

require(dataRetrieval)
require(here)

siteNumber <- "12082500"
ChoptankInfo <- readNWISsite(siteNumber)
parameterCd <- "00060"

# Raw daily data:
rawDailyData <- readNWISdv(
  siteNumber, parameterCd,
  "2014-01-01", "2020-12-01"
)

write.csv(rawDailyData, file = here("..","..","data","pied_piper","nisqually","flow_data.csv"))
colnames(rawDailyData)[4] <- c("flow")
