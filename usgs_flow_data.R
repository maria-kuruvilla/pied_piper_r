# goal - to extract flow data from the usgs website 
# author - Maria Kuruvilla 
# date - 24th Jan 2022

require(rvest)

url <- 'https://nwis.waterdata.usgs.gov/usa/nwis/uv/?cb_00060=on&format=html&site_no=12048000&period=&begin_date=2020-01-12&end_date=2020-12-19'

titles <- read_html(url)

scraped_df <- data.frame(titles)