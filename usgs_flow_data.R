# goal - to extract flow data from the usgs website 
# author - Maria Kuruvilla 
# date - 24th Jan 2022

require(rvest)

url <- 'https://nwis.waterdata.usgs.gov/usa/nwis/uv/?cb_00060=on&format=html&site_no=12048000&period=&begin_date=2020-01-12&end_date=2020-12-19'

html <- read_html(url)


# scraped_df <- data.frame(titles) #did not work
# 
# titles %>% 
#   html_element(".tracklist") %>% 
#   html_table() # does not work
# 
# html <- read_html("https://en.wikipedia.org/w/index.php?title=The_Lego_Movie&oldid=998422565")
# html %>% 
#   html_element(".tracklist") %>% 
#   html_table() #works but not for my table

flow_data <- html_table(html) #WORKS!

df_flow <- as.data.frame(flow_data[[2]])

colnames(df_flow) <- c("date_time","discarge")

df_flow$date <- substr(df_flow$date_time,0,10)

df_flow$time <- substr(df_flow$date_time,12,16)

df_flow$tz <- substr(df_flow$date_time,20,22)

df_flow$date <- as.POSIXlt(substr(df_flow$date_time,0,10),format="%m/%d/%Y") 
df_flow$time <- format(as.POSIXct(substr(df_flow$date_time,12,16),format="%H:%M"), 
                       format = "%H:%M:%S")


