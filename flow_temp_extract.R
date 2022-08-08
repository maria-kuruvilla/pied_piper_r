# Goal - to enter date and river and receive both flow and temp
#date - April 18th 2022

require(rvest)
require(tidyverse)

##flow


river= "Dungeness"
river = "Nisqually"

begin_date = "2017-01-01"
end_date = "2021-12-31"

if(river == "Dungeness"){
  code = "12048000"
}
if(river == "Nisqually"){
  code = "12089500"
}

url = paste0("https://nwis.waterdata.usgs.gov/usa/nwis/uv/?cb_00060=on&format=html&site_no=",
  code,"&period=&begin_date=",begin_date, "&end_date=", end_date)

html <- read_html(url)

flow_data <- html_table(html) #WORKS!

df_flow_nisqually <- as.data.frame(flow_data[[2]])
df_flow_dungeness <- as.data.frame(flow_data[[2]])

df_flow_dungeness$date_time <- as.POSIXct(substr(df_flow_dungeness[,1],1,16), 
                                          format = "%m/%d/%Y %H:%M")

df_flow_dungeness$flow <- as.numeric(strsplit(df_flow_dungeness[,2],"A"))

total <- length(df_flow_dungeness$date_time)


df_flow_dungeness$flow <- lapply(df_flow_dungeness[,2], function(x) as.numeric(strsplit(gsub(",", "", x),"A")[[1]][1]))

### temp

url_temp <- 'https://apps.ecology.wa.gov/ContinuousFlowAndWQ/StationData/Prod/18A050/18A050_2014_WTM_FM.TXT'

html_temp <- read_html(url_temp)

temp_data <- html_text(html_temp) #WORKS!

trial <- strsplit(temp_data,split="\r\n")
trial2 <- strsplit(trial[[1]][13:length(trial[[1]])],split = "\\s+")

for(i in seq(from = 522,to = 1122, by = 49)){
  #print(i)
  print(substr(temp_data,i,i+ 46))
  
}

table_w_header <- read.table(text = temp_data,sep = " ", fill = TRUE, 
                             header = TRUE, stringsAsFactors = FALSE)

temp_data_trial = gsub('\r\n', '\n', temp_data, fixed=TRUE)
table <- read.csv(text=temp_data_trial, header = FALSE)


#this is not working
#trying something else

temp_data_trial2 = gsub('\r\n', '\n', trial[[1]][13:length(trial[[1]])], fixed=TRUE)
table <- read.csv(text=temp_data_trial2, header = FALSE)


date <- as.data.frame(as.POSIXct(substr(table$V1,1,10), 
                                 format = "%m/%d/%Y"))

time <- as.data.frame(as.POSIXct(substr(table$V1,14,18), 
                                 format = "%H:%M"))

date_time <- as.data.frame(as.POSIXct(substr(table$V1,1,18), 
                                      format = "%m/%d/%Y %H:%M"))

temperature <- as.data.frame(lapply(table$V1, function(x) as.numeric(strsplit(x,"\\s+")[[1]][3])))

df_temperature_dungeness <- c(time,date,date_time,temperature)

