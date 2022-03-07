#Goal - to web scrape WA ecology website to get temperature
#Date - 10 Feb 2022

require(rvest)
require(tidyverse)

url <- 'https://apps.ecology.wa.gov/ContinuousFlowAndWQ/StationData/Prod/18A050/18A050_2014_WTM_FM.TXT'

html <- read_html(url)

temp_data <- html_text(html) #WORKS!

temp_data <- substr(temp_data,522,5683)

j=522
for(i in seq(from = 522,to = 1122, by = 49)){
  #print(i)
  print(substr(temp_data,i,i+ 46))
  
}

trial <- strsplit(temp_data,split="\r\n")
trial2 <- strsplit(trial[[1]][1],split = " ")
