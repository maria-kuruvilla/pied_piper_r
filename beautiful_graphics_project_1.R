# Goal - produce one figure with fish per hour on y axis,
# time of year on x axis. Have one line for multiple years and the average of the years 
# in darker. Do this first for one species and one river. 

# date - Feb 4 2022

#nisqually and chinook


library(RODBC)
library(here)
library(tidyverse)
library(tidyquant)

year = c(2015,2016,2017,2018,2019,2020)
plot1 = ggplot()
for(i in 1:6){
  string_year = paste(toString(year[i]),"Nisqually.accdb")
  print(string_year)
  channel <- odbcConnectAccess2007(here("Documents","data","pied_piper",string_year))
  df <- sqlFetch(channel, 'qry_AllCatch')
  #print(unique(df$WSPEName))
  df_chinook0 <- df[(df$WSPEName=="Chinook 0+ W" & df$CaptureType==1),]# | df$WSPEName=="Coho 1+ H",]
  #print(df_chinook0[1,])
  if(i == 1){
    df_new = df_chinook0
  }
  else{
    df_new <- rbind(df_new,df_chinook0)
  }
  
  
  #plot1 = plot1 + geom_line(data = df_chinook0, aes(x = day_of_year, y = NumCaught))
}
df_new$day_of_year <- format(as.POSIXct(df_new$StartDate), format = "%m/%d")
df_new$year <- format(as.POSIXct(df_new$StartDate), format = "%Y")
df_new$start_datetime <- as.POSIXct(paste(df_new$StartDate, 
                                           format(df_new$StartTime, format = "%H:%M:%S")))

df_new$start_time <- format(as.POSIXct(df_new$start_datetime), format = "%H:%M:%S")


df_new$end_datetime <- as.POSIXct(paste(df_new$EndDate, 
                                         format(df_new$EndTime, format = "%H:%M:%S")))

df_new$end_time <- format(as.POSIXct(df_new$end_datetime), format = "%H:%M:%S")

df_new$fish_per_hour <- df_new$NumCaught/as.numeric(df_new$end_datetime - df_new$start_datetime)

df_new_summarize <- df_new %>%
  group_by(day_of_year, year) %>%
  summarise(sum_num_caught = sum(fish_per_hour))
plot1 <- ggplot(data = df_new_summarize, aes(x = day_of_year, y = sum_num_caught, colour = year, group=year)) +
  geom_line(lwd = 1) + theme_minimal() +
  scale_x_discrete(expand = c(0,0),breaks = c("01/01","02/01","03/01","04/01",
                                              "05/01","06/01","07/01","08/01",
                                              "09/01","10/01","11/01", "12/01"),
                   labels = c("Jan","Feb","Mar","Apr",
                               "May","Jun","Jul","Aug",
                               "Sep","Oct","Nov", "Dec"))
print(plot1)

my_data_frame <- df_new %>%
  group_by(day_of_year) %>%
  summarise(avg_num_caught = mean(fish_per_hour), sd = sd(fish_per_hour))

plot2 <- ggplot(data = my_data_frame, aes(x = day_of_year, y = avg_num_caught, group = 1)) +
  geom_line() + theme_minimal() +
  geom_ribbon(aes(y = avg_num_caught, ymin = avg_num_caught - sd, ymax = avg_num_caught + sd), alpha = .2) +
  scale_x_discrete(expand = c(0,0),breaks = c("01/01","02/01","03/01","04/01",
                                              "05/01","06/01","07/01","08/01",
                                              "09/01","10/01","11/01", "12/01"),
                   labels = c("Jan","Feb","Mar","Apr",
                              "May","Jun","Jul","Aug",
                              "Sep","Oct","Nov", "Dec"))
print(plot2)
#std deviation shows negative values

#next step - plot fish per hour

plot3 <- ggplot(data = my_data_frame, aes(x = day_of_year, y = avg_num_caught, group = 1,color = "2")) +
  geom_line(color = "black") + theme_minimal() +
  scale_x_discrete(expand = c(0,0),breaks = c("01/01","02/01","03/01","04/01",
                                              "05/01","06/01","07/01","08/01",
                                              "09/01","10/01","11/01", "12/01"),
                   labels = c("Jan","Feb","Mar","Apr",
                              "May","Jun","Jul","Aug",
                              "Sep","Oct","Nov", "Dec"))+ 
  geom_ma(ma_fun = SMA, n = 7, color = "red", linetype = 1, size = 1, show.legend = TRUE) +
  xlab("Time of Year") + ylab("Number of fish caught per hour") +
  labs(title = "Nisqually Chinook 0+ W")+ theme(legend.position=c(0.8, 0.2))
print(plot3)


# try dungeness
year = c(2015,2016,2017,2018,2019,2020)
plot1 = ggplot()
for(i in 1:6){
  string_year = paste(toString(year[i]),"Dungeness.accdb")
  print(string_year)
  channel <- odbcConnectAccess2007(here("Documents","data","pied_piper",string_year))
  df <- sqlFetch(channel, 'qry_AllCatch')
  #print(unique(df$WSPEName))
  df_chinook0 <- df[(df$WSPEName=="Chinook 0+ W" & df$CaptureType==1),]# | df$WSPEName=="Coho 1+ H",]
  #print(df_chinook0[1,])
  if(i == 1){
    df_new = df_chinook0
  }
  else{
    df_new <- rbind(df_new,df_chinook0)
  }
  
  
  #plot1 = plot1 + geom_line(data = df_chinook0, aes(x = day_of_year, y = NumCaught))
}
df_new$day_of_year <- format(as.POSIXct(df_new$StartDate), format = "%m/%d")
df_new$year <- format(as.POSIXct(df_new$StartDate), format = "%Y")
df_new$start_datetime <- as.POSIXct(paste(df_new$StartDate, 
                                          format(df_new$StartTime, format = "%H:%M:%S")))

df_new$start_time <- format(as.POSIXct(df_new$start_datetime), format = "%H:%M:%S")


df_new$end_datetime <- as.POSIXct(paste(df_new$EndDate, 
                                        format(df_new$EndTime, format = "%H:%M:%S")))

df_new$end_time <- format(as.POSIXct(df_new$end_datetime), format = "%H:%M:%S")

df_new$fish_per_hour <- df_new$NumCaught/as.numeric(df_new$end_datetime - df_new$start_datetime)

df_new_summarize <- df_new %>%
  group_by(day_of_year, year) %>%
  summarise(sum_num_caught = sum(fish_per_hour))
my_data_frame <- df_new %>%
  group_by(day_of_year) %>%
  summarise(avg_num_caught = mean(fish_per_hour), sd = sd(fish_per_hour))

plot4 <- ggplot(data = my_data_frame, aes(x = day_of_year, y = avg_num_caught, group = 1)) +
  geom_line() + theme_minimal() +
  scale_x_discrete(expand = c(0,0),breaks = c("01/01","02/01","03/01","04/01",
                                              "05/01","06/01","07/01","08/01",
                                              "09/01","10/01","11/01", "12/01"),
                   labels = c("Jan","Feb","Mar","Apr",
                              "May","Jun","Jul","Aug",
                              "Sep","Oct","Nov", "Dec"))+ 
  geom_ma(ma_fun = SMA, n = 7, color = "red", linetype = 1, size = 1) +
  xlab("Time of Year") + ylab("Number of fish caught per hour") +
  labs(title = "Dungeness Chinook 0+ W")+ theme(legend.position=c(0.8, 0.2))
print(plot4)

##################################################################

species = "Coho"
age = "0+"
type = "W"
data_filter = paste(species,age,type)
river = "Dungeness"
file_type = ".accdb"
file = paste0(river,file_type)

year = c(2015,2016,2017,2018,2019,2020)

for(i in 1:6){
  string_year = paste(toString(year[i]),file)
  print(string_year)
  channel <- odbcConnectAccess2007(here("Documents","data","pied_piper",string_year))
  df <- sqlFetch(channel, 'qry_AllCatch')
  #print(unique(df$WSPEName))
  df_species <- df[(df$WSPEName==data_filter & df$CaptureType==1),]# | df$WSPEName=="Coho 1+ H",]
  #print(df_chinook0[1,])
  if(i == 1){
    df_new = df_species
  }
  else{
    df_new <- rbind(df_new,df_species)
  }
}

df_new$day_of_year <- format(as.POSIXct(df_new$StartDate), format = "%m/%d")
df_new$year <- format(as.POSIXct(df_new$StartDate), format = "%Y")
df_new$start_datetime <- as.POSIXct(paste(df_new$StartDate, 
                                          format(df_new$StartTime, format = "%H:%M:%S")))

df_new$start_time <- format(as.POSIXct(df_new$start_datetime), format = "%H:%M:%S")


df_new$end_datetime <- as.POSIXct(paste(df_new$EndDate, 
                                        format(df_new$EndTime, format = "%H:%M:%S")))

df_new$end_time <- format(as.POSIXct(df_new$end_datetime), format = "%H:%M:%S")

df_new$fish_per_hour <- df_new$NumCaught/as.numeric(df_new$end_datetime - df_new$start_datetime)

df_new_summarize <- df_new %>%
  group_by(day_of_year, year) %>%
  summarise(sum_num_caught = sum(fish_per_hour))
my_data_frame <- df_new %>%
  group_by(day_of_year) %>%
  summarise(avg_num_caught = mean(fish_per_hour), sd = sd(fish_per_hour))

plot4 <- ggplot(data = my_data_frame, aes(x = day_of_year, y = avg_num_caught, group = 1)) +
  geom_line() + theme_minimal() +
  scale_x_discrete(expand = c(0,0),breaks = c("01/01","02/01","03/01","04/01",
                                              "05/01","06/01","07/01","08/01",
                                              "09/01","10/01","11/01", "12/01"),
                   labels = c("Jan","Feb","Mar","Apr",
                              "May","Jun","Jul","Aug",
                              "Sep","Oct","Nov", "Dec"))+ 
  geom_ma(aes(color = "Moving average"),ma_fun = SMA, n = as.integer(length(my_data_frame$avg_num_caught)/10), color = "red", linetype = 1, size = 1) +
  xlab("Time of Year") + ylab("Number of fish caught per hour") +
  labs(title = paste(river,data_filter))+ 
  scale_colour_manual(name = 'Legend', 
                      guide = 'legend',
                      values = c('Moving Average' = 'red'), 
                      labels = c('Moving Average'))+
  theme(legend.justification=c(1,1), legend.position=c(0.75, 0.85))
print(plot4)



species = "Coho"
age = "1+"
type = "W"
data_filter = paste(species,age,type)
river = "Dungeness"
file_type = ".accdb"
file = paste0(river,file_type)


species = "Chinook"
age = "0+"
type = "W"
data_filter = paste(species,age,type)
river = "Dungeness"
file_type = ".accdb"
file = paste0(river,file_type)

#scatter plot for deviation

#calculate deviation of wild fish per hour from moving average for each day


#plot deviation as function of number of hatchery fish released


f21 <- rep(1/21,21)
my_data_frame$moving_average <- stats::filter(my_data_frame$avg_num_caught,f21,  sides = 2)
my_data_frame$moving_average[is.na(my_data_frame$moving_average)] <- 
  my_data_frame$avg_num_caught[is.na(my_data_frame$moving_average)]
plot5 <- ggplot(data = my_data_frame, aes(x = day_of_year, y = avg_num_caught, group = 1)) +
  geom_line() + theme_minimal() +
  scale_x_discrete(expand = c(0,0),breaks = c("01/01","02/01","03/01","04/01",
                                              "05/01","06/01","07/01","08/01",
                                              "09/01","10/01","11/01", "12/01"),
                   labels = c("Jan","Feb","Mar","Apr",
                              "May","Jun","Jul","Aug",
                              "Sep","Oct","Nov", "Dec"))+ 
  xlab("Time of Year") + ylab("Number of fish caught per hour") +
  labs(title = paste(river,data_filter))+ 
  geom_line(aes(x=day_of_year,y=moving_average), color = "red", size = 1)
  
print(plot5)


##scatter plot for deviation

#for loop for all entries in df_new
for(i in 1:length(df_new$day_of_year)){
  #for loop for all entries in my_data_frame
  for(j in 1:length(my_data_frame$day_of_year)){
    ma_day_of_year = my_data_frame$day_of_year[j]
    all_data_day_of_year = df_new$day_of_year[i]
    #if both day of years are the same, add it to moving average column of df_new
    if(ma_day_of_year == all_data_day_of_year){
      df_new$moving_average[i] <- my_data_frame$moving_average[j]
    }
  }
}
  
#calculate deviation of wild fish per hour from moving average for each day
#and add deviation column to df_new
df_new$deviation <- df_new$fish_per_hour - df_new$moving_average
     


#plot hatchery fish

species = "Coho"
age = "1+"
type = "H"
data_filter = paste(species,age,type)
river = "Dungeness"
file_type = ".accdb"
file = paste0(river,file_type)

###function to return dataframe given river, species, age and type
extract_all_data <- function(species,age,type,river,file_type){
  data_filter = paste(species,age,type)
  file = paste0(river,file_type)
  year = c(2015,2016,2017,2018,2019,2020)
  
  for(i in 1:6){
    string_year = paste(toString(year[i]),file)
    print(string_year)
    channel <- odbcConnectAccess2007(here("Documents","data","pied_piper",string_year))
    df <- sqlFetch(channel, 'qry_AllCatch')
    #print(unique(df$WSPEName))
    df_species <- df[(df$WSPEName==data_filter & df$CaptureType==1),]# | df$WSPEName=="Coho 1+ H",]
    #print(df_chinook0[1,])
    if(i == 1){
      df_species_all_years = df_species
    }
    else{
      df_species_all_years <- rbind(df_species_all_years,df_species)
    }
  }
  
  df_species_all_years$day_of_year <- format(as.POSIXct(df_species_all_years$StartDate), format = "%m/%d")
  df_species_all_years$year <- format(as.POSIXct(df_species_all_years$StartDate), format = "%Y")
  df_species_all_years$start_datetime <- as.POSIXct(paste(df_species_all_years$StartDate, 
                                            format(df_species_all_years$StartTime, format = "%H:%M:%S")))
  
  df_species_all_years$start_time <- format(as.POSIXct(df_species_all_years$start_datetime), format = "%H:%M:%S")
  
  
  df_species_all_years$end_datetime <- as.POSIXct(paste(df_species_all_years$EndDate, 
                                          format(df_species_all_years$EndTime, format = "%H:%M:%S")))
  
  df_species_all_years$end_time <- format(as.POSIXct(df_species_all_years$end_datetime), format = "%H:%M:%S")
  
  df_species_all_years$fish_per_hour <- df_species_all_years$NumCaught/as.numeric(df_species_all_years$end_datetime - df_species_all_years$start_datetime)
  return(df_species_all_years)
}

coho_w <- extract_all_data("Coho", "1+", "W", "Dungeness", ".accdb")
coho_h <- extract_all_data("Coho", "1+", "H", "Dungeness", ".accdb")

year_average <- function(df_new){
  my_data_frame <- df_new %>%
    group_by(day_of_year) %>%
    summarise(avg_num_caught = mean(fish_per_hour), sd = sd(fish_per_hour))
  f21 <- rep(1/21,21)
  my_data_frame$moving_average <- stats::filter(my_data_frame$avg_num_caught,f21,  sides = 2)
  my_data_frame$moving_average[is.na(my_data_frame$moving_average)] <- 
    my_data_frame$avg_num_caught[is.na(my_data_frame$moving_average)]
  for(i in 1:length(df_new$day_of_year)){
    #for loop for all entries in my_data_frame
    for(j in 1:length(my_data_frame$day_of_year)){
      ma_day_of_year = my_data_frame$day_of_year[j]
      all_data_day_of_year = df_new$day_of_year[i]
      #if both day of years are the same, add it to moving average column of df_new
      if(ma_day_of_year == all_data_day_of_year){
        df_new$moving_average[i] <- my_data_frame$moving_average[j]
      }
    }
  }
  return(df_new)
  
}

average_wildfish_per_hour <- year_average(coho_w)



for(i in 1:length(coho_h$fish_per_hour)){
  for(j in 1:length(average_wildfish_per_hour$fish_per_hour)){
    if(coho_h$StartDate[i] == average_wildfish_per_hour$StartDate[j]){
      coho_h$wild_fish[i] <- average_wildfish_per_hour$fish_per_hour[j] 
      coho_h$wild_fish_ma[i] <- average_wildfish_per_hour$moving_average[j]
      coho_h$wild_fish_deviation[i] <- coho_h$wild_fish[i] - coho_h$wild_fish_ma[i]
      
    }
    
  }
}

plot_dev <- ggplot(data = coho_h,aes(x = fish_per_hour,y=wild_fish_deviation))+
  geom_point() + theme_minimal()
plot_dev




#plot deviation as function of number of hatchery fish released
