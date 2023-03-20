#Plan for the pied piper project as proposed during general exam

##############
#Onset of migration, Middle of migration, End of migration, Duration of migration
#need to remove years where there are no migrants


#calculate mean jan tmep, mean feb temp, mean march temp, mean april temp,

# have one df for duration


library(here)
library(tidyverse)
agg_data_w_ma_msd <- read.csv(here("Documents","data","pied_piper","dungeness_2005-2020_combine_ma_msd_atu_photoperiod.csv"),header=TRUE)#,na.strings=c("[nan]"))


years = unique(agg_data_w_ma_msd$year)

#columns for start - year, doy, mean temp before, mean temp feb,
# mean  temp march,  flood days before, flood days in feb,
#flood days march,  total wild
#start_chinook0_df <- matrix(ncol = 8, nrow = length(years))
start_chinook1_df <- matrix(ncol = 9, nrow = length(years))
start_coho1_df <- matrix(ncol = 15, nrow = length(years))
start_steelhead_df <- matrix(ncol = 15, nrow = length(years))

#columns for middle - year, doy, mean temp between start and middle,
#mean temp_april, flood days between start and middle, flood days april,
#total_wild, hatchery
#middle_chinook0_df <- matrix(ncol = 7, nrow = length(years))
middle_chinook1_df <- matrix(ncol = 8, nrow = length(years))
middle_coho1_df <- matrix(ncol = 7, nrow = length(years))
middle_steelhead_df <- matrix(ncol = 7, nrow = length(years))

#end_chinook0_df <- matrix(ncol = 8, nrow = length(years))
end_chinook1_df <- matrix(ncol = 5, nrow = length(years))
end_coho1_df <- matrix(ncol = 8, nrow = length(years))
end_steelhead_df <- matrix(ncol = 8, nrow = length(years))

#year
#duration
#number of hatchery fish released within duration
#mean temp within duration
#number of flood days within duration
#number of fish that migrate
duration_chinook1_df <- matrix(ncol = 6, nrow = length(years))

for(i in 1:length(years)){
  data_year <- agg_data_w_ma_msd[agg_data_w_ma_msd$year == years[i],]
  
  # data_year$cum_chinook0_wild <- cumsum(data_year$chinook0_wild_num)
  # 
  # #end = append(end,which.min(abs(data_year$cum_chinook0_wild - max(data_year$cum_chinook0_wild)*0.9)))
  # 
  # start <- which.min(abs(data_year$cum_chinook0_wild - max(data_year$cum_chinook0_wild)*0.1))
  # 
  # middle <- which.min(abs(data_year$cum_chinook0_wild - max(data_year$cum_chinook0_wild)*0.5))
  # 
  # end <- which.min(abs(data_year$cum_chinook0_wild - max(data_year$cum_chinook0_wild)*0.9))
  # 
  # start_chinook0_df[i,1] <- data_year$year[start]
  # start_chinook0_df[i,2] <- data_year$doy[start]
  # start_chinook0_df[i,3] <- data_year$temp[start]
  # start_chinook0_df[i,4] <- data_year$atu_solstice[start]
  # start_chinook0_df[i,5] <- data_year$flow[start]
  # start_chinook0_df[i,6] <- data_year$photoperiod[start]
  # start_chinook0_df[i,7] <- data_year$atu_april[start]
  # start_chinook0_df[i,8] <- data_year$atu_solstice[data_year$doy == 70]
  # 
  # middle_chinook0_df[i,1] <- data_year$year[middle]
  # middle_chinook0_df[i,2] <- data_year$doy[middle]
  # middle_chinook0_df[i,3] <- data_year$temp[middle]
  # middle_chinook0_df[i,4] <- data_year$atu_solstice[middle]
  # middle_chinook0_df[i,5] <- data_year$flow[middle]
  # middle_chinook0_df[i,6] <- data_year$photoperiod[middle]
  # middle_chinook0_df[i,7] <- data_year$atu_april[middle]
  # 
  # end_chinook0_df[i,1] <- data_year$year[end]
  # end_chinook0_df[i,2] <- data_year$doy[end]
  # end_chinook0_df[i,3] <- data_year$temp[end]
  # end_chinook0_df[i,4] <- data_year$atu_solstice[end]
  # end_chinook0_df[i,5] <- data_year$flow[end]
  # end_chinook0_df[i,6] <- data_year$photoperiod[end]
  # end_chinook0_df[i,7] <- data_year$atu_april[end]
  # end_chinook0_df[i,8] <- data_year$doy[end] - data_year$doy[start]
  
  
  data_year$cum_chinook1_wild <- cumsum(data_year$chinook1_wild_num)
  data_year$cum_chinook1_hatchery <- cumsum(data_year$chinook1_hatchery_num)
  start <- which.min(abs(data_year$cum_chinook1_wild - max(data_year$cum_chinook1_wild)*0.1))
  
  middle <- which.min(abs(data_year$cum_chinook1_wild - max(data_year$cum_chinook1_wild)*0.5))
  
  end <- which.min(abs(data_year$cum_chinook1_wild - max(data_year$cum_chinook1_wild)*0.9))
  
  if(data_year$cum_chinook1_wild[length(data_year$cum_chinook1_wild)] < 10){
    next
  }
  else{
    #columns for start - year, doy, mean temp before, mean temp feb,
    # mean  temp march,flood days before, flood days in feb,
    #flood days march, total wild
    start_chinook1_df[i,1] <- data_year$year[start]
    start_chinook1_df[i,2] <- data_year$doy[start]
    start_chinook1_df[i,3] <- mean(data_year$temp[data_year$doy<data_year$doy[start]],
                                   na.rm = TRUE)
    start_chinook1_df[i,4] <- mean(data_year$temp[intersect(
      which(data_year$doy>31),which(data_year$doy<60))], na.rm = TRUE)
    start_chinook1_df[i,5] <- mean(data_year$temp[intersect(
      which(data_year$doy>59),which(data_year$doy<92))], na.rm = TRUE)
    start_chinook1_df[i,6] <- sum(data_year$flow[data_year$doy < data_year$doy[start]]
                                  > 500) #defining flood days as flow > 500
    start_chinook1_df[i,7] <- sum(data_year$flow[intersect(which(
      data_year$doy > 31), 
      which(data_year$doy < 60))] > 500)
    start_chinook1_df[i,8] <- sum(data_year$flow[intersect(which(
      data_year$doy > 59), 
      which(data_year$doy < 92))] > 500)
    start_chinook1_df[i,9] <- data_year$cum_chinook1_wild[length(
      data_year$cum_chinook1_wild)]
    
    #start_chinook1_df[i,2] <- data_year$doy[start]
    # start_chinook1_df[i,3] <- data_year$temp[start]
    # start_chinook1_df[i,4] <- data_year$atu_solstice[start]
    # start_chinook1_df[i,5] <- data_year$flow[start]
    # start_chinook1_df[i,6] <- data_year$photoperiod[start]
    # start_chinook1_df[i,7] <- data_year$atu_april[start]
    # start_chinook1_df[i,8] <- data_year$atu_solstice[data_year$doy == 70]
    
    #columns for middle - year, doy, mean temp between start and middle,
    #mean temp_april, flood days between start and middle, flood days april,
    #total_wild, hatchery
    middle_chinook1_df[i,1] <- data_year$year[middle]
    middle_chinook1_df[i,2] <- data_year$doy[middle]
    middle_chinook1_df[i,3] <- mean(data_year$temp[intersect(
      which(data_year$doy>data_year$doy[start]),
      which(data_year$doy<data_year$doy[middle]))])
    middle_chinook1_df[i,4] <- mean(data_year$temp[intersect(
      which(data_year$doy>91),
      which(data_year$doy<121))])
    middle_chinook1_df[i,5] <- sum(data_year$flow[intersect(which(
      data_year$doy > data_year$doy[start]), 
      which(data_year$doy < data_year$doy[middle]))] > 500)
    middle_chinook1_df[i,6] <- sum(data_year$flow[intersect(which(
      data_year$doy > 91), 
      which(data_year$doy < 121))] > 500)
    middle_chinook1_df[i,7] <- data_year$cum_chinook1_wild[length(
      data_year$cum_chinook1_wild)]
    middle_chinook1_df[i,8] <- (data_year$cum_chinook1_hatchery[middle] 
                             - data_year$cum_chinook1_hatchery[start])
    # middle_chinook1_df[i,3] <- data_year$temp[middle]
    # middle_chinook1_df[i,4] <- data_year$atu_solstice[middle]
    # middle_chinook1_df[i,5] <- data_year$flow[middle]
    # middle_chinook1_df[i,6] <- data_year$photoperiod[middle]
    # middle_chinook1_df[i,7] <- data_year$atu_april[middle]
    
    end_chinook1_df[i,1] <- data_year$year[end]
    end_chinook1_df[i,2] <- data_year$doy[end]
    end_chinook1_df[i,3] <- mean(data_year$temp[intersect(
      which(data_year$doy>data_year$doy[middle]),
      which(data_year$doy<data_year$doy[end]))])
    end_chinook1_df[i,4] <- sum(data_year$flow[intersect(which(
      data_year$doy > data_year$doy[middle]), 
      which(data_year$doy < data_year$doy[end]))] > 500)
    end_chinook1_df[i,5] <- (data_year$cum_chinook1_hatchery[end] 
                             - data_year$cum_chinook1_hatchery[middle])
    # end_chinook1_df[i,3] <- data_year$temp[end]
    # end_chinook1_df[i,4] <- data_year$atu_solstice[end]
    # end_chinook1_df[i,5] <- data_year$flow[end]
    # end_chinook1_df[i,6] <- data_year$photoperiod[end]
    # end_chinook1_df[i,7] <- data_year$atu_april[end]
    # end_chinook1_df[i,8] <- data_year$doy[end] - data_year$doy[start]
    
    duration_chinook1_df[i,1] <- data_year$year[end]
    duration_chinook1_df[i,2] <- data_year$doy[end] - data_year$doy[start]
    duration_chinook1_df[i,3] <- (data_year$cum_chinook1_hatchery[end] 
                                  - data_year$cum_chinook1_hatchery[start])
    duration_chinook1_df[i,4] <- mean(data_year$temp[intersect(
      which(data_year$doy>data_year$doy[start]),
      which(data_year$doy<data_year$doy[end]))])
    duration_chinook1_df[i,5] <- sum(data_year$flow[intersect(which(
      data_year$doy > data_year$doy[start]), 
      which(data_year$doy < data_year$doy[end]))] > 500)
    duration_chinook1_df[i,6] <- data_year$cum_chinook1_wild[length(
      data_year$cum_chinook1_wild)]
    
  }
  
  data_year$cum_coho1_wild <- cumsum(data_year$coho1_wild_num)
  
  #end = append(end,which.min(abs(data_year$cum_chinook0_wild - max(data_year$cum_chinook0_wild)*0.9)))
  data_year$cum_coho1_wild <- cumsum(data_year$coho1_wild_num)
  data_year$cum_coho1_hatchery <- cumsum(data_year$coho1_hatchery_num)
  
  if(data_year$cum_coho1_wild[length(data_year$cum_coho1_wild)] < 10){
    next
  }
  else{
  
    
    
    start <- which.min(abs(data_year$cum_coho1_wild - max(data_year$cum_coho1_wild)*0.1))
    
    middle <- which.min(abs(data_year$cum_coho1_wild - max(data_year$cum_coho1_wild)*0.5))
    
    end <- which.min(abs(data_year$cum_coho1_wild - max(data_year$cum_coho1_wild)*0.9))
    
    #middle = append(middle,which.min(abs(data_year$cum_chinook0_wild - max(data_year$cum_chinook0_wild)*0.5)))
    
    start_coho1_df[i,1] <- data_year$year[start]
    start_coho1_df[i,2] <- data_year$doy[start]
    start_coho1_df[i,3] <- data_year$temp[start]
    start_coho1_df[i,4] <- data_year$atu_solstice[start]
    start_coho1_df[i,5] <- data_year$flow[start]
    start_coho1_df[i,6] <- data_year$photoperiod[start]
    start_coho1_df[i,7] <- data_year$atu_april[start]
    start_coho1_df[i,8] <- data_year$atu_solstice[data_year$doy == 70]
    start_coho1_df[i,9] <- mean(data_year$temp[data_year$doy<data_year$doy[start]],
                                   na.rm = TRUE)
    start_coho1_df[i,10] <- mean(data_year$temp[intersect(
      which(data_year$doy>31),which(data_year$doy<60))], na.rm = TRUE)
    start_coho1_df[i,11] <- mean(data_year$temp[intersect(
      which(data_year$doy>59),which(data_year$doy<92))], na.rm = TRUE)
    start_coho1_df[i,12] <- sum(data_year$flow[data_year$doy < data_year$doy[start]]
                                  > 500) #defining flood days as flow > 500
    start_coho1_df[i,13] <- sum(data_year$flow[intersect(which(
      data_year$doy > 31), 
      which(data_year$doy < 60))] > 500)
    start_coho1_df[i,14] <- sum(data_year$flow[intersect(which(
      data_year$doy > 59), 
      which(data_year$doy < 92))] > 500)
    start_coho1_df[i,15] <- data_year$cum_coho1_wild[length(
      data_year$cum_coho1_wild)]
    
    middle_coho1_df[i,1] <- data_year$year[middle]
    middle_coho1_df[i,2] <- data_year$doy[middle]
    middle_coho1_df[i,3] <- data_year$temp[middle]
    middle_coho1_df[i,4] <- data_year$atu_solstice[middle]
    middle_coho1_df[i,5] <- data_year$flow[middle]
    middle_coho1_df[i,6] <- data_year$photoperiod[middle]
    middle_coho1_df[i,7] <- data_year$atu_april[middle]
    
    end_coho1_df[i,1] <- data_year$year[end]
    end_coho1_df[i,2] <- data_year$doy[end]
    end_coho1_df[i,3] <- data_year$temp[end]
    end_coho1_df[i,4] <- data_year$atu_solstice[end]
    end_coho1_df[i,5] <- data_year$flow[end]
    end_coho1_df[i,6] <- data_year$photoperiod[end]
    end_coho1_df[i,7] <- data_year$atu_april[end]
    end_coho1_df[i,8] <- data_year$doy[end] - data_year$doy[start]
  }
  
  data_year$cum_steelhead_wild <- cumsum(data_year$steelheadsmolt_wild_num)
  
  #end = append(end,which.min(abs(data_year$cum_chinook0_wild - max(data_year$cum_chinook0_wild)*0.9)))
  data_year$cum_steelhead_wild <- cumsum(data_year$steelheadsmolt_wild_num)
  data_year$cum_steelhead_hatchery <- cumsum(data_year$steelheadsmolt_hatchery_num)
  
  if(data_year$cum_steelhead_wild[length(data_year$cum_steelhead_wild)] < 10){
    next
  }
  else{
    #end = append(end,which.min(abs(data_year$cum_chinook0_wild - max(data_year$cum_chinook0_wild)*0.9)))
    
    start <- which.min(abs(data_year$cum_steelhead_wild - max(data_year$cum_steelhead_wild)*0.1))
    
    middle <- which.min(abs(data_year$cum_steelhead_wild - max(data_year$cum_steelhead_wild)*0.5))
    
    end <- which.min(abs(data_year$cum_steelhead_wild - max(data_year$cum_steelhead_wild)*0.9))
    
    #middle = append(middle,which.min(abs(data_year$cum_chinook0_wild - max(data_year$cum_chinook0_wild)*0.5)))
    
    #columns for start - year, doy, mean temp before, mean temp feb,
    # mean  temp march,flood days before, flood days in feb,
    #flood days march, total wild
    start_steelhead_df[i,1] <- data_year$year[start]
    start_steelhead_df[i,2] <- data_year$doy[start]
    start_steelhead_df[i,3] <- data_year$temp[start]
    start_steelhead_df[i,4] <- data_year$atu_solstice[start]
    start_steelhead_df[i,5] <- data_year$flow[start]
    start_steelhead_df[i,6] <- data_year$photoperiod[start]
    start_steelhead_df[i,7] <- data_year$atu_april[start]
    start_steelhead_df[i,8] <- data_year$atu_solstice[data_year$doy == 70]
    
    start_steelhead_df[i,9] <- mean(data_year$temp[data_year$doy<data_year$doy[start]],
                                na.rm = TRUE)
    start_steelhead_df[i,10] <- mean(data_year$temp[intersect(
      which(data_year$doy>31),which(data_year$doy<60))], na.rm = TRUE)
    start_steelhead_df[i,11] <- mean(data_year$temp[intersect(
      which(data_year$doy>59),which(data_year$doy<92))], na.rm = TRUE)
    start_steelhead_df[i,12] <- sum(data_year$flow[data_year$doy < data_year$doy[start]]
                                > 500) #defining flood days as flow > 500
    start_steelhead_df[i,13] <- sum(data_year$flow[intersect(which(
      data_year$doy > 31), 
      which(data_year$doy < 60))] > 500)
    start_steelhead_df[i,14] <- sum(data_year$flow[intersect(which(
      data_year$doy > 59), 
      which(data_year$doy < 92))] > 500)
    start_steelhead_df[i,15] <- data_year$cum_steelhead_wild[length(
      data_year$cum_steelhead_wild)]
    
    middle_steelhead_df[i,1] <- data_year$year[middle]
    middle_steelhead_df[i,2] <- data_year$doy[middle]
    middle_steelhead_df[i,3] <- data_year$temp[middle]
    middle_steelhead_df[i,4] <- data_year$atu_solstice[middle]
    middle_steelhead_df[i,5] <- data_year$flow[middle]
    middle_steelhead_df[i,6] <- data_year$photoperiod[middle]
    middle_steelhead_df[i,7] <- data_year$atu_april[middle]
    
    end_steelhead_df[i,1] <- data_year$year[end]
    end_steelhead_df[i,2] <- data_year$doy[end]
    end_steelhead_df[i,3] <- data_year$temp[end]
    end_steelhead_df[i,4] <- data_year$atu_solstice[end]
    end_steelhead_df[i,5] <- data_year$flow[end]
    end_steelhead_df[i,6] <- data_year$photoperiod[end]
    end_steelhead_df[i,7] <- data_year$atu_april[end]
    end_steelhead_df[i,8] <- data_year$doy[end] - data_year$doy[start]
  }
  
  
}

#columns for start - year, doy, mean temp before, mean temp feb,
# mean  temp march,  flood days before, flood days in feb,
#flood days march,  total wild
start_chinook1_df <- as.data.frame(start_chinook1_df)
colnames(start_chinook1_df) <- c('year', 'start_chinook1', 'mean_temp',
                                 'mean_temp_feb', 'mean_temp_mar',
                                 'flood_days', 'flood_days_feb', 'flood_days_mar',
                                 'total_wild')
#columns for middle - year, doy, mean temp between start and middle,
#mean temp_april, flood days between start and middle, flood days april,
#total_wild
middle_chinook1_df <- as.data.frame(middle_chinook1_df)
colnames(middle_chinook1_df) <- c('year', 'middle_chinook1', 'mean_temp', 
                                  'mean_temp_april', 'flood_days',
                                 'flood_days_april', 'total_wild','hatchery')

end_chinook1_df <- as.data.frame(end_chinook1_df)
colnames(end_chinook1_df) <- c('year', 'end_chinook1', 'mean_temp', 
                                 'flood_days', 'hatchery')

#year
#number of hatchery fish released within duration
#mean temp within duration
#number of flood days within duration
#number of fish that migrate

duration_chinook1_df <- as.data.frame(duration_chinook1_df)
colnames(duration_chinook1_df) <- c('year', 'duration', 'total_hatchery', 'mean_temp', 
                               'flood_days', 'total_wild')


#figures

p1 <- ggplot(data = start_chinook1_df, aes(x = year, y = start_chinook1)) + geom_point(size = 2)+ 
  theme(panel.background = element_rect(fill=NA, 
                                        color="gray50"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  #how to set limits on the x axis or y axis (does not clip values outside lims)
  coord_cartesian(ylim=c(30,110)) +
  ylab('Start of migration')+ 
  xlab('Year')+ 
  scale_y_continuous(expand = c(0, 0), breaks = c(32,60,91, 121), labels = c("Feb", "Mar","Apr", "May")) +
  labs(title = 'Chinook yearlings')
p1

ggsave(filename=here("Documents","output","pied_piper","start_migration_chinook1.png"), plot=p1, 
       width=12, height=6, dpi=300)

p_temp <- ggplot(data = start_chinook1_df, aes(x = mean_temp_feb, y = start_chinook1)) + geom_point(size = 2)+ 
  theme(panel.background = element_rect(fill=NA, 
                                        color="gray50"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  #how to set limits on the x axis or y axis (does not clip values outside lims)
  #coord_cartesian(ylim=c(30,110)) +
  ylab('Start of migration')+ 
  xlab('Mean temperature in Feb')+ 
  #scale_y_continuous(expand = c(0, 0), breaks = c(32,60,91, 121), labels = c("Feb", "Mar","Apr", "May")) +
  labs(title = 'Chinook yearlings')
p_temp
ggsave(filename=here("Documents","output","pied_piper","start_migration__febtemp_chinook1.png"), plot=p_temp, 
       width=12, height=6, dpi=300)

p_duration <- ggplot(data = duration_chinook1_df, aes(x = total_hatchery, y = duration)) + geom_point(size = 2)+ 
  theme(panel.background = element_rect(fill=NA, 
                                        color="gray50"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  #how to set limits on the x axis or y axis (does not clip values outside lims)
  #coord_cartesian(ylim=c(30,110)) +
  ylab('Duration of migration')+ 
  xlab('Number of hatchery fish released')+ 
  #scale_y_continuous(expand = c(0, 0), breaks = c(32,60,91, 121), labels = c("Feb", "Mar","Apr", "May")) +
  labs(title = 'Chinook yearlings')
p_duration
ggsave(filename=here("Documents","output","pied_piper","duration_hatchery_chinook1.png"), plot=p_duration, 
       width=12, height=6, dpi=300)

#########################################
# Coho

##start

#all of the previous columns
#mean temp before, mean temp feb,
# mean  temp march,flood days before, flood days in feb,
#flood days march, total wild

start_coho1_df <- as.data.frame(start_coho1_df)
colnames(start_coho1_df) <- c('year', 'start_coho1', 'temp', 'atu_solstice', 
                              'flow', 'photoperiod', 'atu_april', 'atu_mar10',
                              'mean_temp', 'mean_temp_feb', 'mean_temp_mar',
                              'flood_days', 'flood_days_feb', 'flood_days_mar',
                              'total_wild')




p2 <- ggplot(data = start_coho1_df, aes(x = year, y = start_coho1)) + geom_point(size = 2)+ 
  theme(panel.background = element_rect(fill=NA, 
                                        color="gray50"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  #how to set limits on the x axis or y axis (does not clip values outside lims)
  coord_cartesian(ylim=c(40,130)) +
  ylab('Start of migration')+ 
  xlab('Year')+ 
  scale_y_continuous(expand = c(0, 0), breaks = c(60,91, 121), labels = c("Mar","Apr", "May")) +
  labs(title = 'Coho')
p2
ggsave(filename=here("Documents","output","pied_piper","start_coho1.png"), plot=p2, 
       width=12, height=6, dpi=300)


start_steelhead_df <- as.data.frame(start_steelhead_df)
colnames(start_steelhead_df) <- c('year', 'start_steelhead', 'temp', 'atu_solstice', 
                                  'flow', 'photoperiod', 'atu_april', 'atu_mar10',
                                  'mean_temp', 'mean_temp_feb', 'mean_temp_mar',
                                  'flood_days', 'flood_days_feb', 'flood_days_mar',
                                  'total_wild')


p3 <- ggplot(data = start_steelhead_df, aes(x = year, y = start_steelhead)) + geom_point(size = 2)+ 
  theme(panel.background = element_rect(fill=NA, 
                                        color="gray50"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  #how to set limits on the x axis or y axis (does not clip values outside lims)
  coord_cartesian(ylim=c(91,134)) +
  ylab('Start of migration')+ 
  xlab('Year')+ 
  scale_y_continuous(expand = c(0, 0), breaks = c(60,91, 105, 121), labels = c("Mar","Apr", "15 Apr","May")) +
  labs(title = 'Steelhead')
p3
ggsave(filename=here("Documents","output","pied_piper","start_steelhead.png"), plot=p3, 
       width=12, height=6, dpi=300)

library(patchwork)
p <- p1+ p2 + p3
p

ggsave(filename=here("Documents","output","pied_piper","start_chinook1_coho1_steelhead.png"), plot=p, 
       width=15, height=6, dpi=300)

#trying linear model 
start_model <- lm(start_chinook1 ~ year + mean_temp_feb + mean_temp_mar + 
                    flood_days_feb + flood_days_mar + total_wild, start_chinook1_df)
summary(start_model)
plot(fitted(start_model),residuals(start_model))

start_model <- lm(start_chinook1 ~ year + mean_temp_feb +
                    flood_days_feb +  total_wild, start_chinook1_df)
summary(start_model)
plot(fitted(start_model),residuals(start_model))

start_model <- lm(start_chinook1 ~ mean_temp_feb +
                    flood_days_feb +  total_wild, start_chinook1_df)
summary(start_model)
plot(fitted(start_model),residuals(start_model))

start_model <- lm(year ~ mean_temp_feb + flood_days_feb, start_chinook1_df)
summary(start_model) # temp or flood days is not significant
plot(fitted(start_model),residuals(start_model))

#trying linear model fir duration
duration_model <- lm(duration ~ year + total_hatchery + total_wild, 
                     duration_chinook1_df)
summary(duration_model)
plot(fitted(start_model),residuals(start_model))


##############
#Deviation of fish per hour from mean, normal distribution

library(here)
require(MASS)
require(lme4)
require(nlme)
library(MuMIn)
require(modelr)

agg_data_w_ma_msd <- read.csv(here("Documents","data","pied_piper","dungeness_2005-2020_combine_ma_msd_atu_photoperiod.csv"),header=TRUE)#,na.strings=c("[nan]"))


#use residual of temperature as covariate
hist(agg_data_w_ma_msd$temp)

lm_temp <- lme(temp ~ doy, random = ~1+doy|year, data = agg_data_w_ma_msd, 
               correlation =  corAR1(form = ~doy|year), na.action=na.exclude)
summary(lm_temp)
plot(lm_temp$fitted, lm_temp$residuals)
acf(lm_temp$residuals)




data <- agg_data_w_ma_msd %>% add_residuals(lm_temp)


#chinook0
data$chinook0_deviation_day <- data$chinook0_wild_perhour - 
  data$ma_chinook0_wild_perhour

data$chinook0_deviation_scaled_day <- (
  data$chinook0_deviation_day)/(data$msd_chinook0_wild_perhour+1)

mm <- lme(chinook0_deviation_scaled_day  ~ temp + flow + 
            atu_solstice + I(atu_solstice^2) + 
            photoperiod + resid + chinook0_hatchery_perhour + 
            chinook1_hatchery_perhour + coho1_hatchery_perhour + 
            steelheadsmolt_hatchery_perhour, random = ~ 1|year, correlation = corAR1(form = ~doy|year), data = data, na.action=na.exclude)
summary(mm)
acf(residuals(mm, retype="normalized"), na.action=na.exclude)
plot(fitted(mm), residuals(mm))
r.squaredGLMM(mm)

chinook0_dev <- matrix(ncol = 13, nrow = 10)
chinook0_dev <- as.data.frame(chinook0_dev)
colnames(chinook0_dev) <- c('int', 'temp', 'flow', 'atu_solstice', 'atu^2', 'photoperiod', 'residuals', 'chinook0_h','chinook1_h','coho1_h','steelheadsmolt_h', 'AIC', 'Delta AIC')
chinook0_dev[1,1] <- summary(mm)$tTable[1,5]
chinook0_dev[1,2] <- summary(mm)$tTable[2,5]
chinook0_dev[1,3] <- summary(mm)$tTable[3,5]
chinook0_dev[1,4] <- summary(mm)$tTable[4,5]
chinook0_dev[1,5] <- summary(mm)$tTable[5,5]
chinook0_dev[1,6] <- summary(mm)$tTable[6,5]
chinook0_dev[1,7] <- summary(mm)$tTable[7,5]
chinook0_dev[1,8] <- summary(mm)$tTable[8,5]
chinook0_dev[1,9] <- summary(mm)$tTable[9,5]
chinook0_dev[1,10] <- summary(mm)$tTable[10,5]
chinook0_dev[1,11] <- summary(mm)$tTable[11,5]
chinook0_dev[1,12] <- summary(mm)$AIC

mm <- lme(chinook0_deviation_scaled_day  ~  flow + 
            atu_solstice + I(atu_solstice^2) + 
            photoperiod + resid + chinook0_hatchery_perhour + 
            chinook1_hatchery_perhour + coho1_hatchery_perhour + 
            steelheadsmolt_hatchery_perhour, random = ~ 1|year, correlation = corAR1(form = ~doy|year), data = data, na.action=na.exclude)
summary(mm)
acf(residuals(mm, retype="normalized"), na.action=na.exclude)
plot(fitted(mm), residuals(mm))


chinook0_dev[2,1] <- summary(mm)$tTable[1,5]
chinook0_dev[2,2] <- NA
chinook0_dev[2,3] <- summary(mm)$tTable[2,5]
chinook0_dev[2,4] <- summary(mm)$tTable[3,5]
chinook0_dev[2,5] <- summary(mm)$tTable[4,5]
chinook0_dev[2,6] <- summary(mm)$tTable[5,5]
chinook0_dev[2,7] <- summary(mm)$tTable[6,5]
chinook0_dev[2,8] <- summary(mm)$tTable[7,5]
chinook0_dev[2,9] <- summary(mm)$tTable[8,5]
chinook0_dev[2,10] <- summary(mm)$tTable[9,5]
chinook0_dev[2,11] <- summary(mm)$tTable[10,5]
chinook0_dev[2,12] <- summary(mm)$AIC

mm <- lme(chinook0_deviation_scaled_day  ~ temp + 
            atu_solstice + I(atu_solstice^2) + 
            photoperiod + resid + chinook0_hatchery_perhour + 
            chinook1_hatchery_perhour + coho1_hatchery_perhour + 
            steelheadsmolt_hatchery_perhour, random = ~ 1|year, correlation = corAR1(form = ~doy|year), data = data, na.action=na.exclude)
summary(mm)
acf(residuals(mm, retype="normalized"), na.action=na.exclude)
plot(fitted(mm), residuals(mm))

chinook0_dev[3,1] <- summary(mm)$tTable[1,5]
chinook0_dev[3,2] <- summary(mm)$tTable[2,5]
chinook0_dev[3,3] <- NA
chinook0_dev[3,4] <- summary(mm)$tTable[3,5]
chinook0_dev[3,5] <- summary(mm)$tTable[4,5]
chinook0_dev[3,6] <- summary(mm)$tTable[5,5]
chinook0_dev[3,7] <- summary(mm)$tTable[6,5]
chinook0_dev[3,8] <- summary(mm)$tTable[7,5]
chinook0_dev[3,9] <- summary(mm)$tTable[8,5]
chinook0_dev[3,10] <- summary(mm)$tTable[9,5]
chinook0_dev[3,11] <- summary(mm)$tTable[10,5]
chinook0_dev[3,12] <- summary(mm)$AIC

mm <- lme(chinook0_deviation_scaled_day  ~ 
            atu_solstice + I(atu_solstice^2) + 
            photoperiod + resid + chinook0_hatchery_perhour + 
            chinook1_hatchery_perhour + coho1_hatchery_perhour + 
            steelheadsmolt_hatchery_perhour, random = ~ 1|year, correlation = corAR1(form = ~doy|year), data = data, na.action=na.exclude)
summary(mm)
acf(residuals(mm, retype="normalized"), na.action=na.exclude)
plot(fitted(mm), residuals(mm))

chinook0_dev[4,1] <- summary(mm)$tTable[1,5]
chinook0_dev[4,2] <- NA
chinook0_dev[4,3] <- NA
chinook0_dev[4,4] <- summary(mm)$tTable[2,5]
chinook0_dev[4,5] <- summary(mm)$tTable[3,5]
chinook0_dev[4,6] <- summary(mm)$tTable[4,5]
chinook0_dev[4,7] <- summary(mm)$tTable[5,5]
chinook0_dev[4,8] <- summary(mm)$tTable[6,5]
chinook0_dev[4,9] <- summary(mm)$tTable[7,5]
chinook0_dev[4,10] <- summary(mm)$tTable[8,5]
chinook0_dev[4,11] <- summary(mm)$tTable[9,5]
chinook0_dev[4,12] <- summary(mm)$AIC


mm <- lme(chinook0_deviation_scaled_day  ~ temp + flow+
            atu_solstice + I(atu_solstice^2) + photoperiod + resid  + 
            chinook1_hatchery_perhour + coho1_hatchery_perhour + 
            steelheadsmolt_hatchery_perhour, random = ~ 1|year, correlation = corAR1(form = ~doy|year), data = data, na.action=na.exclude)
summary(mm)
acf(residuals(mm, retype="normalized"), na.action=na.exclude)
plot(fitted(mm), residuals(mm))

chinook0_dev[5,1] <- summary(mm)$tTable[1,5]
chinook0_dev[5,2] <- summary(mm)$tTable[2,5]
chinook0_dev[5,3] <- summary(mm)$tTable[3,5]
chinook0_dev[5,4] <- summary(mm)$tTable[4,5]
chinook0_dev[5,5] <- summary(mm)$tTable[5,5]
chinook0_dev[5,6] <- summary(mm)$tTable[6,5]
chinook0_dev[5,7] <- summary(mm)$tTable[7,5]
chinook0_dev[5,8] <- NA
chinook0_dev[5,9] <- summary(mm)$tTable[8,5]
chinook0_dev[5,10] <- summary(mm)$tTable[9,5]
chinook0_dev[5,11] <- summary(mm)$tTable[10,5]
chinook0_dev[5,12] <- summary(mm)$AIC

mm <- lme(chinook0_deviation_scaled_day  ~ temp + flow+
            atu_solstice + I(atu_solstice^2) + photoperiod + resid, random = ~ 1|year, correlation = corAR1(form = ~doy|year), data = data, na.action=na.exclude)
summary(mm)
acf(residuals(mm, retype="normalized"), na.action=na.exclude)
plot(fitted(mm), residuals(mm))

chinook0_dev[6,1] <- summary(mm)$tTable[1,5]
chinook0_dev[6,2] <- summary(mm)$tTable[2,5]
chinook0_dev[6,3] <- summary(mm)$tTable[3,5]
chinook0_dev[6,4] <- summary(mm)$tTable[4,5]
chinook0_dev[6,5] <- summary(mm)$tTable[5,5]
chinook0_dev[6,6] <- summary(mm)$tTable[6,5]
chinook0_dev[6,7] <- summary(mm)$tTable[7,5]
chinook0_dev[6,8] <- NA
chinook0_dev[6,9] <- NA
chinook0_dev[6,10] <- NA
chinook0_dev[6,11] <- NA
chinook0_dev[6,12] <- summary(mm)$AIC


mm <- lme(chinook0_deviation_scaled_day  ~ chinook0_hatchery_perhour + 
            chinook1_hatchery_perhour + 
            coho1_hatchery_perhour + 
            steelheadsmolt_hatchery_perhour, random = ~ 1|year, correlation = corAR1(form = ~doy|year), data = data, na.action=na.exclude)
summary(mm)
acf(residuals(mm, retype="normalized"), na.action=na.exclude)
plot(fitted(mm), residuals(mm))

chinook0_dev[7,1] <- summary(mm)$tTable[1,5]
chinook0_dev[7,2] <- NA
chinook0_dev[7,3] <- NA
chinook0_dev[7,4] <- NA
chinook0_dev[7,5] <- NA
chinook0_dev[7,6] <- NA
chinook0_dev[7,7] <- NA
chinook0_dev[7,8] <- summary(mm)$tTable[2,5]
chinook0_dev[7,9] <- summary(mm)$tTable[3,5]
chinook0_dev[7,10] <- summary(mm)$tTable[4,5]
chinook0_dev[7,11] <- summary(mm)$tTable[5,5]
chinook0_dev[7,12] <- summary(mm)$AIC



mm <- lme(chinook0_deviation_scaled_day  ~ flow+
            photoperiod + resid + chinook0_hatchery_perhour +
            chinook1_hatchery_perhour, random = ~ 1|year, correlation = corAR1(form = ~doy|year), data = data, na.action=na.exclude)
summary(mm)
acf(residuals(mm, retype="normalized"), na.action=na.exclude)
plot(fitted(mm), residuals(mm))

chinook0_dev[8,1] <- summary(mm)$tTable[1,5]
chinook0_dev[8,2] <- NA
chinook0_dev[8,3] <- summary(mm)$tTable[2,5]
chinook0_dev[8,4] <- NA
chinook0_dev[8,5] <- NA
chinook0_dev[8,6] <- summary(mm)$tTable[3,5]
chinook0_dev[8,7] <- summary(mm)$tTable[4,5]
chinook0_dev[8,8] <- summary(mm)$tTable[5,5]
chinook0_dev[8,9] <- summary(mm)$tTable[6,5]
chinook0_dev[8,10] <- NA
chinook0_dev[8,11] <- NA
chinook0_dev[8,12] <- summary(mm)$AIC

mm <- lme(chinook0_deviation_scaled_day  ~ temp + flow+
            photoperiod + resid + chinook0_hatchery_perhour +
            chinook1_hatchery_perhour, random = ~ 1|year, correlation = corAR1(form = ~doy|year), data = data, na.action=na.exclude)
summary(mm)
acf(residuals(mm, retype="normalized"), na.action=na.exclude)
plot(fitted(mm), residuals(mm))

chinook0_dev[9,1] <- summary(mm)$tTable[1,5]
chinook0_dev[9,2] <- summary(mm)$tTable[2,5]
chinook0_dev[9,3] <- summary(mm)$tTable[3,5]
chinook0_dev[9,4] <- NA
chinook0_dev[9,5] <- NA
chinook0_dev[9,6] <- summary(mm)$tTable[4,5]
chinook0_dev[9,7] <- summary(mm)$tTable[5,5]
chinook0_dev[9,8] <- summary(mm)$tTable[6,5]
chinook0_dev[9,9] <- summary(mm)$tTable[7,5]
chinook0_dev[9,10] <- NA
chinook0_dev[9,11] <- NA
chinook0_dev[9,12] <- summary(mm)$AIC


mm <- lme(chinook0_deviation_scaled_day  ~ flow+
            photoperiod + resid + chinook0_hatchery_perhour +
            chinook1_hatchery_perhour + coho1_hatchery_perhour +
            steelheadsmolt_hatchery_perhour , random = ~ 1|year, correlation = corAR1(form = ~doy|year), data = data, na.action=na.exclude)
summary(mm)
acf(residuals(mm, retype="normalized"), na.action=na.exclude)
plot(fitted(mm), residuals(mm))

chinook0_dev[10,1] <- summary(mm)$tTable[1,5]
chinook0_dev[10,2] <- NA
chinook0_dev[10,3] <- summary(mm)$tTable[2,5]
chinook0_dev[10,4] <- NA
chinook0_dev[10,5] <- NA
chinook0_dev[10,6] <- summary(mm)$tTable[3,5]
chinook0_dev[10,7] <- summary(mm)$tTable[4,5]
chinook0_dev[10,8] <- summary(mm)$tTable[5,5]
chinook0_dev[10,9] <- summary(mm)$tTable[6,5]
chinook0_dev[10,10] <- summary(mm)$tTable[7,5]
chinook0_dev[10,11] <- summary(mm)$tTable[8,5]
chinook0_dev[10,12] <- summary(mm)$AIC

mm <- lme(chinook0_deviation_scaled_day  ~ flow+
            atu_solstice + photoperiod + resid + chinook0_hatchery_perhour +
            chinook1_hatchery_perhour + coho1_hatchery_perhour +
            steelheadsmolt_hatchery_perhour , random = ~ 1|year, correlation = corAR1(form = ~doy|year), data = data, na.action=na.exclude)
summary(mm)
acf(residuals(mm, retype="normalized"), na.action=na.exclude)
plot(fitted(mm), residuals(mm))
r.squaredGLMM(mm)

chinook0_dev[11,1] <- summary(mm)$tTable[1,5]
chinook0_dev[11,2] <- NA
chinook0_dev[11,3] <- summary(mm)$tTable[2,5]
chinook0_dev[11,4] <- summary(mm)$tTable[3,5]
chinook0_dev[11,5] <- NA
chinook0_dev[11,6] <- summary(mm)$tTable[4,5]
chinook0_dev[11,7] <- summary(mm)$tTable[5,5]
chinook0_dev[11,8] <- summary(mm)$tTable[6,5]
chinook0_dev[11,9] <- summary(mm)$tTable[7,5]
chinook0_dev[11,10] <- summary(mm)$tTable[8,5]
chinook0_dev[11,11] <- summary(mm)$tTable[9,5]
chinook0_dev[11,12] <- summary(mm)$AIC

qqnorm(residuals(mm), main= "")
qqline(residuals(mm))

for(i in 1:11){
  chinook0_dev[i,13] <- chinook0_dev[i,12] - min(chinook0_dev[,12])
}


output_df <- chinook0_dev[order(chinook0_dev$'Delta AIC'),]
write.csv(output_df, here("Documents","output","pied_piper","chinook0_dev_aic.csv"))

#need to add flow deviation

##############################################################
#### chum
data$chum0_deviation_day <- data$chum0_wild_perhour - 
  data$ma_chum0_wild_perhour

data$chum0_deviation_scaled_day <- (
  data$chum0_deviation_day)/(data$msd_chum0_wild_perhour+1)


data$flow_deviation <- data$flow - data$ma_flow

mm <- lme(chum0_deviation_scaled_day  ~ temp + flow + flow_deviation +
            atu_solstice + I(atu_solstice^2) + 
            photoperiod + resid + chinook0_hatchery_perhour + 
            chinook1_hatchery_perhour + coho1_hatchery_perhour + 
            steelheadsmolt_hatchery_perhour + pink0_hatchery_perhour, 
          random = ~ 1|year, correlation = corAR1(form = ~doy|year), data = data, na.action=na.exclude)
summary(mm)
acf(residuals(mm, retype="normalized"), na.action=na.exclude)
plot(fitted(mm), residuals(mm))

chum0_dev <- matrix(ncol = 15, nrow = 10)
chum0_dev <- as.data.frame(chum0_dev)
colnames(chum0_dev) <- c('int', 'temp', 'flow', 'flow_deviation','atu_solstice', 'atu^2', 'photoperiod', 'residuals', 'chinook0_h','chinook1_h','coho1_h','steelheadsmolt_h','pink_h', 'AIC', 'Delta AIC')
chum0_dev[1,1] <- summary(mm)$tTable[1,5]
chum0_dev[1,2] <- summary(mm)$tTable[2,5]
chum0_dev[1,3] <- summary(mm)$tTable[3,5]
chum0_dev[1,4] <- summary(mm)$tTable[4,5]
chum0_dev[1,5] <- summary(mm)$tTable[5,5]
chum0_dev[1,6] <- summary(mm)$tTable[6,5]
chum0_dev[1,7] <- summary(mm)$tTable[7,5]
chum0_dev[1,8] <- summary(mm)$tTable[8,5]
chum0_dev[1,9] <- summary(mm)$tTable[9,5]
chum0_dev[1,10] <- summary(mm)$tTable[10,5]
chum0_dev[1,11] <- summary(mm)$tTable[11,5]
chum0_dev[1,12] <- summary(mm)$tTable[12,5]
chum0_dev[1,13] <- summary(mm)$tTable[13,5]
chum0_dev[1,14] <- summary(mm)$AIC


mm <- lme(chum0_deviation_scaled_day  ~ temp + flow + flow_deviation +
            atu_solstice + I(atu_solstice^2) + 
            photoperiod + resid, 
          random = ~ 1|year, correlation = corAR1(form = ~doy|year), data = data, na.action=na.exclude)
summary(mm)
acf(residuals(mm, retype="normalized"), na.action=na.exclude)
plot(fitted(mm), residuals(mm))

chum0_dev[2,1] <- summary(mm)$tTable[1,5]
chum0_dev[2,2] <- summary(mm)$tTable[2,5]
chum0_dev[2,3] <- summary(mm)$tTable[3,5]
chum0_dev[2,4] <- summary(mm)$tTable[4,5]
chum0_dev[2,5] <- summary(mm)$tTable[5,5]
chum0_dev[2,6] <- summary(mm)$tTable[6,5]
chum0_dev[2,7] <- summary(mm)$tTable[7,5]
chum0_dev[2,8] <- summary(mm)$tTable[8,5]
chum0_dev[2,9] <- NA
chum0_dev[2,10] <- NA
chum0_dev[2,11] <- NA
chum0_dev[2,12] <- NA
chum0_dev[2,13] <- NA
chum0_dev[2,14] <- summary(mm)$AIC


mm <- lme(chum0_deviation_scaled_day  ~ temp + flow + flow_deviation +
            atu_solstice + I(atu_solstice^2) + 
            photoperiod, 
          random = ~ 1|year, correlation = corAR1(form = ~doy|year), data = data, na.action=na.exclude)
summary(mm)
acf(residuals(mm, retype="normalized"), na.action=na.exclude)
plot(fitted(mm), residuals(mm))

chum0_dev[3,1] <- summary(mm)$tTable[1,5]
chum0_dev[3,2] <- summary(mm)$tTable[2,5]
chum0_dev[3,3] <- summary(mm)$tTable[3,5]
chum0_dev[3,4] <- summary(mm)$tTable[4,5]
chum0_dev[3,5] <- summary(mm)$tTable[5,5]
chum0_dev[3,6] <- summary(mm)$tTable[6,5]
chum0_dev[3,7] <- summary(mm)$tTable[7,5]
chum0_dev[3,8] <- NA
chum0_dev[3,9] <- NA
chum0_dev[3,10] <- NA
chum0_dev[3,11] <- NA
chum0_dev[3,12] <- NA
chum0_dev[3,13] <- NA
chum0_dev[3,14] <- summary(mm)$AIC

mm <- lme(chum0_deviation_scaled_day  ~ flow + flow_deviation+
            atu_solstice, 
          random = ~ 1|year, correlation = corAR1(form = ~doy|year), data = data, na.action=na.exclude)
summary(mm)
acf(residuals(mm, retype="normalized"), na.action=na.exclude)
plot(fitted(mm), residuals(mm))

chum0_dev[4,1] <- summary(mm)$tTable[1,5]
chum0_dev[4,2] <- NA
chum0_dev[4,3] <- summary(mm)$tTable[2,5]
chum0_dev[4,4] <- summary(mm)$tTable[3,5]
chum0_dev[4,5] <- summary(mm)$tTable[4,5]
chum0_dev[4,6] <- NA
chum0_dev[4,7] <- NA
chum0_dev[4,8] <- NA
chum0_dev[4,9] <- NA
chum0_dev[4,10] <- NA
chum0_dev[4,11] <- NA
chum0_dev[4,12] <- NA
chum0_dev[4,13] <- NA
chum0_dev[4,14] <- summary(mm)$AIC


mm <- lme(chum0_deviation_scaled_day  ~ flow + flow_deviation+
            atu_solstice + I(atu_solstice^2), 
          random = ~ 1|year, correlation = corAR1(form = ~doy|year), data = data, na.action=na.exclude)
summary(mm)
acf(residuals(mm, retype="normalized"), na.action=na.exclude)
plot(fitted(mm), residuals(mm))

chum0_dev[5,1] <- summary(mm)$tTable[1,5]
chum0_dev[5,2] <- NA
chum0_dev[5,3] <- summary(mm)$tTable[2,5]
chum0_dev[5,4] <- summary(mm)$tTable[3,5]
chum0_dev[5,5] <- summary(mm)$tTable[4,5]
chum0_dev[5,6] <- summary(mm)$tTable[5,5]
chum0_dev[5,7] <- NA
chum0_dev[5,8] <- NA
chum0_dev[5,9] <- NA
chum0_dev[5,10] <- NA
chum0_dev[5,11] <- NA
chum0_dev[5,12] <- NA
chum0_dev[5,13] <- NA
chum0_dev[5,14] <- summary(mm)$AIC

mm <- lme(chum0_deviation_scaled_day  ~ flow + flow_deviation+
            atu_solstice + I(atu_solstice^2), 
          random = ~ 1|year, correlation = corAR1(form = ~doy|year), data = data, na.action=na.exclude)
summary(mm)
acf(residuals(mm, retype="normalized"), na.action=na.exclude)
plot(fitted(mm), residuals(mm))

chum0_dev[5,1] <- summary(mm)$tTable[1,5]
chum0_dev[5,2] <- NA
chum0_dev[5,3] <- summary(mm)$tTable[2,5]
chum0_dev[5,4] <- summary(mm)$tTable[3,5]
chum0_dev[5,5] <- summary(mm)$tTable[4,5]
chum0_dev[5,6] <- summary(mm)$tTable[5,5]
chum0_dev[5,7] <- NA
chum0_dev[5,8] <- NA
chum0_dev[5,9] <- NA
chum0_dev[5,10] <- NA
chum0_dev[5,11] <- NA
chum0_dev[5,12] <- NA
chum0_dev[5,13] <- NA
chum0_dev[5,14] <- summary(mm)$AIC



mm <- lme(chum0_deviation_scaled_day  ~ flow + flow_deviation+
            atu_solstice + photoperiod, 
          random = ~ 1|year, correlation = corAR1(form = ~doy|year), data = data, na.action=na.exclude)
summary(mm)
acf(residuals(mm, retype="normalized"), na.action=na.exclude)
plot(fitted(mm), residuals(mm))

chum0_dev[6,1] <- summary(mm)$tTable[1,5]
chum0_dev[6,2] <- NA
chum0_dev[6,3] <- summary(mm)$tTable[2,5]
chum0_dev[6,4] <- summary(mm)$tTable[3,5]
chum0_dev[6,5] <- summary(mm)$tTable[4,5]
chum0_dev[6,6] <- NA
chum0_dev[6,7] <- summary(mm)$tTable[5,5]
chum0_dev[6,8] <- NA
chum0_dev[6,9] <- NA
chum0_dev[6,10] <- NA
chum0_dev[6,11] <- NA
chum0_dev[6,12] <- NA
chum0_dev[6,13] <- NA
chum0_dev[6,14] <- summary(mm)$AIC



mm <- lme(chum0_deviation_scaled_day  ~ 1, 
          random = ~ 1|year, correlation = corAR1(form = ~doy|year), data = data, na.action=na.exclude)
summary(mm)
acf(residuals(mm, retype="normalized"), na.action=na.exclude)
plot(fitted(mm), residuals(mm))

chum0_dev[7,1] <- summary(mm)$tTable[1,5]
chum0_dev[7,2] <- NA
chum0_dev[7,3] <- NA
chum0_dev[7,4] <- NA
chum0_dev[7,5] <- NA
chum0_dev[7,6] <- NA
chum0_dev[7,7] <- NA
chum0_dev[7,8] <- NA
chum0_dev[7,9] <- NA
chum0_dev[7,10] <- NA
chum0_dev[7,11] <- NA
chum0_dev[7,12] <- NA
chum0_dev[7,13] <- NA
chum0_dev[7,14] <- summary(mm)$AIC


mm <- lme(chum0_deviation_scaled_day  ~ flow_deviation, 
          random = ~ 1|year, correlation = corAR1(form = ~doy|year), data = data, na.action=na.exclude)
summary(mm)
acf(residuals(mm, retype="normalized"), na.action=na.exclude)
plot(fitted(mm), residuals(mm))

chum0_dev[8,1] <- summary(mm)$tTable[1,5]
chum0_dev[8,2] <- NA
chum0_dev[8,3] <- NA
chum0_dev[8,4] <- summary(mm)$tTable[2,5]
chum0_dev[8,5] <- NA
chum0_dev[8,6] <- NA
chum0_dev[8,7] <- NA
chum0_dev[8,8] <- NA
chum0_dev[8,9] <- NA
chum0_dev[8,10] <- NA
chum0_dev[8,11] <- NA
chum0_dev[8,12] <- NA
chum0_dev[8,13] <- NA
chum0_dev[8,14] <- summary(mm)$AIC

mm <- lme(chum0_deviation_scaled_day  ~ flow + flow_deviation+
            atu_solstice + chinook1_hatchery_perhour + coho1_hatchery_perhour, 
          random = ~ 1|year, correlation = corAR1(form = ~doy|year), data = data, na.action=na.exclude)
summary(mm)
acf(residuals(mm, retype="normalized"), na.action=na.exclude)
plot(fitted(mm), residuals(mm))

chum0_dev[9,1] <- summary(mm)$tTable[1,5]
chum0_dev[9,2] <- NA
chum0_dev[9,3] <- summary(mm)$tTable[2,5]
chum0_dev[9,4] <- summary(mm)$tTable[3,5]
chum0_dev[9,5] <- summary(mm)$tTable[4,5]
chum0_dev[9,6] <- NA
chum0_dev[9,7] <- NA
chum0_dev[9,8] <- NA
chum0_dev[9,9] <- NA
chum0_dev[9,10] <- summary(mm)$tTable[5,5]
chum0_dev[9,11] <- summary(mm)$tTable[6,5]
chum0_dev[9,12] <- NA
chum0_dev[9,13] <- NA
chum0_dev[9,14] <- summary(mm)$AIC

mm <- lme(chum0_deviation_scaled_day  ~ flow + flow_deviation+
            atu_solstice + chinook1_hatchery_perhour, 
          random = ~ 1|year, correlation = corAR1(form = ~doy|year), data = data, na.action=na.exclude)
summary(mm)
acf(residuals(mm, retype="normalized"), na.action=na.exclude)
plot(fitted(mm), residuals(mm))

chum0_dev[10,1] <- summary(mm)$tTable[1,5]
chum0_dev[10,2] <- NA
chum0_dev[10,3] <- summary(mm)$tTable[2,5]
chum0_dev[10,4] <- summary(mm)$tTable[3,5]
chum0_dev[10,5] <- summary(mm)$tTable[4,5]
chum0_dev[10,6] <- NA
chum0_dev[10,7] <- NA
chum0_dev[10,8] <- NA
chum0_dev[10,9] <- NA
chum0_dev[10,10] <- summary(mm)$tTable[5,5]
chum0_dev[10,11] <- NA
chum0_dev[10,12] <- NA
chum0_dev[10,13] <- NA
chum0_dev[10,14] <- summary(mm)$AIC

qqnorm(residuals(mm), main= "")
qqline(residuals(mm))

for(i in 1:10){
  chum0_dev[i,15] <- chum0_dev[i,14] - min(chum0_dev[,14], na.rm = TRUE)
}

output_df <- chum0_dev[order(chum0_dev$'Delta AIC'),]
write.csv(output_df, here("Documents","output","pied_piper","chum0_dev_aic.csv"))


##############################################################
#### coho
data$coho1_deviation_day <- data$coho1_wild_perhour - 
  data$ma_coho1_wild_perhour

data$coho1_deviation_scaled_day <- (
  data$coho1_deviation_day)/(data$msd_coho1_wild_perhour+1)

mm <- lme(coho1_deviation_scaled_day  ~ temp + flow + flow_deviation +
            atu_solstice + I(atu_solstice^2) + 
            photoperiod + resid + chinook0_hatchery_perhour + 
            chinook1_hatchery_perhour + coho1_hatchery_perhour + 
            steelheadsmolt_hatchery_perhour + pink0_hatchery_perhour, 
          random = ~ 1|year, correlation = corAR1(form = ~doy|year), data = data, na.action=na.exclude)
summary(mm)
acf(residuals(mm, retype="normalized"), na.action=na.exclude)
plot(fitted(mm), residuals(mm))

coho1_dev <- matrix(ncol = 15, nrow = 10)
coho1_dev <- as.data.frame(coho1_dev)
colnames(coho1_dev) <- c('int', 'temp', 'flow', 'flow_deviation','atu_solstice', 'atu^2', 'photoperiod', 'residuals', 'chinook0_h','chinook1_h','coho1_h','steelheadsmolt_h','pink_h', 'AIC', 'Delta AIC')
coho1_dev[1,1] <- summary(mm)$tTable[1,5]
coho1_dev[1,2] <- summary(mm)$tTable[2,5]
coho1_dev[1,3] <- summary(mm)$tTable[3,5]
coho1_dev[1,4] <- summary(mm)$tTable[4,5]
coho1_dev[1,5] <- summary(mm)$tTable[5,5]
coho1_dev[1,6] <- summary(mm)$tTable[6,5]
coho1_dev[1,7] <- summary(mm)$tTable[7,5]
coho1_dev[1,8] <- summary(mm)$tTable[8,5]
coho1_dev[1,9] <- summary(mm)$tTable[9,5]
coho1_dev[1,10] <- summary(mm)$tTable[10,5]
coho1_dev[1,11] <- summary(mm)$tTable[11,5]
coho1_dev[1,12] <- summary(mm)$tTable[12,5]
coho1_dev[1,13] <- summary(mm)$tTable[13,5]
coho1_dev[1,14] <- summary(mm)$AIC


mm <- lme(coho1_deviation_scaled_day  ~ temp + flow + flow_deviation +
            atu_solstice + I(atu_solstice^2) + 
            photoperiod + resid, 
          random = ~ 1|year, correlation = corAR1(form = ~doy|year), data = data, na.action=na.exclude)
summary(mm)
acf(residuals(mm, retype="normalized"), na.action=na.exclude)
plot(fitted(mm), residuals(mm))

coho1_dev[2,1] <- summary(mm)$tTable[1,5]
coho1_dev[2,2] <- summary(mm)$tTable[2,5]
coho1_dev[2,3] <- summary(mm)$tTable[3,5]
coho1_dev[2,4] <- summary(mm)$tTable[4,5]
coho1_dev[2,5] <- summary(mm)$tTable[5,5]
coho1_dev[2,6] <- summary(mm)$tTable[6,5]
coho1_dev[2,7] <- summary(mm)$tTable[7,5]
coho1_dev[2,8] <- summary(mm)$tTable[8,5]
coho1_dev[2,9] <- NA
coho1_dev[2,10] <- NA
coho1_dev[2,11] <- NA
coho1_dev[2,12] <- NA
coho1_dev[2,13] <- NA
coho1_dev[2,14] <- summary(mm)$AIC


qqnorm(residuals(mm), main= "")
qqline(residuals(mm))


mm <- lme(coho1_deviation_scaled_day  ~ 1, 
          random = ~ 1|year, correlation = corAR1(form = ~doy|year), data = data, na.action=na.exclude)
summary(mm)
acf(residuals(mm, retype="normalized"), na.action=na.exclude)
plot(fitted(mm), residuals(mm))

coho1_dev[3,1] <- summary(mm)$tTable[1,5]
coho1_dev[3,2] <- NA
coho1_dev[3,3] <- NA
coho1_dev[3,4] <- NA
coho1_dev[3,5] <- NA
coho1_dev[3,6] <- NA
coho1_dev[3,7] <- NA
coho1_dev[3,8] <- NA
coho1_dev[3,9] <- NA
coho1_dev[3,10] <- NA
coho1_dev[3,11] <- NA
coho1_dev[3,12] <- NA
coho1_dev[3,13] <- NA
coho1_dev[3,14] <- summary(mm)$AIC


qqnorm(residuals(mm), main= "")
qqline(residuals(mm))

mm <- lme(coho1_deviation_scaled_day  ~ flow_deviation + resid, 
          random = ~ 1|year, correlation = corAR1(form = ~doy|year), data = data, na.action=na.exclude)
summary(mm)
acf(residuals(mm, retype="normalized"), na.action=na.exclude)
plot(fitted(mm), residuals(mm))

coho1_dev[4,1] <- summary(mm)$tTable[1,5]
coho1_dev[4,2] <- NA
coho1_dev[4,3] <- NA
coho1_dev[4,4] <- summary(mm)$tTable[2,5]
coho1_dev[4,5] <- NA
coho1_dev[4,6] <- NA
coho1_dev[4,7] <- NA
coho1_dev[4,8] <- summary(mm)$tTable[3,5]
coho1_dev[4,9] <- NA
coho1_dev[4,10] <- NA
coho1_dev[4,11] <- NA
coho1_dev[4,12] <- NA
coho1_dev[4,13] <- NA
coho1_dev[4,14] <- summary(mm)$AIC

mm <- lme(coho1_deviation_scaled_day  ~ flow_deviation + photoperiod + resid, 
          random = ~ 1|year, correlation = corAR1(form = ~doy|year), data = data, na.action=na.exclude)
summary(mm)
acf(residuals(mm, retype="normalized"), na.action=na.exclude)
plot(fitted(mm), residuals(mm))

coho1_dev[5,1] <- summary(mm)$tTable[1,5]
coho1_dev[5,2] <- NA
coho1_dev[5,3] <- NA
coho1_dev[5,4] <- summary(mm)$tTable[2,5]
coho1_dev[5,5] <- NA
coho1_dev[5,6] <- NA
coho1_dev[5,7] <- summary(mm)$tTable[3,5]
coho1_dev[5,8] <- summary(mm)$tTable[4,5]
coho1_dev[5,9] <- NA
coho1_dev[5,10] <- NA
coho1_dev[5,11] <- NA
coho1_dev[5,12] <- NA
coho1_dev[5,13] <- NA
coho1_dev[5,14] <- summary(mm)$AIC

mm <- lme(coho1_deviation_scaled_day  ~ flow_deviation + atu_solstice + resid, 
          random = ~ 1|year, correlation = corAR1(form = ~doy|year), data = data, na.action=na.exclude)
summary(mm)
acf(residuals(mm, retype="normalized"), na.action=na.exclude)
plot(fitted(mm), residuals(mm))

coho1_dev[6,1] <- summary(mm)$tTable[1,5]
coho1_dev[6,2] <- NA
coho1_dev[6,3] <- NA
coho1_dev[6,4] <- summary(mm)$tTable[2,5]
coho1_dev[6,5] <- summary(mm)$tTable[3,5]
coho1_dev[6,6] <- NA
coho1_dev[6,7] <- NA
coho1_dev[6,8] <- summary(mm)$tTable[4,5]
coho1_dev[6,9] <- NA
coho1_dev[6,10] <- NA
coho1_dev[6,11] <- NA
coho1_dev[6,12] <- NA
coho1_dev[6,13] <- NA
coho1_dev[6,14] <- summary(mm)$AIC

mm <- lme(coho1_deviation_scaled_day  ~ flow_deviation + atu_solstice + resid +
            coho1_hatchery_perhour, 
          random = ~ 1|year, correlation = corAR1(form = ~doy|year), data = data, na.action=na.exclude)
summary(mm)
acf(residuals(mm, retype="normalized"), na.action=na.exclude)
plot(fitted(mm), residuals(mm))

coho1_dev[7,1] <- summary(mm)$tTable[1,5]
coho1_dev[7,2] <- NA
coho1_dev[7,3] <- NA
coho1_dev[7,4] <- summary(mm)$tTable[2,5]
coho1_dev[7,5] <- summary(mm)$tTable[3,5]
coho1_dev[7,6] <- NA
coho1_dev[7,7] <- NA
coho1_dev[7,8] <- summary(mm)$tTable[4,5]
coho1_dev[7,9] <- NA
coho1_dev[7,10] <- NA
coho1_dev[7,11] <- summary(mm)$tTable[5,5]
coho1_dev[7,12] <- NA
coho1_dev[7,13] <- NA
coho1_dev[7,14] <- summary(mm)$AIC

for(i in 1:7){
  coho1_dev[i,15] <- coho1_dev[i,14] - min(coho1_dev[,14], na.rm = TRUE)
}

output_df <- coho1_dev[order(coho1_dev$'Delta AIC'),]
write.csv(output_df, here("Documents","output","pied_piper","coho1_dev_aic.csv"))

####################################

#accounting for ARMA
require(forecast)
auto.arima(data$chinook0_deviation_scaled_day, trace = TRUE) 
#p = 4, q = 2, value = c(0.19,0.11, 0.05,0.04,0.19,0.16)

mm <- lme(chinook0_deviation_scaled_day  ~ flow+
            photoperiod + resid + chinook0_hatchery_perhour +
            chinook1_hatchery_perhour, random = ~ 1|year, correlation = corARMA(value = c(0.19,0.11, 0.05,0.04,0.19,0.16), form = ~doy|year, p = 4, q = 2), data = data, na.action=na.exclude)
summary(mm)
r.squaredGLMM(mm)
acf(residuals(mm, retype="normalized"), na.action=na.exclude)
plot(fitted(mm), residuals(mm))

mm <- lme(chinook0_deviation_scaled_day  ~ flow+ atu_solstice+
            photoperiod + resid + chinook0_hatchery_perhour +
            chinook1_hatchery_perhour + chinook1_hatchery_perhour +
            steelheadsmolt_hatchery_perhour + coho1_hatchery_perhour, random = ~ 1|year, correlation = corARMA(value = c(0.19,0.11, 0.05,0.04,0.19,0.16), form = ~doy|year, p = 4, q = 2), data = data, na.action=na.exclude)
summary(mm)
r.squaredGLMM(mm)
acf(residuals(mm, retype="normalized"), na.action=na.exclude)
plot(fitted(mm), residuals(mm))
qqnorm(residuals(mm))
qqline(residuals(mm))

##############
#Probability of migration, Binomial, sample size changes 











##############
#Daily fish counts, poisson 







