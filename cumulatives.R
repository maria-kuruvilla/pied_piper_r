#calculate cumulative count for each year
#then calculate which date half teh fish have migrated


library(here)
library(tidyverse)
library(patchwork)
agg_data_w_ma_msd <- read.csv(here("Documents","data","pied_piper","dungeness_2005-2020_combine_ma_msd_atu_photoperiod.csv"),header=TRUE)#,na.strings=c("[nan]"))


years = unique(agg_data_w_ma_msd$year)


start_chinook0_df <- matrix(ncol = 8, nrow = length(years))
start_chinook1_df <- matrix(ncol = 8, nrow = length(years))
start_coho1_df <- matrix(ncol = 8, nrow = length(years))
start_steelhead_df <- matrix(ncol = 8, nrow = length(years))

middle_chinook0_df <- matrix(ncol = 7, nrow = length(years))
middle_chinook1_df <- matrix(ncol = 7, nrow = length(years))
middle_coho1_df <- matrix(ncol = 7, nrow = length(years))
middle_steelhead_df <- matrix(ncol = 7, nrow = length(years))

end_chinook0_df <- matrix(ncol = 8, nrow = length(years))
end_chinook1_df <- matrix(ncol = 8, nrow = length(years))
end_coho1_df <- matrix(ncol = 8, nrow = length(years))
end_steelhead_df <- matrix(ncol = 8, nrow = length(years))

for(i in 1:length(years)){
  data_year <- agg_data_w_ma_msd[agg_data_w_ma_msd$year == years[i],]
  
  data_year$cum_chinook0_wild <- cumsum(data_year$chinook0_wild_num)
  
  #end = append(end,which.min(abs(data_year$cum_chinook0_wild - max(data_year$cum_chinook0_wild)*0.9)))
  
  start <- which.min(abs(data_year$cum_chinook0_wild - max(data_year$cum_chinook0_wild)*0.1))
  
  middle <- which.min(abs(data_year$cum_chinook0_wild - max(data_year$cum_chinook0_wild)*0.5))
  
  end <- which.min(abs(data_year$cum_chinook0_wild - max(data_year$cum_chinook0_wild)*0.9))
  
  start_chinook0_df[i,1] <- data_year$year[start]
  start_chinook0_df[i,2] <- data_year$doy[start]
  start_chinook0_df[i,3] <- data_year$temp[start]
  start_chinook0_df[i,4] <- data_year$atu_solstice[start]
  start_chinook0_df[i,5] <- data_year$flow[start]
  start_chinook0_df[i,6] <- data_year$photoperiod[start]
  start_chinook0_df[i,7] <- data_year$atu_april[start]
  start_chinook0_df[i,8] <- data_year$atu_solstice[data_year$doy == 70]
  
  middle_chinook0_df[i,1] <- data_year$year[middle]
  middle_chinook0_df[i,2] <- data_year$doy[middle]
  middle_chinook0_df[i,3] <- data_year$temp[middle]
  middle_chinook0_df[i,4] <- data_year$atu_solstice[middle]
  middle_chinook0_df[i,5] <- data_year$flow[middle]
  middle_chinook0_df[i,6] <- data_year$photoperiod[middle]
  middle_chinook0_df[i,7] <- data_year$atu_april[middle]
  
  end_chinook0_df[i,1] <- data_year$year[end]
  end_chinook0_df[i,2] <- data_year$doy[end]
  end_chinook0_df[i,3] <- data_year$temp[end]
  end_chinook0_df[i,4] <- data_year$atu_solstice[end]
  end_chinook0_df[i,5] <- data_year$flow[end]
  end_chinook0_df[i,6] <- data_year$photoperiod[end]
  end_chinook0_df[i,7] <- data_year$atu_april[end]
  end_chinook0_df[i,8] <- data_year$doy[end] - data_year$doy[start]
  
  
  data_year$cum_chinook1_wild <- cumsum(data_year$chinook1_wild_num)
  start <- which.min(abs(data_year$cum_chinook1_wild - max(data_year$cum_chinook1_wild)*0.1))
  
  middle <- which.min(abs(data_year$cum_chinook1_wild - max(data_year$cum_chinook1_wild)*0.5))
  
  end <- which.min(abs(data_year$cum_chinook1_wild - max(data_year$cum_chinook1_wild)*0.9))
  
  start_chinook1_df[i,1] <- data_year$year[start]
  start_chinook1_df[i,2] <- data_year$doy[start]
  start_chinook1_df[i,3] <- data_year$temp[start]
  start_chinook1_df[i,4] <- data_year$atu_solstice[start]
  start_chinook1_df[i,5] <- data_year$flow[start]
  start_chinook1_df[i,6] <- data_year$photoperiod[start]
  start_chinook1_df[i,7] <- data_year$atu_april[start]
  start_chinook1_df[i,8] <- data_year$atu_solstice[data_year$doy == 70]
  
  middle_chinook1_df[i,1] <- data_year$year[middle]
  middle_chinook1_df[i,2] <- data_year$doy[middle]
  middle_chinook1_df[i,3] <- data_year$temp[middle]
  middle_chinook1_df[i,4] <- data_year$atu_solstice[middle]
  middle_chinook1_df[i,5] <- data_year$flow[middle]
  middle_chinook1_df[i,6] <- data_year$photoperiod[middle]
  middle_chinook1_df[i,7] <- data_year$atu_april[middle]
  
  end_chinook1_df[i,1] <- data_year$year[end]
  end_chinook1_df[i,2] <- data_year$doy[end]
  end_chinook1_df[i,3] <- data_year$temp[end]
  end_chinook1_df[i,4] <- data_year$atu_solstice[end]
  end_chinook1_df[i,5] <- data_year$flow[end]
  end_chinook1_df[i,6] <- data_year$photoperiod[end]
  end_chinook1_df[i,7] <- data_year$atu_april[end]
  end_chinook1_df[i,8] <- data_year$doy[end] - data_year$doy[start]
  
  data_year$cum_coho1_wild <- cumsum(data_year$coho1_wild_num)
  
  #end = append(end,which.min(abs(data_year$cum_chinook0_wild - max(data_year$cum_chinook0_wild)*0.9)))
  
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
  
  data_year$cum_steelhead_wild <- cumsum(data_year$steelheadsmolt_wild_num)
  
  #end = append(end,which.min(abs(data_year$cum_chinook0_wild - max(data_year$cum_chinook0_wild)*0.9)))
  
  start <- which.min(abs(data_year$cum_steelhead_wild - max(data_year$cum_steelhead_wild)*0.1))
  
  middle <- which.min(abs(data_year$cum_steelhead_wild - max(data_year$cum_steelhead_wild)*0.5))
  
  end <- which.min(abs(data_year$cum_steelhead_wild - max(data_year$cum_steelhead_wild)*0.9))
  
  #middle = append(middle,which.min(abs(data_year$cum_chinook0_wild - max(data_year$cum_chinook0_wild)*0.5)))
  
  start_steelhead_df[i,1] <- data_year$year[start]
  start_steelhead_df[i,2] <- data_year$doy[start]
  start_steelhead_df[i,3] <- data_year$temp[start]
  start_steelhead_df[i,4] <- data_year$atu_solstice[start]
  start_steelhead_df[i,5] <- data_year$flow[start]
  start_steelhead_df[i,6] <- data_year$photoperiod[start]
  start_steelhead_df[i,7] <- data_year$atu_april[start]
  start_steelhead_df[i,8] <- data_year$atu_solstice[data_year$doy == 70]
  
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

start_chinook0_df <- as.data.frame(start_chinook0_df)
colnames(start_chinook0_df) <- c('year', 'start_chinook0', 'temp', 'atu_solstice', 'flow', 'photoperiod', 'atu_april', 'atu_mar10')

middle_chinook0_df <- as.data.frame(middle_chinook0_df)
colnames(middle_chinook0_df) <- c('year', 'middle_chinook0', 'temp', 'atu_solstice', 'flow', 'photoperiod', 'atu_april')

end_chinook0_df <- as.data.frame(end_chinook0_df)
colnames(end_chinook0_df) <- c('year', 'end_chinook0', 'temp', 'atu_solstice', 'flow', 'photoperiod', 'atu_april', 'duration')



start_chinook1_df <- as.data.frame(start_chinook1_df)
colnames(start_chinook1_df) <- c('year', 'start_chinook1', 'temp', 'atu_solstice', 'flow', 'photoperiod', 'atu_april', 'atu_mar10')

middle_chinook1_df <- as.data.frame(middle_chinook1_df)
colnames(middle_chinook1_df) <- c('year', 'middle_chinook1', 'temp', 'atu_solstice', 'flow', 'photoperiod', 'atu_april')

end_chinook1_df <- as.data.frame(end_chinook1_df)
colnames(end_chinook1_df) <- c('year', 'end_chinook1', 'temp', 'atu_solstice', 'flow', 'photoperiod', 'atu_april', 'duration')



start_coho1_df <- as.data.frame(start_coho1_df)
colnames(start_coho1_df) <- c('year', 'start_coho1', 'temp', 'atu_solstice', 'flow', 'photoperiod', 'atu_april', 'atu_mar10')

middle_coho1_df <- as.data.frame(middle_coho1_df)
colnames(middle_coho1_df) <- c('year', 'middle_coho1', 'temp', 'atu_solstice', 'flow', 'photoperiod', 'atu_april')

end_coho1_df <- as.data.frame(end_coho1_df)
colnames(end_coho1_df) <- c('year', 'end_coho1', 'temp', 'atu_solstice', 'flow', 'photoperiod', 'atu_april', 'duration')



start_steelhead_df <- as.data.frame(start_steelhead_df)
colnames(start_steelhead_df) <- c('year', 'start_steelhead', 'temp', 'atu_solstice', 'flow', 'photoperiod', 'atu_april', 'atu_mar10')

middle_steelhead_df <- as.data.frame(middle_steelhead_df)
colnames(middle_steelhead_df) <- c('year', 'middle_steelhead', 'temp', 'atu_solstice', 'flow', 'photoperiod', 'atu_april')

end_steelhead_df <- as.data.frame(end_steelhead_df)
colnames(end_steelhead_df) <- c('year', 'end_steelhead', 'temp', 'atu_solstice', 'flow', 'photoperiod', 'atu_april', 'duration')



############################################
#figures
plot(agg_data_w_ma_msd$doy[agg_data_w_ma_msd$year==2020], agg_data_w_ma_msd$chinook0_wild_perhour[agg_data_w_ma_msd$year==2020], 'l')
plot(start_chinook0_df$year, start_chinook0_df$start_chinook0)
plot(start_chinook1_df$year, start_chinook1_df$start_chinook1)
plot(start_coho1_df$year, start_coho1_df$start_coho1)
plot(start_steelhead_df$year, start_steelhead_df$start_steelhead)

plot(start_chinook0_df$atu_april, start_chinook0_df$start_chinook0)
plot(start_chinook1_df$atu_april, start_chinook1_df$start_chinook1)
plot(start_coho1_df$atu_april, start_coho1_df$start_coho1)
plot(start_steelhead_df$atu_april, start_steelhead_df$start_steelhead)

p1 <- ggplot(data = start_chinook0_df, aes(x = year, y = start_chinook0)) + geom_point()+ 
  theme(panel.background = element_rect(fill=NA, 
                                        color="gray50"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  #how to set limits on the x axis or y axis (does not clip values outside lims)
  coord_cartesian(ylim=c(20,130)) +
  ylab('Start of migration')+ 
  xlab('Year')+ 
  scale_y_continuous(expand = c(0, 0), breaks = c(32,60,91, 121), labels = c("Feb", "Mar","Apr", "May")) +
  labs(title = 'Chinook subyearlings')
p1

p2 <- ggplot(data = start_chinook1_df, aes(x = year, y = start_chinook1)) + geom_point()+ 
  theme(panel.background = element_rect(fill=NA, 
                                        color="gray50"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  #how to set limits on the x axis or y axis (does not clip values outside lims)
  coord_cartesian(ylim=c(20,130)) +
  ylab('Start of migration')+ 
  xlab('Year')+  
  scale_y_continuous(expand = c(0, 0), breaks = c(32,60,91, 121), labels = c("Feb", "Mar","Apr", "May")) +
  labs(title = 'Chinook yearlings')
p2

p3 <- ggplot(data = start_coho1_df, aes(x = year, y = start_coho1)) + geom_point()+ 
  theme(panel.background = element_rect(fill=NA, 
                                        color="gray50"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  #how to set limits on the x axis or y axis (does not clip values outside lims)
  coord_cartesian(ylim=c(32,150)) +
  ylab('Start of migration')+ 
  xlab('Year')+ 
  scale_y_continuous(expand = c(0, 0), breaks = c(32,60,91, 121), labels = c("Feb", "Mar","Apr", "May")) +
  labs(title = 'Coho')
p3

p4 <- ggplot(data = start_steelhead_df, aes(x = year, y = start_steelhead)) + geom_point()+ 
  theme(panel.background = element_rect(fill=NA, 
                                        color="gray50"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  #how to set limits on the x axis or y axis (does not clip values outside lims)
  coord_cartesian(ylim=c(32,150)) +
  ylab('Start of migration')+ 
  xlab('Year')+ 
  scale_y_continuous(expand = c(0, 0), breaks = c(32,60,91, 121), labels = c("Feb", "Mar","Apr", "May")) +
  labs(title = 'Steelhead')
p4

plot_start <- (p1+p2+p3+p4)
plot_start
# dev.off()
#gets sent to directory called Figs.
ggsave(filename=here("Documents","output","pied_piper","start_migration.png"), plot=plot_start, 
       width=12, height=6, dpi=300)


##############################################################################
#figures for middle and end
middle1 <- ggplot(data = middle_chinook0_df, aes(x = year, y = middle_chinook0)) + geom_point()+ 
  theme(panel.background = element_rect(fill=NA, 
                                        color="gray50"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  #how to set limits on the x axis or y axis (does not clip values outside lims)
  coord_cartesian(ylim=c(55,185)) +
  ylab('Middle of migration')+ 
  xlab('Year')+ 
  scale_y_continuous(expand = c(0, 0), breaks = c(60,91, 121, 152, 182), labels = c( "Mar","Apr", "May", "Jun", "Jul")) +
  labs(title = 'Chinook subyearlings')
middle1


middle2 <- ggplot(data = middle_chinook1_df, aes(x = year, y = middle_chinook1)) + geom_point()+ 
  theme(panel.background = element_rect(fill=NA, 
                                        color="gray50"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  #how to set limits on the x axis or y axis (does not clip values outside lims)
  coord_cartesian(ylim=c(55,185)) +
  ylab('Middle of migration')+ 
  xlab('Year')+ 
  scale_y_continuous(expand = c(0, 0), breaks = c(60,91, 121, 152, 182), labels = c( "Mar","Apr", "May", "Jun", "Jul")) +
  labs(title = 'Chinook yearlings')
middle2


middle3 <- ggplot(data = middle_coho1_df, aes(x = year, y = middle_coho1)) + geom_point()+ 
  theme(panel.background = element_rect(fill=NA, 
                                        color="gray50"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  #how to set limits on the x axis or y axis (does not clip values outside lims)
  coord_cartesian(ylim=c(110,160)) +
  ylab('Middle of migration')+ 
  xlab('Year')+ 
  scale_y_continuous(expand = c(0, 0), breaks = c(121, 152, 182), labels = c("May", "Jun", "Jul")) +
  labs(title = 'Coho')
middle3

middle4 <- ggplot(data = middle_steelhead_df, aes(x = year, y = middle_steelhead)) + geom_point()+ 
  theme(panel.background = element_rect(fill=NA, 
                                        color="gray50"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  #how to set limits on the x axis or y axis (does not clip values outside lims)
  coord_cartesian(ylim=c(110,160)) +
  ylab('Middle of migration')+ 
  xlab('Year')+ 
  scale_y_continuous(expand = c(0, 0), breaks = c(121, 152, 182), labels = c("May", "Jun", "Jul")) +
  labs(title = 'Steelhead')
middle4

middle_plot <- middle1 + middle2 + middle3 + middle4
middle_plot
ggsave(filename=here("Documents","output","pied_piper","middle_migration.png"), plot=middle_plot, 
       width=12, height=6, dpi=300)


#####################################################################
#end of migration
end1 <- ggplot(data = end_chinook0_df, aes(x = year, y = end_chinook0)) + geom_point()+ 
  theme(panel.background = element_rect(fill=NA, 
                                        color="gray50"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  #how to set limits on the x axis or y axis (does not clip values outside lims)
  coord_cartesian(ylim=c(152,250)) +
  ylab('End of migration')+ 
  xlab('Year')+ 
  scale_y_continuous(expand = c(0, 0), breaks = c(60,91, 121, 152, 182, 213, 244), labels = c( "Mar","Apr", "May", "Jun", "Jul", "Aug", "Sep")) +
  labs(title = 'Chinook subyearlings')
end1

end2 <- ggplot(data = end_chinook1_df, aes(x = year, y = end_chinook1)) + geom_point()+ 
  theme(panel.background = element_rect(fill=NA, 
                                        color="gray50"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  #how to set limits on the x axis or y axis (does not clip values outside lims)
  coord_cartesian(ylim=c(25,250)) +
  ylab('End of migration')+ 
  xlab('Year')+ 
  scale_y_continuous(expand = c(0, 0), breaks = c(60,91, 121, 152, 182, 213, 244), labels = c( "Mar","Apr", "May", "Jun", "Jul", "Aug", "Sep")) +
  labs(title = 'Chinook yearlings')
end2

end3 <- ggplot(data = end_coho1_df, aes(x = year, y = end_coho1)) + geom_point()+ 
  theme(panel.background = element_rect(fill=NA, 
                                        color="gray50"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  #how to set limits on the x axis or y axis (does not clip values outside lims)
  coord_cartesian(ylim=c(125,200)) +
  ylab('End of migration')+ 
  xlab('Year')+ 
  scale_y_continuous(expand = c(0, 0), breaks = c(60,91, 121, 152, 182, 213, 244), labels = c( "Mar","Apr", "May", "Jun", "Jul", "Aug", "Sep")) +
  labs(title = 'Coho')
end3


end4 <- ggplot(data = end_steelhead_df, aes(x = year, y = end_steelhead)) + geom_point()+ 
  theme(panel.background = element_rect(fill=NA, 
                                        color="gray50"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  #how to set limits on the x axis or y axis (does not clip values outside lims)
  coord_cartesian(ylim=c(125,200)) +
  ylab('End of migration')+ 
  xlab('Year')+ 
  scale_y_continuous(expand = c(0, 0), breaks = c(60,91, 121, 152, 182, 213, 244), labels = c( "Mar","Apr", "May", "Jun", "Jul", "Aug", "Sep")) +
  labs(title = 'Steelhead')
end4

end_plot <- end1 + end2 + end3 + end4 
end_plot

ggsave(filename=here("Documents","output","pied_piper","end_migration.png"), plot=end_plot, 
       width=12, height=6, dpi=300)

duration1 <- ggplot(data = end_chinook0_df, aes(x = year, y = duration)) + geom_point()+ 
  theme(panel.background = element_rect(fill=NA, 
                                        color="gray50"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  #how to set limits on the x axis or y axis (does not clip values outside lims)
  coord_cartesian(ylim=c(0,180)) +
  ylab('Duration')+ 
  xlab('Year')+ 
  #scale_y_continuous(expand = c(0, 0), breaks = c(60,91, 121, 152, 182, 213, 244), labels = c( "Mar","Apr", "May", "Jun", "Jul", "Aug", "Sep")) +
  labs(title = 'Chinook subyearlings')
duration1

duration2 <- ggplot(data = end_chinook1_df, aes(x = year, y = duration)) + geom_point()+ 
  theme(panel.background = element_rect(fill=NA, 
                                        color="gray50"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  #how to set limits on the x axis or y axis (does not clip values outside lims)
  coord_cartesian(ylim=c(0,180)) +
  ylab('Duration')+ 
  xlab('Year')+ 
  #scale_y_continuous(expand = c(0, 0), breaks = c(60,91, 121, 152, 182, 213, 244), labels = c( "Mar","Apr", "May", "Jun", "Jul", "Aug", "Sep")) +
  labs(title = 'Chinook yearlings')
duration2


duration3 <- ggplot(data = end_coho1_df, aes(x = year, y = duration)) + geom_point()+ 
  theme(panel.background = element_rect(fill=NA, 
                                        color="gray50"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  #how to set limits on the x axis or y axis (does not clip values outside lims)
  coord_cartesian(ylim=c(0,180)) +
  ylab('Duration')+ 
  xlab('Year')+ 
  #scale_y_continuous(expand = c(0, 0), breaks = c(60,91, 121, 152, 182, 213, 244), labels = c( "Mar","Apr", "May", "Jun", "Jul", "Aug", "Sep")) +
  labs(title = 'Coho')
duration3


duration4 <- ggplot(data = end_steelhead_df, aes(x = year, y = duration)) + geom_point()+ 
  theme(panel.background = element_rect(fill=NA, 
                                        color="gray50"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  #how to set limits on the x axis or y axis (does not clip values outside lims)
  coord_cartesian(ylim=c(0,180)) +
  ylab('Duration')+ 
  xlab('Year')+ 
  #scale_y_continuous(expand = c(0, 0), breaks = c(60,91, 121, 152, 182, 213, 244), labels = c( "Mar","Apr", "May", "Jun", "Jul", "Aug", "Sep")) +
  labs(title = 'Steelhead')
duration4

duration_plot <- duration1 + duration2 + duration3 + duration4
duration_plot

ggsave(filename=here("Documents","output","pied_piper","duration_migration.png"), plot=duration_plot, 
       width=12, height=6, dpi=300)


#################a
# atu
atu1 <- ggplot(data = start_chinook0_df, aes(x = atu_solstice, y = start_chinook0)) + geom_point()+ 
  theme(panel.background = element_rect(fill=NA, 
                                        color="gray50"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  #how to set limits on the x axis or y axis (does not clip values outside lims)
  coord_cartesian(ylim=c(35,100))+#,xlim=c(0,500)) +
  ylab('Start of Migration')+ 
  xlab('ATU')+ 
  scale_y_continuous(expand = c(0, 0), breaks = c(32,60,91, 121, 152, 182, 213, 244), labels = c( "Feb", "Mar","Apr", "May", "Jun", "Jul", "Aug", "Sep")) +
  labs(title = 'Chinook subyearlings')
atu1

atu2 <- ggplot(data = start_chinook1_df, aes(x = atu_solstice, y = start_chinook1)) + geom_point()+ 
  theme(panel.background = element_rect(fill=NA, 
                                        color="gray50"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  #how to set limits on the x axis or y axis (does not clip values outside lims)
  coord_cartesian(ylim=c(35,100)) + #,xlim=c(0,1000)) +
  ylab('Start of Migration')+ 
  xlab('ATU')+ 
  scale_y_continuous(expand = c(0, 0), breaks = c(32,60,91, 121, 152, 182, 213, 244), labels = c("Feb", "Mar","Apr", "May", "Jun", "Jul", "Aug", "Sep")) +
  labs(title = 'Chinook yearlings')
atu2

atu3 <- ggplot(data = start_coho1_df, aes(x = atu_april, y = start_coho1)) + geom_point()+ 
  theme(panel.background = element_rect(fill=NA, 
                                        color="gray50"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  #how to set limits on the x axis or y axis (does not clip values outside lims)
  coord_cartesian(ylim=c(91,140)) + #,xlim=c(0,1000)) +
  ylab('Start of Migration')+ 
  xlab('ATU')+ 
  scale_y_continuous(expand = c(0, 0), breaks = c(32,60,91, 121, 152, 182, 213, 244), labels = c("Feb", "Mar","Apr", "May", "Jun", "Jul", "Aug", "Sep")) +
  labs(title = 'Coho')
atu3

atu4 <- ggplot(data = start_steelhead_df, aes(x = atu_april, y = start_steelhead)) + geom_point()+ 
  theme(panel.background = element_rect(fill=NA, 
                                        color="gray50"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  #how to set limits on the x axis or y axis (does not clip values outside lims)
  coord_cartesian(ylim=c(91,140)) + #,xlim=c(0,1000)) +
  ylab('Start of Migration')+ 
  xlab('ATU')+ 
  scale_y_continuous(expand = c(0, 0), breaks = c(32,60,91, 121, 152, 182, 213, 244), labels = c("Feb", "Mar","Apr", "May", "Jun", "Jul", "Aug", "Sep")) +
  labs(title = 'Steelhead')
atu4

atu_mixed <- atu1 + atu2 + atu3 + atu4
atu_mixed

ggsave(filename=here("Documents","output","pied_piper","atu_start_migration.png"), plot=duration_plot, 
       width=12, height=6, dpi=300)


################################

#photoperiod

photo1 <- ggplot(data = start_chinook0_df, aes(x = photoperiod, y = start_chinook0)) + geom_point()+ 
  theme(panel.background = element_rect(fill=NA, 
                                        color="gray50"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  #how to set limits on the x axis or y axis (does not clip values outside lims)
  #coord_cartesian(ylim=c(35,100))+#,xlim=c(0,500)) +
  ylab('Start of Migration')+ 
  xlab('Photoperiod')+ 
  #scale_y_continuous(expand = c(0, 0), breaks = c(32,60,91, 121, 152, 182, 213, 244), labels = c( "Feb", "Mar","Apr", "May", "Jun", "Jul", "Aug", "Sep")) +
  labs(title = 'Chinook subyearlings')
photo1

photo2 <- ggplot(data = start_chinook1_df, aes(x = photoperiod, y = start_chinook1)) + geom_point()+ 
  theme(panel.background = element_rect(fill=NA, 
                                        color="gray50"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  #how to set limits on the x axis or y axis (does not clip values outside lims)
  #coord_cartesian(ylim=c(35,100))+#,xlim=c(0,500)) +
  ylab('Start of Migration')+ 
  xlab('Photoperiod')+ 
  #scale_y_continuous(expand = c(0, 0), breaks = c(32,60,91, 121, 152, 182, 213, 244), labels = c( "Feb", "Mar","Apr", "May", "Jun", "Jul", "Aug", "Sep")) +
  labs(title = 'Chinook yearlings')
photo2


photo3 <- ggplot(data = start_coho1_df, aes(x = photoperiod, y = start_coho1)) + geom_point()+ 
  theme(panel.background = element_rect(fill=NA, 
                                        color="gray50"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  #how to set limits on the x axis or y axis (does not clip values outside lims)
  #coord_cartesian(ylim=c(35,100))+#,xlim=c(0,500)) +
  ylab('Start of Migration')+ 
  xlab('Photoperiod')+ 
  #scale_y_continuous(expand = c(0, 0), breaks = c(32,60,91, 121, 152, 182, 213, 244), labels = c( "Feb", "Mar","Apr", "May", "Jun", "Jul", "Aug", "Sep")) +
  labs(title = 'Coho')
photo3

photo4 <- ggplot(data = start_steelhead_df, aes(x = photoperiod, y = start_steelhead)) + geom_point()+ 
  theme(panel.background = element_rect(fill=NA, 
                                        color="gray50"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  #how to set limits on the x axis or y axis (does not clip values outside lims)
  #coord_cartesian(ylim=c(35,100))+#,xlim=c(0,500)) +
  ylab('Start of Migration')+ 
  xlab('Photoperiod')+ 
  #scale_y_continuous(expand = c(0, 0), breaks = c(32,60,91, 121, 152, 182, 213, 244), labels = c( "Feb", "Mar","Apr", "May", "Jun", "Jul", "Aug", "Sep")) +
  labs(title = 'Steelhead')
photo4
#################################################################
for(i in 1:length(years)){
  data_year <- agg_data_w_ma_msd[agg_data_w_ma_msd$year == years[i],]
  
  data_year$cum_chinook1_wild <- cumsum(data_year$chinook1_wild_num)
  
  #end = append(end,which.min(abs(data_year$cum_chinook0_wild - max(data_year$cum_chinook0_wild)*0.9)))
  
  start <- which.min(abs(data_year$cum_chinook1_wild - max(data_year$cum_chinook1_wild)*0.1))
  
  #middle = append(middle,which.min(abs(data_year$cum_chinook0_wild - max(data_year$cum_chinook0_wild)*0.5)))
  
  start_chinook1_df[i,1] <- data_year$year[start]
  start_chinook1_df[i,2] <- data_year$doy[start]
  start_chinook1_df[i,3] <- data_year$temp[start]
  start_chinook1_df[i,4] <- data_year$atu[start]
  start_chinook1_df[i,5] <- data_year$flow[start]
}

start_chinook1_df <- as.data.frame(start_chinook1_df)
colnames(start_chinook1_df) <- c('year', 'start_chinook1', 'temp', 'atu', 'flow')

#coho
###############################

start_coho1_df <- matrix(ncol = 7, nrow = length(years))
for(i in 1:length(years)){
  data_year <- agg_data_w_ma_msd[agg_data_w_ma_msd$year == years[i],]
  
  data_year$cum_coho1_wild <- cumsum(data_year$coho1_wild_num)
  
  #end = append(end,which.min(abs(data_year$cum_chinook0_wild - max(data_year$cum_chinook0_wild)*0.9)))
  
  start <- which.min(abs(data_year$cum_coho1_wild - max(data_year$cum_coho1_wild)*0.1))
  
  #middle = append(middle,which.min(abs(data_year$cum_chinook0_wild - max(data_year$cum_chinook0_wild)*0.5)))
  
  start_coho1_df[i,1] <- data_year$year[start]
  start_coho1_df[i,2] <- data_year$doy[start]
  start_coho1_df[i,3] <- data_year$temp[start]
  start_coho1_df[i,4] <- data_year$atu[start]
  start_coho1_df[i,5] <- data_year$flow[start]
}

start_coho1_df <- as.data.frame(start_coho1_df)
colnames(start_coho1_df) <- c('year', 'start_coho1', 'temp', 'atu', 'flow')


#steelhead
###############################

start_steelhead_df <- matrix(ncol = 7, nrow = length(years))
for(i in 1:length(years)){
  data_year <- agg_data_w_ma_msd[agg_data_w_ma_msd$year == years[i],]
  
  data_year$cum_steelhead_wild <- cumsum(data_year$steelheadsmolt_wild_num)
  
  #end = append(end,which.min(abs(data_year$cum_chinook0_wild - max(data_year$cum_chinook0_wild)*0.9)))
  
  start <- which.min(abs(data_year$cum_steelhead_wild - max(data_year$cum_steelhead_wild)*0.1))
  
  #middle = append(middle,which.min(abs(data_year$cum_chinook0_wild - max(data_year$cum_chinook0_wild)*0.5)))
  
  start_steelhead_df[i,1] <- data_year$year[start]
  start_steelhead_df[i,2] <- data_year$doy[start]
  start_steelhead_df[i,3] <- data_year$temp[start]
  start_steelhead_df[i,4] <- data_year$atu[start]
  start_steelhead_df[i,5] <- data_year$flow[start]
}

start_steelhead_df <- as.data.frame(start_steelhead_df)
colnames(start_steelhead_df) <- c('year', 'start_steelhead', 'temp', 'atu', 'flow')
