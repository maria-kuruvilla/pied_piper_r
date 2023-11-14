#Goal - copy code from green 2020 to read 2015 skagit data



library(RODBC)
library(here)
library(tidyverse)

#location of data

data_string = here("..","..","..","OneDrive","Documents","data","pied_piper","skagit")

#read in data
access_file <- function(file_name){
  out <- tryCatch(
    {
      # Just to highlight: if you want to use more than one 
      # R expression in the "try" part then you'll have to 
      # use curly brackets.
      # 'tryCatch()' will return the last evaluated expression 
      # in case the "try" part was completed successfully
      
      message("This is the 'try' part")
      
      channel <- odbcConnectAccess2007(here(data_string,
                                            file_name))
      df <- sqlFetch(channel, 'qry_AllCatch')
      # The return value of `readLines()` is the actual value 
      # that will be returned in case there is no condition 
      # (e.g. warning or error). 
      # You don't need to state the return value via `return()` as code 
      # in the "try" part is not wrapped inside a function (unlike that
      # for the condition handlers for warnings and error below)
    },
    error=function(cond) {
      message(paste("File does not exist"),file_name)
      message("Here's the original error message:")
      message(cond)
      # Choose a return value in case of error
      return(NA)
    },
    warning=function(cond) {
      message("Opening access file caused a warning:",file_name)
      message("Here's the original warning message:")
      message(cond)
      # Choose a return value in case of warning
      return(NULL)
    },
    finally={
      # NOTE:
      # Here goes everything that should be executed at the end,
      # regardless of success or error.
      # If you want more than one expression to be executed, then you 
      # need to wrap them in curly brackets ({...}); otherwise you could
      # just have written 'finally=<expression>' 
      #message(paste("File successfully opened:", url))
      message("Finished",file_name)
    }
  )    
  return(out)
  odbcClose(channel)
}

df_2015 <- access_file("2015 Skagit Mainstem.accdb")


#writing new function with tidyverse to read accdb file and convert it into wide format

#making lookup table for species, age, origin

lookup <- c("chinook0_wild_num" = "1", "chinook0_hatchery_num" = "2", 
            "chinook1_wild_num" = "3", "chinook1_hatchery_num" = "4",
            "chum0_wild_num" = "5", "coho0_wild_num" = "6",  
            "coho1_wild_num" = "7", "coho1_hatchery_num" = "8", 
            "steelheadsmolt_wild_num" = "13",
            "sockeye0_wild_num" = "10", "sockeye1_wild_num" = "11",
            "pink0_wild_num" = "9")

df_2015_wide <- df_2015 %>%
  filter(CaptureType == 1, TrapStatus == 1) %>%
  pivot_wider(names_from = SpeciesRunLifeOrigin_Id, values_from = NumCaught, values_fill = 0) %>% 
  rename(any_of(lookup)) %>% 
  group_by(StartDate, EndDate, StartTime, EndTime) %>%
  summarise(chinook0_wild_num = sum(chinook0_wild_num), 
            chinook0_hatchery_num = sum(chinook0_hatchery_num),
            chinook1_wild_num = sum(chinook1_wild_num),
            chinook1_hatchery_num = sum(chinook1_hatchery_num),
            chum0_wild_num = sum(chum0_wild_num),
            coho0_wild_num = sum(coho0_wild_num),
            coho1_wild_num = sum(coho1_wild_num),
            coho1_hatchery_num = sum(coho1_hatchery_num),
            steelheadsmolt_wild_num = sum(steelheadsmolt_wild_num),
            sockeye0_wild_num = sum(sockeye0_wild_num),
            sockeye1_wild_num = sum(sockeye1_wild_num),
            pink0_wild_num = sum(pink0_wild_num)) %>%
  mutate(start_datetime = as.POSIXct(paste(StartDate, 
                                           format(StartTime, format = "%H:%M:%S"))),
         end_datetime = as.POSIXct(paste(EndDate,
                                         format(EndTime, format = "%H:%M:%S"))),
         In = as.numeric(difftime(end_datetime,
                                  start_datetime, 
                                  units = "hours")),
         midpoint = as.POSIXct(start_datetime + 
                                 (end_datetime - start_datetime)/2))
         
file_name = "2015_skagit_mainstem_scoop_screw.csv"
write.csv(df_2015_wide,here("data","skagit",
                        file_name))

df_2015_wide_screw <- df_2015 %>%
  filter(CaptureType == 1, TrapStatus == 1, TrapSiteLocation == 1) %>%
  pivot_wider(names_from = SpeciesRunLifeOrigin_Id, values_from = NumCaught, values_fill = 0) %>% 
  rename(any_of(lookup)) %>% 
  group_by(StartDate, EndDate, StartTime, EndTime) %>%
  summarise(chinook0_wild_num = sum(chinook0_wild_num), 
            chinook0_hatchery_num = sum(chinook0_hatchery_num),
            chinook1_wild_num = sum(chinook1_wild_num),
            # chinook1_hatchery_num = sum(chinook1_hatchery_num),
            chum0_wild_num = sum(chum0_wild_num),
            coho0_wild_num = sum(coho0_wild_num),
            coho1_wild_num = sum(coho1_wild_num),
            coho1_hatchery_num = sum(coho1_hatchery_num),
            steelheadsmolt_wild_num = sum(steelheadsmolt_wild_num),
            sockeye0_wild_num = sum(sockeye0_wild_num),
            sockeye1_wild_num = sum(sockeye1_wild_num),
            pink0_wild_num = sum(pink0_wild_num)) %>%
  mutate(start_datetime = as.POSIXct(paste(StartDate, 
                                           format(StartTime, format = "%H:%M:%S"))),
         end_datetime = as.POSIXct(paste(EndDate,
                                         format(EndTime, format = "%H:%M:%S"))),
         In = as.numeric(difftime(end_datetime,
                                  start_datetime, 
                                  units = "hours")),
         midpoint = as.POSIXct(start_datetime + 
                                 (end_datetime - start_datetime)/2))

file_name = "2015_skagit_mainstem_screw.csv"
write.csv(df_2015_wide_screw,here("data","skagit",
                            file_name))



df_2015_wide_scoop <- df_2015 %>%
  filter(CaptureType == 1, TrapStatus == 1, TrapSiteLocation == 2) %>%
  pivot_wider(names_from = SpeciesRunLifeOrigin_Id, values_from = NumCaught, values_fill = 0) %>% 
  rename(any_of(lookup)) %>% 
  group_by(StartDate, EndDate, StartTime, EndTime) %>%
  summarise(chinook0_wild_num = sum(chinook0_wild_num), 
            chinook0_hatchery_num = sum(chinook0_hatchery_num),
            chinook1_wild_num = sum(chinook1_wild_num),
            chinook1_hatchery_num = sum(chinook1_hatchery_num),
            chum0_wild_num = sum(chum0_wild_num),
            coho0_wild_num = sum(coho0_wild_num),
            coho1_wild_num = sum(coho1_wild_num),
            coho1_hatchery_num = sum(coho1_hatchery_num),
            steelheadsmolt_wild_num = sum(steelheadsmolt_wild_num),
            sockeye0_wild_num = sum(sockeye0_wild_num),
            sockeye1_wild_num = sum(sockeye1_wild_num),
            pink0_wild_num = sum(pink0_wild_num)) %>%
  mutate(start_datetime = as.POSIXct(paste(StartDate, 
                                           format(StartTime, format = "%H:%M:%S"))),
         end_datetime = as.POSIXct(paste(EndDate,
                                         format(EndTime, format = "%H:%M:%S"))),
         In = as.numeric(difftime(end_datetime,
                                  start_datetime, 
                                  units = "hours")),
         midpoint = as.POSIXct(start_datetime + 
                                 (end_datetime - start_datetime)/2))

file_name = "2015_skagit_mainstem_scoop.csv"
write.csv(df_2015_wide_screw,here("data","skagit",
                                  file_name))


