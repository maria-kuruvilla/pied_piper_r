#Goal - copy code from 2021 to read 2022 data



library(RODBC)
library(here)
library(tidyverse)

#function to access file
access_file <- function(file_name){
  out <- tryCatch(
    {
      # Just to highlight: if you want to use more than one 
      # R expression in the "try" part then you'll have to 
      # use curly brackets.
      # 'tryCatch()' will return the last evaluated expression 
      # in case the "try" part was completed successfully
      
      message("This is the 'try' part")
      
      channel <- odbcConnectAccess2007(here("data_raw",
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

df_2022 <- access_file("2022 Green.accdb")
unique(df_2022$WSPEName)


data_reformat <- function(species, age, origin, year, river, file_type){
  
  name = paste(species,age,origin)
  print(name)
  file = paste0(river,file_type)
  string_year = paste(toString(year),file)
  print(string_year)
  df <- access_file(string_year)
  #removing rows that have stopper or block in comments, keeping rows with NA in comments
  df_filter <- df[!((str_detect(df$Comments, "stopper") | str_detect(df$Comments, "Stopper") |
                       str_detect(df$Comments, "block") | str_detect(df$Comments, "Block")) & 
                      !is.na(df$Comments)),]
  df_species <- df_filter[((df_filter$WSPEName == name)
                           & df_filter$CaptureType==1 & (df_filter$TrapStatus==1)),]
  
  df_species$start_datetime <- as.POSIXct(paste(df_species$StartDate, 
                                                format(df_species$StartTime, format = "%H:%M:%S")))
  
  df_species$start_time <- format(as.POSIXct(df_species$start_datetime), format = "%H:%M:%S")
  
  
  df_species$end_datetime <- as.POSIXct(paste(df_species$EndDate, 
                                              format(df_species$EndTime, format = "%H:%M:%S")))
  
  df_species$In <- as.numeric(difftime(df_species$end_datetime,
                                       df_species$start_datetime, 
                                       units = "hours"))
  
  df_species$midpoint <- as.POSIXct(df_species$start_datetime + 
                                      (df_species$end_datetime - df_species$start_datetime)/2)
  
  # if(nrow(df_species)>0){
  #   df_species$WSPEName <- paste(species,age,origin)
  # }
  
  return(df_species)
  
}

trial <- data_reformat("Chinook", "1+", "H", 2022, "Green", ".accdb")




write_csv <- function(year, river, file_type){
  all_species = c("Chinook", "Coho", "Steelhead", "Chum", "Pink")
  all_species_lower = tolower(all_species)
  
  count = 0 
  for(species in all_species){
    
    if(species == "Steelhead"){
      ages = c("smolt")
      ages_edited = c("smolt")
    }
    else{
      ages = c("0+","1+")
      ages_edited = c("0","1")
    }
    
    for(age in ages){
      all_origin = c("H","W", "M")
      all_origin_edited = c("hatchery","wild", "mixed")
      
      
      for(origin in all_origin){
        print(species)
        print(age)
        print(origin)
        if(count == 0){
          new_df <- data_reformat(species, age, origin, year, river, file_type)
          new_df$WSPEName <- gsub("+", "", new_df$WSPEName, fixed = TRUE)
          new_df$WSPEName <- gsub(" ", "_", new_df$WSPEName)
          new_df$WSPEName <- tolower(new_df$WSPEName)
          new_df_agg <- new_df %>% 
            group_by(start_datetime, end_datetime, WSPEName, In, midpoint, StartDate, EndDate) %>% 
            summarise(num_caught = sum(NumCaught)) %>% 
            pivot_wider(names_from = WSPEName, values_from = num_caught)
        }
        else{
          
          new_df1 <- data_reformat(species, age, origin, year, river, file_type)
          new_df1$WSPEName <- gsub("+", "", new_df1$WSPEName, fixed = TRUE)
          new_df1$WSPEName <- gsub(" ", "_", new_df1$WSPEName)
          new_df1$WSPEName <- tolower(new_df1$WSPEName)
          new_df1_agg <- new_df1 %>% 
            group_by(start_datetime, end_datetime, WSPEName, In, midpoint, StartDate, EndDate) %>% 
            summarise(num_caught = sum(NumCaught)) %>% 
            pivot_wider(names_from = WSPEName, values_from = num_caught)
          new_df_agg = merge(x = new_df1_agg, y = new_df_agg, by = c("start_datetime", "end_datetime",
                                                                     "In", "midpoint", 
                                                                     "StartDate", "EndDate"),
                             all = TRUE)
          
        }
        count = count + 1
        
      }
      
    }
  }
  df_merge <- new_df_agg
  df_merge[is.na(df_merge)]<-0 
  file_name = paste0(year,"_",tolower(river),"_all_R_new.csv")
  print(file_name)
  #rename columns
  colnames(df_merge) <- c("start_datetime", "end_datetime", "In", "midpoint", "StartDate", "EndDate",
                          "pink0_wild_num","chum0_mixed_num", "steelheadsmolt_wild_num",
                          "steelheadsmolt_hatchery_num","coho1_mixed_num",
                          "coho1_hatchery_num","coho0_wild_num",
                          "chinook1_wild_num", "chinook1_hatchery_num",  "chinook0_wild_num",
                          "chinook0_hatchery_num")

  write.csv(df_merge,here("data","green",
                          file_name))
  
  return(df_merge)
  
}
trial <- write_csv(2022, "Green", ".accdb")
