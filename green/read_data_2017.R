#Goal - to write the 2017 MS Access files for Green river into csv


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
      message("File successfully opened",file_name)
    }
  )    
  return(out)
  odbcClose(channel)
}

df_2017 <- access_file("2017 Green.accdb")



data_reformat <- function(species, age, origin, df){
  
  name = paste(species,age,origin)
  print(name)
  if(species == "Steelhead"){
    if(origin == "H"){
      data_filter = 25
    }
    else{
      data_filter = 7
    }
  }
  
  if(species == "Chinook"){
    if(origin == "H"){
      if(age == "0+"){
        data_filter = 20
      }
      else{
        data_filter = 22
      }
    }
    else{
      if(age == "0+"){
        data_filter = 4
      }
      else{
        data_filter = 21
      }
    }
  }
  
  if(species == "Coho"){
    if(origin == "H"){
      if(age == "0+"){
        data_filter = 200000
      }
      else{
        data_filter = 23
      }
    }
    else{
      if(age == "0+"){
        data_filter = 2
      }
      else{
        data_filter = 1
      }
    }
  }
  if(species == "Chum"){
    if(origin == "H"){
      data_filter = 200000
    }
    else{
      if(age == "1+"){
        data_filter = 200000
      }
      else{
        data_filter = 3
      }
    }
  }
  if(species == "Pink"){
    if(origin == "H"){
      data_filter = 200000
    }
    else{
      if(age == "1+"){
        data_filter = 200000
      }
      else{
        data_filter = 5
      }
    }
  }
  
  data_filter = paste(species,age,origin)
  file = paste0(river,file_type)
  string_year = paste(toString(year),file)
  print(string_year)
  df <- access_file(string_year)
  
  df_species <- df[((df$WSPEName == name)
                    & df$CaptureType==1 & (df$TrapStatus==1)),]
  
  df_species$start_datetime <- as.POSIXct(paste(df_species$StartDate, 
                                                format(df_species$StartTime, format = "%H:%M:%S")))
  
  df_species$start_time <- format(as.POSIXct(df_species$start_datetime), format = "%H:%M:%S")
  
  
  df_species$end_datetime <- as.POSIXct(paste(df_species$EndDate, 
                                              format(df_species$EndTime, format = "%H:%M:%S")))
  
  df_species$In <- as.numeric(difftime(df_species$end_datetime,
                                       df_species$start_datetime, 
                                       units = "hours"))
  df_species$fish_per_hour <- (df_species$NumCaught
                               /df_species$In)
  
  df_species$midpoint <- as.POSIXct(df_species$start_datetime + 
                                      (df_species$end_datetime - df_species$start_datetime)/2)
  
  # if(nrow(df_species)>0){
  #   df_species$WSPEName <- paste(species,age,origin)
  # }
  
  return(df_species)
  
}

data_rename_2017 <- function(year, species, age, origin, river, file_type){
  new_df_trial <- data_reformat(species, age, origin)
  
  #accounting for various mark types
  list_delete = c()
  count = 0
  if(length(new_df_trial$StartDate)>0){
    for(i in 1:(length(new_df_trial$StartDate)-1)){
      num_caught = new_df_trial$NumCaught[i]
      for(j in (i+1):length(new_df_trial$StartDate)){
        if((new_df_trial$start_datetime[i] == new_df_trial$start_datetime[j])){
          count = count + 1
          list_delete= c(list_delete,j)
          #adding all the numbers of fish caught 
          num_caught = num_caught + new_df_trial$NumCaught[j]
          
          
          new_df_trial$NumCaught[i] = num_caught
        }
      }
    }
  }
  
  #print(list_delete)
  if(length(list_delete)>0){
    df_chinook0_h <- new_df_trial[-list_delete,]
  }
  else{
    df_chinook0_h <- new_df_trial
  }
  
  new_df <- data.frame(num_caught = df_chinook0_h$NumCaught,
                       In = df_chinook0_h$In,
                       Down = df_chinook0_h$start_datetime,
                       Up = df_chinook0_h$end_datetime,
                       Date = df_chinook0_h$StartDate,
                       Up_date = df_chinook0_h$EndDate,
                       Down_time = df_chinook0_h$StartTime,
                       Up_time = df_chinook0_h$EndTime,
                       midpoint = df_chinook0_h$midpoint)
  if(origin=="H"){
    origin_rename = "hatchery_perhour"
    origin_rename_num = "hatchery_num"
  }
  if(origin=="W"){
    origin_rename = "wild_perhour"
    origin_rename_num = "wild_num"
  }
  if(species == "Steelhead"){
    names(new_df)[names(new_df)=="num_caught"] <- 
      paste0(tolower(species),age,"_",origin_rename_num)
    
  }
  else{
    names(new_df)[names(new_df)=="num_caught"] <- 
      paste0(tolower(species),substr(age,0,1),"_",origin_rename_num)
    
  }
  return(new_df)
}

write_csv_2017 <- function(year, river, file_type){
  all_species = c("Chinook", "Coho", "Steelhead", "Chum", "Pink")
  
  count = 0 
  for(species in all_species){
    
    if(species == "Steelhead"){
      ages = c("smolt")
    }
    else{
      ages = c("0+","1+")
    }
    
    for(age in ages){
      if(species == "Coho" & age =="0+"){
        all_origin = c("W")
      }
      else{
        all_origin = c("H","W")
      }
      
      for(origin in all_origin){
        print(species)
        print(age)
        print(origin)
        if(count == 0){
          new_df <- data_rename_2017(year, species, age, origin, river, file_type)
        }
        else{
          new_df1 <- data_rename_2017(year, species, age, origin, river, file_type)
          new_df = merge(x = new_df1, y = new_df, by = c("Down","In","Up","Date",
                                                         "Up_date","Down_time","Up_time",
                                                         "midpoint"),
                         all = TRUE) 
        }
        count = count + 1
        
      }
      
    }
  }
  df_merge <- new_df
  df_merge[is.na(df_merge)]<-0 
  file_name = paste0(year,"_",tolower(river),"_all_R_new.csv")
  print(file_name)
  write.csv(df_merge,here("data","green",
                          file_name))
  
  
}

year = "2017"
river = "Green"
file_type = ".accdb"

write_csv_2017(year, river, file_type)


data_reformat("Chinook", "0+", "W", df_2017)
