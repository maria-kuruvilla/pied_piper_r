#Goal - open access files
#Maria Kuruvilla
#16th Dec 2021

library(RODBC)
library(here)

con <- odbcConnectAccess2007(here("Documents","data","pied_piper","2020_Dungeness.accdb"))

sqlFetch(con, 'tbl_environmental')

odbcClose(con)

#check if sqlFetch works for queries

con <- odbcConnectAccess2007(here("Documents","data","pied_piper","2020_Dungeness.accdb"))

#sqlFetch(con, 'qry_AllCatch') #works but is meant for tables


#working with table
channel <- odbcConnectAccess2007(here("Documents","data","pied_piper","2020_Dungeness.accdb"))
df <- sqlQuery(channel,
               "SELECT [Detail_Id],[SpeciesRunLifeOrigin_Id], [NumCaught]
                FROM [tbl_Catch]
                ORDER BY [Detail_Id];",
               stringsAsFactors = FALSE, as.is=c(TRUE,TRUE,TRUE)) #species are converted to integers

sqlGetResults(channel, as.is = FALSE,  errors = FALSE,
              max = 0, buffsize = 1000,
              nullstring = NA_character_, na.strings = "NA",
              believeNRows = TRUE, dec = getOption("dec"),
              stringsAsFactors = FALSE)

odbcClose(channel)



df <- sqlQuery(channel,
               "SELECT [SpeciesRunLifeOrigin_Id]
                FROM [tbl_Catch]
                ORDER BY [SpeciesRunLifeOrigin_Id];",
               stringsAsFactors = FALSE, as.is= TRUE) #species are converted to integers


df <- sqlFetch(channel, 'qry_AllCatch') #this works!

library(tidyverse)  #includes packages readr and ggplot2
library(hexbin)

ggplot(data=df, aes(x=WSPEName, y=NumCaught)) + 
  geom_col() + coord_flip() 

ggplot(data=df, aes(x=StartDate, y=NumCaught)) + 
  geom_col() + coord_flip() 


df_pink <- df[WSPEName=="Pink 0+ W",]
ggplot(data=df[df$WSPEName=="Pink 0+ W",], aes(x=StartDate[WSPEName=="Pink 0+ W"], y=NumCaught[WSPEName=="Pink 0+ W"])) + 
  geom_col() + coord_flip() 


unique(df$WSPEName) # different species

# plot for wild steelhead and hatchery steelhead

#wild
ggplot(data=df[df$WSPEName=="Steelhead smolt W",], 
       aes(x=StartDate[WSPEName=="Steelhead smolt W"], y=NumCaught[WSPEName=="Steelhead smolt W"])) + 
  geom_col() + coord_flip() +
  ylab("Number of fish caught") + xlab("Date") +
  labs(title = "Steelhead smolt Wild")


#hatchery
ggplot(data=df[df$WSPEName=="Steelhead smolt H",], 
       aes(x=StartDate[WSPEName=="Steelhead smolt H"], y=NumCaught[WSPEName=="Steelhead smolt H"])) + 
  geom_col(color = "black", fill = "black") + coord_flip() +
  ylab("Number of fish caught") + xlab("Date") +
  labs(title = "Steelhead smolt Hatchery")


#both

ggplot(NULL)+
  geom_col(aes(x=df$StartDate[df$WSPEName=="Steelhead smolt H"], y=df$NumCaught[df$WSPEName=="Steelhead smolt H"]),color = "gray", fill = "gray") + coord_flip() +
  ylab("Number of fish caught") + xlab("Date") +
  labs(title = "Steelhead smolt Hatchery") +
  geom_col(aes(x=df$StartDate[df$WSPEName=="Steelhead smolt W"], y=df$NumCaught[df$WSPEName=="Steelhead smolt W"]),color = "blue", fill = "blue", alpha = 0.5) + coord_flip()
  
  
  #not working great



#trying to make different dataset

df_steelhead <- df[df$WSPEName=="Steelhead smolt H" | df$WSPEName=="Steelhead smolt W",]

ggplot(data=df_steelhead, 
       aes(x=StartDate, y=NumCaught, color=  WSPEName, fill = WSPEName)) + 
  geom_col(alpha = 0.3) + 
  ylab("Number of fish caught") + xlab("Date") +
  labs(title = "Steelhead smolt - Hatchery and Wild")

#chinook

df_chinook <- df[df$WSPEName=="Chinook 0+ W" | df$WSPEName=="Chinook 0+ H",]

ggplot(data=df_chinook, 
       aes(x=StartDate, y=NumCaught, color=  WSPEName, fill = WSPEName)) + 
  geom_col(alpha = 0.3) + 
  ylab("Number of fish caught") + xlab("Date") +
  labs(title = "Chinook 0+")

#coho

df_coho <- df[df$WSPEName=="Coho 1+ W" | df$WSPEName=="Coho 1+ H",]

ggplot(data=df_coho, 
       aes(x=StartDate, y=NumCaught, color=  WSPEName, fill = WSPEName)) + 
  geom_col(alpha = 0.3) + 
  ylab("Number of fish caught") + xlab("Date") +
  labs(title = "Coho 1+")


#looking at start times
#coho
ggplot(data=df_coho, 
       aes(x=StartTime, y=NumCaught, color=  WSPEName, fill = WSPEName)) + 
  geom_col(alpha = 0.3) + 
  ylab("Number of fish caught") + xlab("Time") +
  labs(title = "Coho 1+")

#chinook
ggplot(data=df_chinook, 
       aes(x=StartTime, y=NumCaught, color=  WSPEName, fill = WSPEName)) + 
  geom_col(alpha = 0.3) + 
  ylab("Number of fish caught") + xlab("Time") +
  labs(title = "Chinook 0+")

#steelhead
ggplot(data=df_steelhead, 
       aes(x=StartTime, y=NumCaught, color=  WSPEName, fill = WSPEName)) + 
  geom_col(alpha = 0.3) + 
  ylab("Number of fish caught") + xlab("Time") +
  labs(title = "Steelhead 1+")
