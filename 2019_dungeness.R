

library(RODBC)
library(here)

channel <- odbcConnectAccess2007(here("Documents","data","pied_piper","2019 Dungeness.accdb"))


df <- sqlFetch(channel, 'qry_AllCatch')

df_steelhead <- df[df$WSPEName=="Steelhead smolt H" | df$WSPEName=="Steelhead smolt W",]

ggplot(data=df_steelhead, 
       aes(x=StartDate, y=NumCaught, color=  WSPEName, fill = WSPEName)) + 
  geom_col(alpha = 0.3) + 
  ylab("Number of fish caught") + xlab("Date") +
  labs(title = "Steelhead smolt 2019 - Hatchery and Wild")


df_chinook <- df[df$WSPEName=="Chinook 0+ W" | df$WSPEName=="Chinook 0+ H",]

ggplot(data=df_chinook, 
       aes(x=StartDate, y=NumCaught, color=  WSPEName, fill = WSPEName)) + 
  geom_col(alpha = 0.3) + 
  ylab("Number of fish caught") + xlab("Date") +
  labs(title = "Chinook 0+ 2019 Dungeness")

#coho

df_coho <- df[df$WSPEName=="Coho 1+ W" | df$WSPEName=="Coho 1+ H",]

ggplot(data=df_coho, 
       aes(x=StartDate, y=NumCaught, color=  WSPEName, fill = WSPEName)) + 
  geom_col(alpha = 0.3) + 
  ylab("Number of fish caught") + xlab("Date") +
  labs(title = "Coho 1+ 2019 Dungeness")

#start times

#coho
ggplot(data=df_coho, 
       aes(x=StartTime, y=NumCaught, color=  WSPEName, fill = WSPEName)) + 
  geom_col(alpha = 0.3) + 
  ylab("Number of fish caught") + xlab("Time") +
  labs(title = "Coho 1+ 2019 Dungeness")

#chinook
ggplot(data=df_chinook, 
       aes(x=StartTime, y=NumCaught, color=  WSPEName, fill = WSPEName)) + 
  geom_col(alpha = 0.3) + 
  ylab("Number of fish caught") + xlab("Time") +
  labs(title = "Chinook 0+ 2019 Dungeness")

#steelhead
ggplot(data=df_steelhead, 
       aes(x=StartTime, y=NumCaught, color=  WSPEName, fill = WSPEName)) + 
  geom_col(alpha = 0.3) + 
  ylab("Number of fish caught") + xlab("Time") +
  labs(title = "Steelhead 1+ 2019 Dungeness")