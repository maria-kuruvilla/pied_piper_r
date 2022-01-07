#Goal - open access files
#Maria Kuruvilla
#16th Dec 2021

library(RODBC)
library(here)

con <- odbcConnectAccess2007(here("Documents","data","pied_piper","2020_Dungeness.accdb"))

sqlFetch(con, 'tbl_environmental')

odbcClose(con)