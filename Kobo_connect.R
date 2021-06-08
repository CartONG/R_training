################################################################################
## 1 - Download data from KoboToolBox
################################################################################
## Source: https://github.com/ppsapkota/kobohr_apitoolbox
################################################################################

library(httr)
library(tidyverse)
#Locate raw file in the github.
library(devtools)
source_url("https://raw.githubusercontent.com/ppsapkota/kobohr_apitoolbox/master/R/r_func_ps_kobo_utils.R")

#Set kobo credentials --> in separate script

#Download form/project list
url <-"https://kc.humanitarianresponse.info/api/v1/data.csv" #downloads list of all projects in the kobo account
d_formlist_csv <- kobohr_getforms_csv (url,kobo_user, kobo_pw)
d_formlist_csv <- as.data.frame(d_formlist_csv)

view(d_formlist_csv)

#Download data in CSV format

form_id <- d_formlist_csv[24,2]# form ID of the data to download. Manually checked the if in view()

url <- paste("https://kc.humanitarianresponse.info/api/v1/data/",form_id, ".csv", sep = "")
d_raw <- kobohr_getdata_csv(url,kobo_user,kobo_pw)  
data <- as.data.frame(d_raw)



