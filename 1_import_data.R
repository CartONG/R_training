################################################################################
## 1- Importing data
################################################################################
## A - Example download file from the web

library(utils)
 
#download file and store in workspace
download.file(
  url = "https://www.ine.gob.gt/sistema/uploads/2016/10/28/0NiM1ouoHaN67SRO2IzXZ5RNI7FeyHpn.xls",
  destfile = "guatemala_admin.xls"
)


library(readxl)
guatemala_admin <- read_excel("guatemala_admin.xls", 
                              sheet = "Departamentos y municipios")

View(guatemala_admin)


#download file and store in workspace
download.file(
  url = "https://www.downloadexcelfiles.com/sites/default/files/docs/list_of_municipalities_of_el_salvador-1736j.xlsx",
  destfile = "SLV_admin.xlsx"
)

#upload file into the environment as object
library(readxl)
SLV_admin <- read_excel("SLV_admin.xlsx")
View(SLV_admin)



################################################################################
## B - Download data from KoboToolBox - HTTT GET
## Source: https://github.com/ppsapkota/kobohr_apitoolbox
## Source: https://humanitarian-user-group.github.io/post/kobo_restapi/
################################################################################


## OCHA #####################

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

form_id <- d_formlist_csv[24,2]# form ID of the data to download. Manually checked the id in view()

url <- paste("https://kc.humanitarianresponse.info/api/v1/data/",form_id, ".csv", sep = "")
d_raw <- kobohr_getdata_csv(url,kobo_user,kobo_pw)  
data <- as.data.frame(d_raw)


## Kobo UNHCR ######################## 


library(httr)
library(tidyverse)
library(jsonlite) # package for working with JSON data
#for reading and writing data
library(devtools)
source_url("https://raw.githubusercontent.com/ppsapkota/kobohr_apitoolbox/master/R/r_func_ps_kobo_utils.R")

# Download data in JSON format

# Set form ID from kobo 
form_id<-"amAXvCQ2QbikAXoEh7SSuB" #2020 DRS Livelihoods demo survey

url <- paste("https://kobo.unhcr.org/assets/",form_id, "/submissions/?format=json", sep = "") #Retrieve correct url from "https://kobo.unhcr.org/api/v2/assets/amAXvCQ2QbikAXoEh7SSuB/data/"
d_raw <- kobohr_getdata_csv(url,kobo_user,kobo_pw)  
data <- as.data.frame(d_raw)

# check data
nrow(data)
colnames(data)
table(data$Country, data$`BENEFICIARY_INFO/Gender`)

#### Connection to ODK Central
## https://docs.getodk.org/central-submissions/#central-submissions-odata
## Show screenshot 

 
################################################################################
## C - Connect to external databases: DBI package
## Source: https://dbi.r-dbi.org/
################################################################################


library(DBI)
install.packages("RSQLite")

library(RSQLite)

# Create an ephemeral in-memory RSQLite database
con <- dbConnect(RSQLite::SQLite(), dbname = ":memory:")

dbListTables(con)

dbWriteTable(con, "mtcars", mtcars)
dbListTables(con)


dbListFields(con, "mtcars")

dbReadTable(con, "mtcars")

# You can fetch all results:
res <- dbSendQuery(con, "SELECT * FROM mtcars WHERE cyl = 4")
dbFetch(res)

dbClearResult(res)

# Or a chunk at a time
res <- dbSendQuery(con, "SELECT * FROM mtcars WHERE cyl = 4")
while(!dbHasCompleted(res)){
  chunk <- dbFetch(res, n = 5)
  print(nrow(chunk))
}
dbClearResult(res)

dbDisconnect(con)


######################################################
## D -Connect to HDX data
######################################################
## Source: https://humanitarian-user-group.github.io/post/rhdx_demo/



