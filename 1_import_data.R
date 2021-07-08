################################################################################
## 1- Importing data
#####
## Examples of different methods to import data in RStudio
################################################################################

################################################################################
## A - Example download file from the web - HDX Dataset 

library(utils)
 #download file and store in work space
download.file(
  url = "https://data.humdata.org/dataset/81ac1d38-f603-4a98-804d-325c658599a3/resource/99d3e0fe-b84d-4708-8539-08542f05d6ea/download/nga_adminboundaries_tabulardata.xlsx",
  destfile = "nigeria_admin.xlsx"
)

library(readxl)
nigeria_admin <- read_excel("nigeria_admin.xlsx", 
                            sheet = "Admin3")

view(nigeria_admin)


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

#Download data in CSV format

view(d_formlist_csv) # open project list and choose the id of the project we want the data from

# Select the project we want data from
form_id <- d_formlist_csv[25,2]# form ID of the data to download. Manually checked the id in view() 

url_o <- paste("https://kc.humanitarianresponse.info/api/v1/data/",form_id, ".csv", sep = "") # set connection url
d_raw_o <- kobohr_getdata_csv(url_o,kobo_user,kobo_pw)  # create object containing the dataset 

view(d_raw_o) 

xlsform_name<-"./xlsform/kob_xlsform_master.xlsx"
#create dictionary from the ODK/XLSFORM design form
kobo_dico(xlsform_name)
#saves file with the same name (suffix added) in the same folder



## Kobo UNHCR ######################## 

library(httr)
library(tidyverse)
library(jsonlite) # package for working with JSON data
#for reading and writing data
library(devtools)
source_url("https://raw.githubusercontent.com/ppsapkota/kobohr_apitoolbox/master/R/r_func_ps_kobo_utils.R")

# Download data in JSON format

# Set form ID from kobo 
form_id_hcr<-"amAXvCQ2QbikAXoEh7SSuB" #2020 DRS Livelihoods demo survey

url_hcr <- paste("https://kobo.unhcr.org/assets/",form_id_hcr, "/submissions/?format=json", sep = "") #Retrieve correct url from "https://kobo.unhcr.org/api/v2/assets/amAXvCQ2QbikAXoEh7SSuB/data/"
d_raw_hcr <- kobohr_getdata_csv(url_hcr,kobo_user_hcr,kobo_pw_hcr)

colnames(d_raw_hcr)


data <- as.data.frame(d_raw)

# check data
nrow(data)
colnames(data)
table(data$Country, data$`BENEFICIARY_INFO/Gender`)

### https://github.com/unhcr/koboloader
install.packages("remotes")

remotes::install_github("unhcr/koboloadeR", Ncpus=4) 

## Use UNHCR graphical template- https://unhcr-web.github.io/unhcRstyle/docs/
remotes::install_github('unhcr-web/unhcRstyle')





################################################################################
### C - Connection to ODK Central
## https://docs.getodk.org/central-submissions/#central-submissions-odata
################################################################################
 
################################################################################
## D - Connect to external databases: DBI package
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





