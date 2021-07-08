##########################################################################################
##  2 Data wrangling (& Transformation) ---------------------------------------------------
## Using the dplyr package to modify Excel documents into XLS Form compatible documents
##########################################################################################
## Author: Abel Gelman
##########################################################################################

# A - Extract choices in XLSForm-compatible format:
# The partner shared a PDF with the questionnaire they wanted us to code
# A 1st step, not covered here, was to trasnform the PDF in Excel format
# The resulting Excel document had the questionnaire spread in 86 tabs. This is the document we import into R


# Load Packages -----------------------------------------------------------
library(dplyr)
library(readxl)
library(tidyverse)


# Set path to Excel file --------------------------------------------------
flocert_path <- "/Users/Abel/Documents/CartONG/R_webinar/R_training/flocert_1.xlsx"


# Import the 1st sheet of the Excel file (out of the 81)----------------------------------
x <- read_xlsx(flocert_path, 
               sheet = "Table 1",
               skip = 4, # skip 1st 4 rows of the xlsx
               col_types = "text", # force all variables to "character"
               col_names = TRUE) # 1st row as column names

view(x) # Visualize how the document was imported


# Import the rest of the Excel document sheets & compile them in on single dataframe --------
for (i in 2:81){ # The Excel file has 81 sheets. Iterate from the second sheet to the last (note the 1st sheet is alredy imported )
  tempo <- read_xlsx(flocert_path, 
                    sheet = paste("Table", i), # changes the sheet number to import in every iteration
                    col_types = "text",  # force all variables to "character"
                    col_names = TRUE)
  x <- bind_rows(x, tempo)       # add each page at the bottom of the previous one, making one only single work sheet
  
}

view(x) # Check how the document looks
colnames(x) # check column names


# Extract answers for the xlsform options tab
y <- x %>% 
  select(c(3, 5,6,7,8,9))  # selects only the useful columns: question number +
 
view(y)
colnames(y)

y <- y %>% 
  filter(`CC£No.` != "CC£No.", !is.na(`Rank£1`)) %>% #filter out rows that are not needed
  pivot_longer(-`CC£No.`) %>% # pivot the table to have 1 row per answer (choice in xls forms)
  filter(!is.na(value)) %>% # drop rows that don't have any answers
  # change into new columns that are XLSform compatible 
  transmute(list_name = paste("s", gsub(".", "_" ,`CC£No.`, fixed = T), sep = ""), # create list names with the questions numbers
            name = substr(name, nchar(name), nchar(name)), # only keep the rank numbers, not the word rank
            label = value) # change the name of the column label

view(y)
# export as csv - document is ready to copy-paste into the xlsform 
write.csv(y, "/Users/Abel/Documents/CartONG/R_webinar/R_training/flocert_2.csv")


##########################################################################################
## Complex XLS Form formulas 
# extract criteria column -------------------------------------------------
# Within this project, the "criteria" groups questions across sections of the form
# We need to calculate the average rank (response values) for questions of criteria "D"
 
# Identify questions names and their criteria letter
#  Starting from the original dataframe with the questions and answers, format the data:
yy <- x %>% 
  select(c(3,11)) %>% # Keep only the columns with the question number and criteria type
  filter(`CC£No.` != "CC£No.") %>% # filter out rows with no values
  rename(criteria_type = `Criteria£Type`,# rename columns 
         list_name = `CC£No.`) %>% 
  filter(!is.na(criteria_type )) %>% #filter out rows without criteria type
  transmute(list_name = paste("s", gsub(".", "_", list_name, fixed = T), sep = ""),# change format of question number to match XLS Form
            criteria_type = criteria_type)

write.csv(yy, "flocert_criteria.csv") #export output ready to copy paste into XLS Form

# make formula to calculate results averages for questions of criteria "D"

# create columns with XLSForm formulas for the numerator and denominator of the average formula
zz <- yy %>%
  mutate(denominator = paste("if(${", list_name, "}!='',1,0)", sep = ''), # write XLS compatible formula 
         numerator = paste("if(${", list_name, "}!='', ${", list_name, "},0)", sep = ''),
         section = substr(list_name, 2,2))


view(zz)

# Global development average 

d_glb_denominator <- NULL #Creates an empty vector paste the elements of the denominator elements of the formula

for(l in 1:nrow(zz)){ # Loop over zz
  if (zz[l,2] == 'D') # selects only the lines that have criteria "D"
  {d_glb_denominator <- paste(d_glb_denominator, zz[l,3],' + ',sep = '') # adds to the vector each corresponfing formula
  
  }
}

d_glb_denominator

d_glb_numerator <- NULL

for(l in 1:nrow(zz)){
  if (zz[l,2] == 'D') 
  {d_glb_numerator <- paste(d_glb_numerator, zz[l,4],' + ',sep = '')
  
  }
}

d_glb_numerator

#section 3

d_3_denominator <- NULL

for(l in 1:nrow(zz)){
  if (zz[l,2] == 'D' & zz[l, 5] == "3") 
  {d_3_denominator <- paste(d_3_denominator, zz[l,3],' + ',sep = '')
  }
}

d_3_numerator <- NULL

for(l in 1:nrow(zz)){
  if (zz[l,2] == 'D' & zz[l, 5] == 3) 
  {d_3_numerator <- paste(d_3_numerator, zz[l,4],' + ',sep = '')
  
  }
}

#section 4

d_4_denominator <- NULL

for(l in 1:nrow(zz)){
  if (zz[l,2] == 'D' & zz[l, 5] == "3") 
  {d_4_denominator <- paste(d_4_denominator, zz[l,3],' + ',sep = '')
  }
}

d_4_numerator <- NULL

for(l in 1:nrow(zz)){
  if (zz[l,2] == 'D' & zz[l, 5] == 3) 
  {d_4_numerator <- paste(d_4_numerator, zz[l,4],' + ',sep = '')
  
  }
}






