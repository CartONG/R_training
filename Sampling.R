############### SURVEY 

"Probability Sampling Methods"

# Random sampling

"The first class of sampling methods is known as probability sampling methods 
because every member in a population has an equal probability of being selected 
to be in the sample."

"A common way to do it is Randomly select members through the use of a 
random number generator or some means of random selection"
"Example:  if we want to select some staff, we can put the names of each staff 
members into a hat and randomly draw out names to get a sample"

"How to do that with R"

"Packages to use"

library(tidyverse)
library(fabricatr)

gapminder %>% sample_n(size = 10)


# Stratified random sample

"Split each cartong staff according to their pole of origin, age group or gender for example
If we want a sample of 30 staffs, we can for example take randomly 5 persons in each pole
In case we want an equal repartition of each pole in the sample"



"Packages to use"

library(dbplyr)

strat_sample <- df %>%
  group_by(pole) %>% # 6 poles au total à CartONG
  sample_n(size=5)

table(strat_sample$grade)


# Cluster sample

"A commonly used sampling method is cluster sampling, 
in which a population is split into clusters and all members of some clusters 
are chosen to be included in the sample"

"Suppose a company that gives city tours wants to survey its customers out of ten tours 
they give in a single day, they randomly select four tours and ask every 
customer to rate their experience on a scale of 1 to 10"

#randomly choose 4 tour groups out of the 10
clusters <- sample(unique(df$tour), size=4, replace=F)

#define sample as all members who belong to one of the 4 tour groups
cluster_sample <- df[df$tour %in% clusters, ]

#view how many customers came from each tour
table(cluster_sample$tour)

