#############################################################################
#### Data visualization with ggplot2
## Author: Abel Gelman
#############################################################################
library(tidyverse)
#Visualiztion of family size per region

# Create data frame with countries and the UNHCR region the are part of
Region <- c('Asia','Southern Africa','Latin America','Asia','Latin America','West and Central Africa','West and Central Africa','West and Central Africa','West and Central Africa','Latin America','Southern Africa','Southern Africa','Latin America','West and Central Africa','West and Central Africa','Latin America','East and Horn of Africa','West and Central Africa','West and Central Africa','Asia','MENA','East and Horn of Africa','West and Central Africa','Southern Africa','MENA','Latin America','MENA','Southern Africa','Southern Africa','West and Central Africa','West and Central Africa','Latin America','Asia','East and Horn of Africa','West and Central Africa','East and Horn of Africa','Southern Africa','East and Horn of Africa','MENA','Southern Africa','MENA','East and Horn of Africa','MENA','East and Horn of Africa','Europe','Southern Africa','Southern Africa')
COUNTRY <- c('Afghanistan','Angola','Argentina','Bangladesh','Brazil','Burkina Faso','Cameroon','Central African Republic','Chad','Chile','Congo','The Democratic Republic of the Congo','Costa Rica','Cote dIvoire','Djibouti','Ecuador','Ethiopia','Ghana','Guinea','India','Jordan','Kenya','Liberia','Malawi','Mauritania','Mexico','Morocco','Mozambique','Namibia','Niger','Nigeria','Panama','Philippines','Rwanda','Senegal','Somalia','South Africa','South Sudan','Sudan','Swaziland','Syria','United Republic of Tanzania','Tunisia','Uganda','Ukraine','Zambia','Zimbabwe')
drs_regions <- data.frame(Region, COUNTRY,
                             row.names = NULL)

# merge with DRS dataset
df19r <- left_join(df19, drs_regions, by = c("Country" = "COUNTRY"))

# convert analysis variables to numeric
df19r$FamilySize <- as.numeric(df19$FamilySize)
df19r$Dependency <- as.numeric(df19$Dependency)

# calculate average family size per region
fmly_size <- df19r %>% #select dataset
  select(BE, Country,FamilySize, Region) %>% # select columns of interest
  filter(!is.na(FamilySize), BE == "Baseline") %>% # Filter out rows with no family size data and Keep only baseline observations
  group_by(Region) %>% # summarize results by regions
  summarise(Avg = mean(FamilySize), # create column with mean family size per region
            Max = max(FamilySize)) %>% # create colum  with max number of family size per region
  ungroup() # ungroup results

fmly_size

# graph boxplot of family size distribution by region
fmly_sizeG <- df19r %>% #select dataset
  ggplot(aes(x = Region, y = FamilySize, fill = Region))+ #map variables
  geom_boxplot() + #choose geom boxplot 
  coord_flip() + # (optional) flip coordianates. 
  theme(legend.position = "none")+ # (optional) define tables and lables 
  labs(title = "Sampled Family Size Per Region",
       caption = "Source: UNHCR DRS Livelihoods 2019",
       y = "Family size")

fmly_sizeG


# represent same data with density ridges
library(ggridges)

fmly_sizeG2 <- df19r %>% 
  filter(FamilySize < 20) %>% # filtering out families with over 20 members 
  ggplot(aes(x = FamilySize, y = Region, fill = Region))+
  geom_density_ridges2() +
  theme_ridges()+
  theme(legend.position = "none")+
  labs(title = "Sampled Family Size Per Region",
       subtitle = "Extreme values removed",
       caption = "Source: UNHCR DRS Livelihoods 2019",
       y = "")

fmly_sizeG2

# plot Dependancy ratio by region
dep <- df19r %>% 
  select(BE, Country,Dependency, Region) %>% # select columns of interest
  filter(!is.na(Dependency), BE == "Baseline") %>% # Filter out rows with no Dependancy data and Keep only baseline observations
  group_by(Region) %>% # summarize results by regions
  summarise(Min = min(Dependency), 
            Avg = mean(Dependency), # create column with mean family size per region
            Max = max(Dependency)) %>% # create column  with max number of family size per region
  ungroup() # ungroup results

dep

depG <- df19r %>% #select dataset
  filter(Dependency < 99) %>% 
  ggplot(aes(x = Region, y = Dependency, fill = Region))+ #map variables
  geom_boxplot() + #choose geom boxplot 
  coord_flip() + # (optional) flip coordianates. 
  theme(legend.position = "none")+ # (optional) define tables and lables 
  labs(title = "Dependancy Ratio Per Region",
       caption = "Source: UNHCR DRS Livelihoods 2019",
       y = "Dependancy ration (Active household members / Household size")
depG

# Same data but with density lines
df19r %>% 
  filter(Dependency < 99) %>% 
  ggplot(aes(x = Dependency, colour = Region))+
  geom_density()

# Same data but with ridges
df19r %>% 
  filter(Dependency < 7.5) %>% # filtering out families with over 20 members 
  ggplot(aes(x = Dependency, y = Region, fill = Region))+
  geom_density_ridges2() +
  theme_ridges()+
  theme(legend.position = "none")+
  labs(title = "Dependency Ratio Per Region",
       subtitle = "Extreme values removed (< 7.5)",
       caption = "Source: UNHCR DRS Livelihoods 2019",
       y = "")



# Compare family size vs Dependency by region with scaled scatter plots
df19r %>% 
  filter(Dependency < 15 & FamilySize < 30) %>% 
  ggplot(aes(x = FamilySize, y = Dependency, colour = Country))+
  geom_count()+
  scale_size_area(max_size = 5)+
  facet_wrap(~Region)



#####################################################3
### ANIMATED GIFs
# Load packages
library(ggplot2)
library(gganimate) # to create animated graphs
library(png)    
library(gifski) # to export animated graphs as GIFs


# Recator varialble with ordered labels: Decreased < Same < Increased
df19$O1IncomeFarming <- factor(df19$O1IncomeFarming, ordered = TRUE,
                               levels = c("Decreased", "Same", "Increased"))
df19$BE <- factor(df19$BE, ordered = TRUE,
                               levels = c("Baseline", "Endline"))


# set the color palette as a vector for easy recycling 
colors_order  <-  c("orangered4", "deepskyblue3", "chartreuse4", "orangered4", "deepskyblue3", "chartreuse4")

overwrite=T # setting overwrite to TRUE, overrights outouts, which is helpfull when testing 

# draw barchart for income change in farming
t_trans <- df19 %>%  # take the original data set
  filter(!is.na(O1IncomeFarming)) %>% # filter out NA values from the income information variable
  group_by(BE, O1IncomeFarming) %>% # Count unique observations by BE and income farming
  tally() %>% 
  mutate(Freq = sum(n),
         Perc = round((n/Freq)*100 , 0)) %>% 
  ungroup() %>% 
  ggplot(aes(x = O1IncomeFarming, y = Perc)) + # plot
  geom_bar(stat = "identity", fill= colors_order) + 
  theme_classic() +
  # Define animation paramerters
  labs(title = 'Farmig Income Change As Compared to Last Season: {closest_state}',
       x = 'How does your income from last season compares to the previous one?',
       y = '% of Sample',
       caption = "Source: UNHCR DRS 2019 livelihoods") +
  transition_states(BE,  
                    transition_length = 1,
                    state_length = 1) +
  ease_aes('sine-in-out')

animate(t_trans, duration = 5, fps = 20, width = 500, height = 500, renderer = gifski_renderer())
anim_save("Agri_change.gif") 


#### compare agri income change vs slef employment
# draw barchart for income chage in farming
t_comp1 <- df19 %>%  # take the origial data set
  filter(!is.na(O1IncomeFarming)) %>% # filter out values from the 
  group_by(BE, O1IncomeFarming) %>% # prepare data for graphic
  tally() %>% 
  mutate(Freq = sum(n),
         Perc = round((n/Freq)*100 , 0),
         Sector = "Agriculture") %>% 
  rename(Income = O1IncomeFarming) %>% 
  ungroup()

t_comp2 <- df19 %>%  # take the origial data set
  filter(!is.na(O2IncomeYear) & !is.na(BE) ) %>%# filter out values from the 
  group_by(BE, O2IncomeYear ) %>% # prepare data for graphic
  tally() %>% 
  mutate(Freq = sum(n),
         Perc = round((n/Freq)*100 , 0),
         Sector = "Self-employment") %>%
  rename(Income = O2IncomeYear) %>% 
  ungroup() 

colors_order2  <-  c("orangered4", "deepskyblue3", "chartreuse4",
                    "orangered4", "deepskyblue3", "chartreuse4",
                    "orangered4", "deepskyblue3", "chartreuse4",
                    "orangered4", "deepskyblue3", "chartreuse4")


t_comp <- bind_rows(t_comp1, t_comp2) %>% 
  ggplot(aes(x = Income, y = Perc)) + # plot
  geom_bar(stat = "identity", fill= colors_order2) + 
  #theme_classic() +
  facet_wrap(~Sector)+
  # Define animation paramerters
  labs(title = 'Income Change compared to last year: {closest_state}',
       x = 'How does your income from this year compares to last year?',
       y = '% of Sample',
       caption = "Source: UNHCR DRS 2019 livelihoods") +
  transition_states(BE,  
                    transition_length = 1,
                    state_length = 1) +
  ease_aes('sine-in-out')

animate(t_comp, duration = 5, fps = 20, width = 500, height = 500, renderer = gifski_renderer())
anim_save("comp_sector.gif") 











