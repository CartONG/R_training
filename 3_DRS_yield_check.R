####################################################################################
# 3 - Explore Data --------------------------------------------------------
# 2019 Livelihoods DRS Data
####################################################################################
library(tidyverse)


# 1 - download data -------------------------------------------------------

# Dataset available in Github: CartONG/R_traning repo

# Store the link to the raw dataset hostes in github 
drs_df_raw_link <- "https://raw.githubusercontent.com/CartONG/R_training/main/df19.csv?token=ALMJASZ7ZQ23VFXS5VPZOEDA2MM42"

#download file and store in workspace
df19 <- read_csv(drs_df_raw_link)

summary(df19) # get a first glance at the file

# R "guesses" the column type by analyzing the 1st 1000 rows of each columns. In larger files, like the one we are working with here, this could lead to errros.

# download file again but forcing R to read all columns as character
df19 <- read_csv(drs_df_raw_link,
                 col_types = cols(.default = "c"))
summary(df19)

colnames(df19)

### 2 - Check quality off agricultural yield

# Identify the crop with most observations 
crop <- as.data.frame(table(df19$Crop1, df19$BE)) %>% 
  pivot_wider(names_from = Var2, values_from = Freq) %>% 
  arrange(desc(Baseline))
#Cassava

### Analyze Cassava 
yield1 <- df19 %>% 
  select(BE, Crop1, Country, Crop1KG, Crop1HA) %>%  # select only the columns of interest
  filter(BE == "Baseline" & Crop1 == "Cassava") %>% # filter rows by Baseline and crop (cassava)
  rename(Crop = Crop1, # rename column names
         CropKG = Crop1KG,
         CropHA = Crop1HA) 

yield1[4:5] <- sapply(yield1[4:5], as.numeric) # convert character columns that have numeric data to numeric

yield2 <- df19 %>% # repeat previous steps with the 2nd main crop
  select(BE, Crop2, Country, Crop2KG, Crop2HA) %>% 
  filter(BE == "Baseline" & Crop2 == "Cassava") %>% 
  rename(Crop = Crop2,
         CropKG = Crop2KG,
         CropHA = Crop2HA)

yield2[4:5] <- sapply(yield2[4:5], as.numeric)


yield <- bind_rows(yield1, yield2) # join 2 crop objects into one data frame

yield_all <- yield %>% 
  mutate(Yield = CropKG/CropHA, # crate new column calculating yield
         Check = Yield < 350) # set a threshold for yield under which we can assume the data entry is problematic

yield_all$Check <- as.factor(yield_all$Check)

# Plot cassava yield distribution in an histogram highlighting problematic entries
library(hrbrthemes)
yield_all_g <- ggplot(yield_all, aes(x=Yield))+
  geom_histogram(data = subset(yield_all, Check==TRUE), binwidth = 300, fill="red4")+
  geom_histogram(data = subset(yield_all, Check==FALSE), binwidth = 300, fill="#0072BC")+
  labs(title ="Cassava \nYield (Kg/Ha) Per Seasson Per Sampled Beneficiary",
       caption = "Source: UNHCR DRS 2019 livelihoods")+
  labs(x = "Cassava yield (To/Ha) per season",
       y = "# of sampled beneficairies",
       color = "legend")+
  scale_x_continuous(label=function(x){
    x <- x/1000
    return(paste(x, "To"))})+
  scale_color_manual(values = colors)+
  theme_ipsum()


yield_all_g <- ggplot(yield_all, aes(x=Yield, fill = Check))+
  geom_histogram(binwidth = 350)+
  labs(title ="Cassava \nYield (To/Ha) Per Seasson Per Sampled Beneficiary",
       caption = "Source: UNHCR DRS 2019 livelihoods \nHistogram bin width: 350")+
  labs(x = "Cassava yield (To/Ha) per season",
       y = "# of sampled beneficairies")+
  scale_x_continuous(label=function(x){
    x <- x/1000
    return(paste(x, "To"))})+
  theme_ipsum()+
  scale_fill_manual(values = c("#0072BC","red4"))


yield_all_g




### Create a function

# Identify crops by country
crop_check <- function(drs_df, base_end = c("Baseline", "Endilne"), Country_ = "All"){
  require(tidyverse)
  '
  1 - Filter by country & EL/BL
  
  2 - Extract vector of crops
    a - vector crop 1
    b - vector crop2
    c - join vectors
  3 - output unique values
  
  '
  base_end <- match.arg(base_end)
  
  if (Country_ == "All")
    temp <- drs_df %>% 
    filter(BE == base_end)
  
  else
    temp <- drs_df %>% 
    filter(BE == base_end & Country == Country_)
  
  c1 <- unique(temp$Crop1)
  
  c2 <- unique(temp$Crop2)
  
  c <- unique(c(c1, c2))
  c <- c[!is.na(c)]
  c <- c[c != 0]
  
  
  return(c)
  
}

# Graph problematic data entries
crop_yield <- function(df_drs, Crop, base_end = c("Baseline", "Endline"), Country = "All", Threshold = 100, Bin_width = 100){
  
  base_end <- match.arg(base_end)
  
  #crop 1
  if (Country == "All")
    t1 <- df_drs %>% 
      select(BE, Crop1, Country, Crop1KG, Crop1HA) %>% 
      filter(BE == base_end & Crop1 == Crop)
  
  else 
    t1 <- df_drs %>% 
      select(BE, Crop1, Country, Crop1KG, Crop1HA) %>% 
      filter(BE == base_end & Crop1 == Crop & Country == Country)
  
  t1 <- t1 %>% 
    rename(Crop = Crop1,
           CropKG = Crop1KG,
           CropHA = Crop1HA)
  
  
  #Crop2
  if (Country == "All")
    t2 <- df_drs %>% 
    select(BE, Crop2, Country, Crop2KG, Crop2HA) %>% 
    filter(BE == base_end & Crop2 == Crop)
  
  else 
    t2 <- df_drs %>% 
    select(BE, Crop2, Country, Crop2KG, Crop2HA) %>% 
    filter(BE == base_end & Crop2 == Crop & Country == Country)
  
  t2 <- t2 %>% 
    rename(Crop = Crop2,
           CropKG = Crop2KG,
           CropHA = Crop2HA)
  
  
  
  # combine crop lists
  t <- bind_rows(t1, t2)
  t$CropKG <- as.numeric(t$CropKG)
  t$CropHA <- as.numeric(t$CropHA)
  
  
  t <- t %>% 
    mutate(Yield = CropKG / CropHA,
           Check = Yield < Threshold)
  
  t_graph <- ggplot(t, aes(x=Yield)) +
    geom_histogram(data = subset(t, Check == TRUE), binwidth = Bin_width, fill = "red4")+
    geom_histogram(data = subset(yield_all, Check == FALSE), binwidth = Bin_width, fill = "#0072BC")+
    labs(title = paste(Crop, "Yield (Kg/Ha) Per Seasson Per Sampled Beneficiary"),
         caption = "Source: UNHCR DRS 2019 livelihoods")+
    xlab(paste(Crop,"yield (Kg/Ha) per season"))+
    ylab("# of sampled beneficairies")+
    scale_x_continuous(label=function(x){return(paste(x, "kg"))})
  
  return(t_graph)
  
  
}



temp_x <- crop_check(df19, "Baseline", "Burkina Faso")

temp_y <- crop_yield(df19, "Pumpkins, squash and gourds", "Baseline", "Burkina Faso", Threshold = 200 , Bin_width = 100)


for (c in 1: length(temp_x)){
  x <- temp_x[c]
  y <- crop_yield(df19, x, "Baseline", "Burkina Faso", Threshold = 200, Bin_width = 200)
  print(y)
  
}



### dataset with crops by country
y1 <- df19 %>% 
  select(BE, Crop1, Country, Crop1KG, Crop1HA) %>% 
  filter(BE == "Baseline" & !is.na(Crop1)) %>% 
  rename(Crop = Crop1,
         CropKG = Crop1KG,
         CropHA = Crop1HA)


y2 <- df19 %>% 
  select(BE, Crop2, Country, Crop2KG, Crop2HA) %>% 
  filter(BE == "Baseline" & !is.na(Crop2)) %>% 
  rename(Crop = Crop2,
         CropKG = Crop2KG,
         CropHA = Crop2HA)

y_ttl <- bind_rows(y1, y2)
y_ttl$CropKG <- as.numeric(y_ttl$CropKG)
y_ttl$CropHA <- as.numeric(y_ttl$CropHA)

y_ttl <- y_ttl %>% 
  mutate(Yield = CropKG/CropHA,
         Check = Yield < 300)

y_ttlG <- y_ttl %>% 
  filter(Country == "Burkina Faso" & Crop != "0")

y_ttlG <- ggplot(y_ttlG, aes(x=Yield))+
  geom_histogram(data = subset(y_ttlG, Check==TRUE), binwidth = 100, fill="red4")+
  geom_histogram(data = subset(y_ttlG, Check==FALSE), binwidth = 100, fill="#0072BC")+
  labs(title ="Yield (Kg/Ha) Per Seasson Per Sampled Beneficiary",
       caption = "Source: UNHCR DRS 2019 livelihoods")+
  xlab("yield (Kg/Ha) per season")+
  ylab("# of sampled beneficairies")+
  scale_x_continuous(label=function(x){return(paste(x, "kg"))})+
  facet_wrap(~Crop, 
             ncol = 2)



y_ttlG

## filter out problematic yield entries

fiterDR <- df19
# select columns (BE, Country, FamilySize, Crop1, Crop2, Crop1KG, O1IncomeFarming)

# Transform the following variables to numeric: FamilySize, Crop1KG, Crop1HA
# create variable Yield (Crop1KG / Crop1HA)
# create variable check with threshold of problematic yield values (eg: 150Kg) (play around with the threshold)

# how may row we have

# if enough:
# Compare yield change btw baseline and endline 

# compare yield change by Country (only country with enough data if any)


# SCATTER PLOT: x= yield, y=FamilySize, colored by: O1IncomeFarming

df19$Crop1KG <- as.numeric(df19$Crop1KG)
df19$Crop1HA <- as.numeric(df19$Crop1HA)
df19$FamilySize <- as.numeric(df19$FamilySize)



dfYield2 <- df19 %>%
  select(BE, Country, FamilySize, Crop1, Crop1HA, Crop1KG, O1IncomeFarming, ArrivalYear) %>% 
  mutate(Yield = round(Crop1KG / Crop1HA, 0),
         Check = Yield < 150) %>% 
  filter(Check==FALSE) %>% 
  filter (Crop1=="Cassava"|Crop1=="Maize"|Crop1=="Groundnuts, with shell") %>% 
  filter(Yield != Inf) %>% 
  group_by(BE,Crop1) %>% 
  summarise(avgYield = mean(Yield)) %>% 
  ungroup() %>% 
  arrange(Crop1)

dfYield2_graph <-dfYield2 %>% 
  ggplot(aes(x = BE, y = avgYield)) + 
  geom_bar(position="dodge", stat="identity")  +
  facet_wrap(~Crop1)

dfYield3 <- df19 %>%
  select(BE, Country, FamilySize, Crop1, Crop1HA, Crop1KG, O1IncomeFarming, ArrivalYear) %>% 
  mutate(Yield = round(Crop1KG / Crop1HA, 0),
         Check = Yield < 150) %>% 
  filter(Check==FALSE) %>% 
  filter (Crop1=="Cassava"|Crop1=="Maize"|Crop1=="Groundnuts, with shell") %>% 
  filter(Yield != Inf)

dfYield3_graph <- dfYield3 %>% 
  filter(Yield < 10000) %>% 
  ggplot(aes(x = FamilySize, y = Yield, color= Crop1))+
  geom_point(alpha = 0.4)+
  geom_jitter(width = 5, height = 5)+
  geom_rug(col="brown",alpha=0.1, size=1)

cor(dfYield3$FamilySize, dfYield3$Yield)

dfYield3_graph2 <- dfYield3 %>% 
  filter(Yield < 10000) %>% 
  ggplot(aes(x = FamilySize, y = Yield, color= O1IncomeFarming))+
  geom_point(alpha = 0.4)+
  geom_jitter(width = 5, height = 5)+
  geom_rug(col="brown",alpha=0.1, size=1)

dfYield3_graph3 <- dfYield3 %>% 
  filter(Yield < 10000 & Crop1 == "Cassava") %>% 
  ggplot(aes(x = FamilySize, y = Yield, color= Crop1))+
  geom_point(alpha = 0.4)+
  geom_jitter(width = 5, height = 5)+
  geom_rug(col="brown",alpha=0.1, size=1)

dfYield3_graph4 <- dfYield3 %>% 
  filter(Yield < 10000 & Crop1 == "Maize") %>% 
  ggplot(aes(x = FamilySize, y = Yield, color= Crop1))+
  geom_point(alpha = 0.4)+
  geom_jitter(width = 5, height = 5)+
  geom_rug(col="brown",alpha=0.1, size=1)


dfYield3_graph5 <- dfYield3 %>% 
  filter(Yield < 10000 & !is.na(ArrivalYear) & Crop1 == "Cassava") %>% 
  ggplot(aes(x = ArrivalYear, y = Yield, color= Crop1))+
  geom_point(alpha = 0.4)+
  geom_jitter(width = 2, height = 2)+
  theme(axis.text.x = element_text(angle = 45))

dfYield3_graph5_cor <- dfYield3 %>% 
  filter(Yield < 10000 & !is.na(ArrivalYear) & Crop1 == "Cassava")

dfYield3_graph5_cor$ArrivalYear <- as.numeric(dfYield3_graph5_cor$ArrivalYear)

cor(dfYield3_graph5_cor$ArrivalYear, dfYield3_graph5_cor$Yield)


lm1 <- lm(ArrivalYear ~ Yield, dfYield3_graph5_cor)
summary(lm1)
  