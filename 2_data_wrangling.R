################################################################################
## 2 - Data wrangling
################################################################################
## Example 1: Excel file downloaded from the web - Guatemala political division 

library(tidyverse)

view(guatemala_admin)
colnames(guatemala_admin)

gt_admin <- guatemala_admin %>% 
  select(-1, code = ...2, place = ...3) %>% 
  drop_na() %>% 
  filter(code != "Código")

view(gt_admin)
gt_dpto <- gt_admin[1:22,] %>% 
  select(dpto = place, 1)
gt_dpto$code <- as.integer(gt_dpto$code)

gt_muni <- gt_admin[23:nrow(gt_admin),] %>% 
  mutate(index = substr(code, 0,2)) %>% 
  select(-1, muni = place, code = index)
gt_muni$code <- as.integer(gt_muni$code)

gt_loc <- inner_join(gt_muni, gt_dpto, by = "code")

view(gt_loc)


# data cleanining
head(df)
tail(df)

