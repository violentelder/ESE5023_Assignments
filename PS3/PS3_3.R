#========Vegetarians and Zinc=======
library(tidyr)
library(dplyr)
setwd('D:/ESE5023/ESE5023_Assignments/Data')

DATA <- read.csv("Zn_woman.csv",header = T)
DATA <- as_tibble(DATA)

nonvegetarians <- DATA %>% 
  gather(type, value) %>% 
  filter(type == "Pregnant.nonvegetarians") %>% 
  pull(value)

vegetarians <- DATA %>% 
  gather(type, value) %>% 
  filter(type == "Pregnant.vegetarians") %>% 
  pull(value)

Nonpvegetarians <- DATA %>% 
  gather(type, value) %>% 
  filter(type == "Nonpregnant.vegetarians") %>% 
  pull(value)


t.test(nonvegetarians,vegetarians)



#=====extra：怀孕期间，孕妇zn含量是否会降低
t.test(Nonpvegetarians,vegetarians)