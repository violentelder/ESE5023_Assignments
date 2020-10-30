#=======Was Tyrannosaurus Rex Warm-Blooded?=======
library(tidyr)
library(dplyr)
library(ggplot2)
setwd('D:/ESE5023/ESE5023_Assignments/Data')

DATA <- read.csv("Tyrannosaurus.csv",header = T)
DATA <- as_tibble(DATA)
DATA

anova_one_way <- aov(value~factor(name), data = DATA)
summary(anova_one_way)


  



