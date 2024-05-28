getwd()
library(dplyr)
setwd('C:\\Users\\eliro\\Documents\\CHIARA\\UNIVERSITA\\DATA MANAGMENT UNIT 2')
codes=read.csv('country_code.csv', encoding="latin1", check.names = FALSE)

distances=read.csv('distanze.csv',sep=";", encoding="latin1", check.names = FALSE)

file2_filtrato <- distances %>%
  filter(distances$orig %in% codes$codes)

file2_filtrato <- file2_filtrato %>%
  filter(file2_filtrato$dest %in% codes$codes)

file2_filtrato <- file2_filtrato %>%
  mutate(dist = ifelse(orig == dest, 0, dist))
