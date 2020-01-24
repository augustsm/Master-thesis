library(tidyverse)

directory = '/home/shomed/a/augustsm/Documents/Master-thesis/data/Hourly_precipitation'
setwd(directory)
file_list = list.files(directory)

dataset = read_csv(file = file_list[1], col_types = 'Td')
dataset$obs = NULL

for(file in file_list){
  df = read_csv(file = file, col_types = 'Td')
  stat_name = strsplit(file, '_')[[1]][1]
  colnames(df) = c('time', stat_name)
  
  df = df %>% filter(lubridate::year(time) > 2008)
  
  dataset = dataset %>% full_join(., df)
}

dataset = dataset[order(dataset$time),]

write_csv(dataset, '../data_2009-2019.csv')
setwd('/home/shomed/a/augustsm/Documents/Master-thesis')
