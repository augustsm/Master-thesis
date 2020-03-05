source('code/utils.R')

raw_data = read_raw_data()
station_info = read_station_information(raw_data)

raw_data_plot = raw_data %>% gather(station, prcp, -time) %>%
  ggplot(aes(x = time, y = prcp)) + geom_line() + facet_wrap(station~., ncol = 10) +
  xlab('Time (year)') + ylab('Precipitation (mm/h)')

ggsave(raw_data_plot, 'fig/raw_data_plot.pdf')

raw_data %>% gather(station, prcp, -time) %>%
  filter(station %in% unique(station)[rep(c(T, rep(F,10)),15)]) %>%
  ggplot(aes(x = time, y = prcp)) + geom_line() + facet_wrap(station~., ncol = 4) +
  xlab('Year') + ylab('Precipitation (mm/h)')

data = remove_bad_data(raw_data)

statistics_df = data.frame('ID' = colnames(data[,-1]), 'mean' = colMeans(data[,-1], na.rm = T))
quant_df = data %>% gather(ID, val, -time) %>% group_by(ID) %>% summarise(`0.99quant` = quantile(val, 0.99, na.rm=T))

station_info %>% full_join(.,statistics_df) %>%
  ggplot(aes(x=X, y=Y, col = mean)) + geom_point()

station_info %>% full_join(.,quant_df) %>%
  ggplot(aes(x=X, y=Y, col = `0.99quant`)) + geom_point()

data %>% gather(station, val, -time) %>% 
  group_by(year = year(time), station) %>%
  summarise(n = sum(!is.na(val))) %>%
  ggplot(aes(x = year, y = n)) + geom_line() + facet_wrap(station~.)

data %>% gather(station, val, -time) %>% 
  group_by(year = year(time), station) %>%
  summarise(p = sum(val==0, na.rm=T)/sum(!is.na(val))) %>%
  ggplot(aes(x = year, y = p)) + geom_line() + facet_wrap(station~.)

data %>% gather(station, val, -time) %>% 
  group_by(year = year(time), station) %>%
  summarise(m = mean(val, na.rm = T)) %>% 
  filter(m < 0.4) %>%
  ggplot(aes(x = year, y = m)) + geom_line() + facet_wrap(station~.)

data %>% gather(station, val, -time) %>% 
  group_by(year = year(time), station) %>%
  summarise(q = quantile(val, 0.99, na.rm = T)) %>%
  ggplot(aes(x = year, y = q)) + geom_line() + facet_wrap(station~.)

data %>% gather(station, val, -time) %>%
  filter(val>0) %>%
  group_by(week = floor(yday(time)/7)) %>%
  summarise(mean = mean(val, na.rm = T)) %>%
  ggplot(aes(x=1:53, y = mean)) + geom_line() + 
  geom_line(aes(y=exp(result_gamma$summary.random$week$mean+result_gamma$summary.fixed$mean)), 
            col = 'red')

coords = as.data.frame(get_locations(station_info = station_info))
coords$ID = station_info$ID
filtered_data_spat_plot = data %>% gather(ID, prcp, -time) %>%
  filter(!is.na(prcp)) %>%
  group_by(ID) %>%
  summarise(num_obs = length(prcp), quant = quantile(prcp, 0.998)) %>%
  full_join(., coords) %>%
  ggplot(aes(x = X, y = Y, col = quant, size = num_obs)) + 
    geom_point() + xlab('West - East') + ylab('South - North') +
    labs(size = 'Number of observations', col =  '0.998-quantile')
    
ggsave(filename = 'fig/filtered_data_spat_plot.png', 
       plot = filtered_data_spat_plot,
       width = 10, height = 10)

for(i in 1:12){
print(data %>%
  gather(ID, prcp, -time) %>%
  filter(prcp > 0, month(time) == i) %>%
  mutate(year= year(time)) %>%
  group_by(ID, year) %>%
  summarise(max = max(prcp)) %>%
  group_by(ID) %>%
  summarise(m = mean(max)) %>%
  full_join(.,station_info) %>%
  filter(!is.na(m)) %>%
  ggplot(aes(x = X, y = Y, col = m)) + geom_point())
}

data %>% gather(ID, prcp, -time) %>%
  filter(prcp>0) %>%
  group_by(ID) %>%
  summarise(q = quantile(prcp, 0.95), m = median(prcp)) %>%
  full_join(.,station_info) %>%
  ggplot(aes(x=masl, y = q)) + geom_point(col = 'blue') + geom_point(aes(y = m), col = 'red')


raw_data %>% filter(SN17875>5) %>%
  ggplot(aes(x = SN17875)) + geom_histogram(boundary = 5)
  








