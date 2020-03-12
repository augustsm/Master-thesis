source('code/utils.R')

raw_data = read_raw_data()
station_info = read_station_information(raw_data)

raw_data_plot = raw_data %>% gather(station, prcp, -time) %>%
  ggplot(aes(x = time, y = prcp)) + geom_line() + facet_wrap(station~., ncol = 10) +
  xlab('Time (year)') + ylab('Precipitation (mm/h)')

ggsave(raw_data_plot, 'fig/raw_data_plot.pdf')

#Plot examples of strange data
unreliable1 = raw_data %>% gather(station, prcp, -time) %>%
  filter(station == 'SN17640', time>ymd(20181001), time<ymd(20191101)) %>%
  ggplot(aes(x = time, y = prcp)) + geom_line() +
  xlab('Time') + ylab('Precipitation (mm/h)') +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),  
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))
ggsave(width = 400*0.0139, height = 300*0.0139,filename = 'fig/unreliable1.png', unreliable1)

unreliable2 =raw_data %>% gather(station, prcp, -time) %>%
  filter(station == 'SN17875', time > ymd(20190809) & time < ymd(20190814)) %>%
  ggplot(aes(x = time, y = prcp)) + geom_line() +
  xlab('Time') + ylab('Precipitation (mm/h)') +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),  
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))
ggsave(width = 400*0.0139, height = 300*0.0139,filename = 'fig/unreliable2.png', unreliable2)

unreliable3 = raw_data %>% gather(station, prcp, -time) %>%
  filter(station == 'SN19490', time > ymd(20120101) & time < ymd(20191101)) %>%
  ggplot(aes(x = time, y = prcp)) + geom_line() +
  xlab('Time') + ylab('Precipitation (mm/h)') +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),  
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))
ggsave(width = 400*0.0139, height = 300*0.0139,filename = 'fig/unreliable3.png', unreliable3)

unreliable4 = raw_data %>% gather(station, prcp, -time) %>%
  filter(station == 'SN18265', time > ymd(20180701) & time < ymd(20191101)) %>%
  ggplot(aes(x = time, y = prcp)) + geom_line() +
  xlab('Time') + ylab('Precipitation (mm/h)') +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),  
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))
ggsave(width = 400*0.0139, height = 300*0.0139,filename = 'fig/unreliable4.png', unreliable4)

unreliable5 = raw_data %>% gather(station, prcp, -time) %>%
  filter(station == 'SN18690', time > ymd(20180701) & time < ymd(20191101)) %>%
  ggplot(aes(x = time, y = prcp)) + geom_line() +
  xlab('Time') + ylab('Precipitation (mm/h)') +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),  
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))
ggsave(width = 400*0.0139, height = 300*0.0139,filename = 'fig/unreliable5.png', unreliable5)

unreliable6 = raw_data %>% gather(station, prcp, -time) %>%
  filter(station == 'SN19660', time > ymd(20180701) & time < ymd(20191101)) %>%
  ggplot(aes(x = time, y = prcp)) + geom_line() +
  xlab('Time') + ylab('Precipitation (mm/h)') +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),  
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))
ggsave(width = 400*0.0139, height = 300*0.0139,filename = 'fig/unreliable6.png', unreliable6)


data = remove_bad_data(raw_data)

stations_to_plot = colnames(data)[-1][rep(c(T, rep(F, 4)), 30)]
data_plot = data %>% gather(station, prcp, -time) %>%
  filter(station %in% stations_to_plot) %>%
  ggplot(aes(x = time, y = prcp)) + geom_line() + facet_wrap(station~., ncol = 5) +
  xlab('Time (year)') + ylab('Precipitation (mm/h)') +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))

ggsave(data_plot, filename = 'fig/data_plot.png', width = 8.27, height = 11.69)

stations_to_plot = stations_to_plot[c(T,F)]
small_scale_plot = data %>% gather(station, prcp, -time) %>%
  filter(station %in% stations_to_plot) %>%
  filter(time > ymd(20190831), time < ymd(20190904)) %>%
  ggplot(aes(x = time, y = prcp, col = station)) +
  geom_line() + xlab('Time') + ylab('Precipitation (mm/h)') +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),  
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))
ggsave(small_scale_plot, filename = 'fig/small_scale.png', width = 700*0.0139, height = 400*0.0139)

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
  

for(station in colnames(data)[-1]){
  cat(station, ': ')
  for(week in 1:53){
    tot = sum(!is.na(data[[station]][ceiling(yday(data[['time']])/7) == week]))
    if(tot==0){
      cat(week, ', ')
    }
  }
  cat('\n')
}








