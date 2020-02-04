library(tidyverse)
library(INLA)

source('code/utils.R')

raw_data = read_raw_data()
data = remove_bad_data(raw_data)
station_info = read_station_information(data)

station_info$index = 1:dim(station_info)[1]
locations = station_info[c('X', 'Y')]
locations = as.data.frame(latlon_to_laea(locations)@coords)
locations = locations %>% mutate(x = (X-min(X))/(max(X)-min(X)), y = (Y-min(Y))/(max(X)-min(X)))
station_info[c('X', 'Y', 'masl')] = NULL

monthly_max_values = data %>%
  gather(ID, prcp, -time) %>%
  filter(prcp > 0) %>%
  mutate(month = month(time), year= year(time)) %>%
  group_by(ID, year, month) %>%
  summarise(max = max(prcp)) %>%
  full_join(.,station_info) %>%
  filter(!is.na(max))

# define priors
hyper_matern = list(range = list(prior = 'pc.range', param = c(0.2, 0.1)))
hyper_gev = list(tail = list(prior="pc.gevtail", param=c(7, 0, 0.99)))
# define formula object
form = max ~ f(index, model = 'dmatern', locations = as.matrix(locations), hyper = hyper_matern, constr = T)

results = list()
for(i in 1:12){
  gev_data = monthly_max_values %>%
    filter(month == i)
  
  gev_data[c('month', 'year', 'ID')] = NULL
  
  results[[i]] = inla(formula = form,
                    data = gev_data,
                    family = 'gev2',
                    scale = 1/900,
                    control.family = list(hyper = hyper_gev),
                    control.predictor = list(compute = T, link = 1),
                    verbose = T)
  plot(results[[i]])
  print(ggplot(data = NULL, aes(x = 1:dim(gev_data)[1])) +
    geom_line(aes(y = results[[i]]$summary.linear.predictor$mean), col = 'red') +
    geom_line(aes(y = gev_data$max), col = 'blue')) +
    geom_ribbon(aes(min = results[[i]]$summary.linear.predictor$`0.025quant`,
                    max = results[[i]]$summary.linear.predictor$`0.975quant`), alpha = 0.3) +
    xlim(1,200)
}

ggplot(data = locations, aes(x=X,y=Y)) +
  geom_point(aes(col = sapply(station_info$index, function(x) mean(results[[12]]$summary.linear.predictor$mean[gev_data$index==x])))) +
  labs(col = 'eta')
             