library(ggplot2)
library(stats)

source('code/utils.R')

locations = get_locations()

load('files/result_gamma_temp')

plot(result_gamma)

ggplot(data = as.data.frame(locations), aes(x = X, y = Y)) +
  geom_point(aes(col = result_gamma$summary.random$index$mean)) +
  labs(col = 'Spatial effect')
