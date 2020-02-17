library(ggplot2)
library(stats)

source('code/utils.R')

locations = get_locations()

load('files/result_gamma_temp')
load('files/result_binom_temp')
load('files/result_gp_temp')

load('files/gamma_data')

plot(result_gamma)

ggplot(data = as.data.frame(locations), aes(x = X, y = Y)) +
  geom_point(aes(col = result_gamma$summary.random$index$mean)) +
  labs(col = 'Spatial effect') +
  ggtitle('Gamma stage')

plot(result_binom)

ggplot(data = as.data.frame(locations), aes(x = X, y = Y)) +
  geom_point(aes(col = result_binom$summary.random$index$mean)) +
  labs(col = 'Spatial effect') +
  ggtitle('Binomial stage')

plot(result_gp)

ggplot(data = as.data.frame(locations), aes(x = X, y = Y)) +
  geom_point(aes(col = result_gp$summary.random$index$mean)) +
  labs(col = 'Spatial effect') +
  ggtitle('GP stage')

ggplot(data = NULL, aes(x=0:52)) +
  geom_line(aes(y = result_gamma$summary.random$week_rw$mean, col = 'RW')) +
  geom_line(aes(y = result_gamma$summary.random$week_iid$mean, col = 'IID')) +
  geom_line(aes(y = result_gamma$summary.random$week_rw$mean + result_gamma$summary.random$week_iid$mean, col = 'Total')) +
  xlab('Week') + ylab('Effect on latent field')

ggplot(data = NULL, aes(x=0:52)) +
  geom_line(aes(y = result_binom$summary.random$week_rw$mean, col = 'RW')) +
  geom_line(aes(y = result_binom$summary.random$week_iid$mean, col = 'IID')) +
  geom_line(aes(y = result_binom$summary.random$week_rw$mean + result_binom$summary.random$week_iid$mean, col = 'Total')) +
  xlab('Week') + ylab('Effect on latent field')

ggplot(data = NULL, aes(x=0:52)) +
  geom_line(aes(y = result_gp$summary.random$week_rw$mean, col = 'RW')) +
  geom_line(aes(y = result_gp$summary.random$week_iid$mean, col = 'IID')) +
  geom_line(aes(y = result_gp$summary.random$week_iid$mean + result_gp$summary.random$week_rw$mean, col = 'Total')) +
  xlab('Week') + ylab('Effect on latent field')



# plot weekly quantiles for one station
station = 'SN26975'
gamma_means_station = sapply(0:52, function(i) exp(result_gamma$summary.linear.predictor$mean[min(which(
  gamma_data$index == station_to_index(station, station_info) & gamma_data$week_rw==i
))]))

gamma_prec = result_gamma$summary.hyperpar$mean[1]
p = 0.89
u_station = qgamma(p, shape = gamma_means_station^2*gamma_prec, scale = 1/(gamma_prec*gamma_means_station))

probs_station = sapply(0:52, function(i) result_binom$summary.fitted.values$mean[
  binom_data$index == station_to_index(station, station_info) & binom_data$week_rw == i
])

gp_quants_station = sapply(0:52, function(i) result_gp$summary.fitted.values$mean[min(which(
  gp_data$index == station_to_index(station, station_info) & gp_data$week_rw == i
))])

xi = result_gp$summary.hyperpar$mean[1]

alpha = 0.998
if(sum(probs_station<(1-alpha))){warning('Too low fitted probability')}

quants = u_station + gp_quants_station*(((1-alpha)/probs_station)^(-xi)-1)/((0.5)^(-xi)-1)

ggplot(data = NULL, aes(x = 0:52, y = quants)) +
  geom_point(col = 'blue')














