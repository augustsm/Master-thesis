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

ggplot(data = NULL, aes(x = 0:52))+
  geom_ribbon(aes(min = result_binom$summary.random$week_rw$`0.025quant`,
                  max = result_binom$summary.random$week_rw$`0.975quant`), alpha = 0.3) +
  geom_line(aes(y = result_binom$summary.random$week_rw$mean), col = 'red') +
  xlab('Week') + ylab('Effect on latent field')

ggplot(data = as.data.frame(locations), aes(x = X, y = Y)) +
  geom_point(aes(col = result_binom$summary.random$index$mean)) +
  labs(col = 'Spatial effect') +
  ggtitle('Binomial stage')

ggplot(data = NULL, aes(x = 0:52))+
  geom_ribbon(aes(min = result_gp$summary.random$week_rw$`0.025quant`,
                  max = result_gp$summary.random$week_rw$`0.975quant`), alpha = 0.3) +
  geom_line(aes(y = result_gp$summary.random$week_rw$mean), col = 'red') +
  xlab('Week') + ylab('Effect on latent field')

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


gamma_effect_blindern = extract_linear_combinations(result_gamma, 
                                                    station = 'SN18700', 
                                                    station_info = station_info)
ggplot(data = NULL, aes(x=0:52)) +
  geom_ribbon(aes(min = exp(gamma_effect_blindern$lower),
                  max = exp(gamma_effect_blindern$upper)), alpha = 0.3) +
  geom_line(aes(y = exp(gamma_effect_blindern$mean)), col = 'red') +
  xlab('Week') + ylab('Gamma mean')

# plot weekly quantiles for one station
station = 'SN18700'

gamma_means = exp(extract_linear_combinations(result_gamma, station = station, 
                                              station_info = station_info)[['mean']])
gamma_prec = result_gamma$summary.hyperpar$mean[1]

p = 0.89

u_station = qgamma_mean_prec(p, gamma_means, gamma_prec)
  
probs_station = logit(extract_linear_combinations(result_binom, station = station,
                                                  station_info = station_info)[['mean']])

gp_medians_station = exp(extract_linear_combinations(result_gp, station = station,
                                                     station_info = station_info)[['mean']])

xi = result_gp$summary.hyperpar$mean[1]

alpha = 0.998
if(sum(probs_station<(1-alpha))){warning('Too low fitted probability')}

quants = u_station + gp_medians_station*(((1-alpha)/probs_station)^(-xi)-1)/((0.5)^(-xi)-1)

ggplot(data = NULL, aes(x = 0:52, y = quants)) +
  geom_point(col = 'blue')

ggplot(NULL, aes(x = 1:52))+
  geom_ribbon(aes(ymin = quantDf$lower, ymax = quantDf$upper), fill = 'gray', alpha = 0.5)+
  geom_line(aes(y=quantDf$fitted, col = 'Fitted'))+
  ggtitle('Station quantile comparison')+
  xlab('Week') + ylab('0.998 quantile') +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_line(aes(x = 1:53, y = quants))














