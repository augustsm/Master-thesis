library(ggplot2)
library(stats)

source('code/utils.R')

data = read_data()
station_info = read_station_information(data)
locations = get_locations(station_info = station_info)

load('files/result_gamma_temp')
load('files/result_binom_temp')
load('files/result_gp_temp')

load('files/gamma_data')

load('files/loss')

plot(result_gamma)

for(i in 1:53){
print(ggplot(data = as.data.frame(locations), aes(x = X, y = Y)) +
  geom_point(aes(col = result_binom$summary.random$index$mean[((i-1)*148+1):(i*148)])) +
  labs(col = 'Spatial effect') +
  ggtitle('Gamma stage'))
}

ggplot(data = as.data.frame(locations), aes(x = X, y = Y)) +
  geom_point(aes(col = result_gamma$summary.random$index$mean)) +
  labs(col = 'Spatial effect') +
  ggtitle('Gamma stage')

ggplot(data = as.data.frame(locations), aes(x = X, y = Y)) +
  geom_point(aes(col = result_gamma$summary.random$index_sin$mean)) +
  labs(col = 'Spatial effect') +
  ggtitle('Gamma stage, sine term')

ggplot(data = as.data.frame(locations), aes(x = X, y = Y)) +
  geom_point(aes(col = result_gamma$summary.random$index_cos$mean)) +
  labs(col = 'Spatial effect') +
  ggtitle('Gamma stage, cosine term')

ggplot(data = NULL, aes(x = result_gamma$summary.random$index_cos$mean, 
                        y = result_gamma$summary.random$index_sin$mean,
                        label = station_info$ID)) +
  geom_point() +
  geom_text(aes(vjust = 2), size = 3) +
  xlab('Cos') + ylab('Sin')

ggplot(data = NULL, aes(x = 1:53))+
  geom_ribbon(aes(min = result_binom$summary.random$week_rw$`0.025quant`,
                  max = result_binom$summary.random$week_rw$`0.975quant`), alpha = 0.3) +
  geom_line(aes(y = result_binom$summary.random$week_rw$mean), col = 'red') +
  xlab('Week') + ylab('Effect on latent field')

ggplot(data = as.data.frame(locations), aes(x = X, y = Y)) +
  geom_point(aes(col = result_binom$summary.random$index$mean)) +
  labs(col = 'Spatial effect') +
  ggtitle('Binomial stage')

ggplot(data = NULL, aes(x = 1:53))+
  geom_ribbon(aes(min = result_gp$summary.random$week_rw$`0.025quant`,
                  max = result_gp$summary.random$week_rw$`0.975quant`), alpha = 0.3) +
  geom_line(aes(y = result_gp$summary.random$week_rw$mean), col = 'red') +
  xlab('Week') + ylab('RW effect on latent field')

ggplot(data = NULL, aes(x = 1:53))+
  geom_ribbon(aes(min = result_gp$summary.random$week_iid$`0.025quant`,
                  max = result_gp$summary.random$week_iid$`0.975quant`), alpha = 0.3) +
  geom_line(aes(y = result_gp$summary.random$week_iid$mean), col = 'red') +
  xlab('Week') + ylab('Iid effect on gp latent field')

ggplot(data = as.data.frame(locations), aes(x = X, y = Y)) +
  geom_point(aes(col = result_gp$summary.random$index$mean)) +
  labs(col = 'Spatial effect') +
  ggtitle('GP stage')

ggplot(data = NULL, aes(x=1:53)) +
  geom_line(aes(y = result_gamma$summary.random$week_rw$mean, col = 'RW')) +
  geom_line(aes(y = result_gamma$summary.random$week_iid$mean, col = 'IID')) +
  geom_line(aes(y = result_gamma$summary.random$week_rw$mean + result_gamma$summary.random$week_iid$mean, col = 'Total')) +
  xlab('Week') + ylab('Effect on latent field')

ggplot(data = NULL, aes(x=1:53)) +
  geom_line(aes(y = result_binom$summary.random$week_rw$mean, col = 'RW')) +
  geom_line(aes(y = result_binom$summary.random$week_iid$mean, col = 'IID')) +
  geom_line(aes(y = result_binom$summary.random$week_rw$mean + result_binom$summary.random$week_iid$mean, col = 'Total')) +
  xlab('Week') + ylab('Effect on latent field')

ggplot(data = NULL, aes(x=1:53)) +
  geom_line(aes(y = result_gp$summary.random$week_rw$mean, col = 'RW')) +
  geom_line(aes(y = result_gp$summary.random$week_iid$mean, col = 'IID')) +
  geom_line(aes(y = result_gp$summary.random$week_iid$mean + result_gp$summary.random$week_rw$mean, col = 'Total')) +
  xlab('Week') + ylab('Effect on latent field')

for(i in 79:89){
  linear_combinations = extract_linear_combinations(result_gamma, 
                                                    station = index_to_station(i, station_info),
                                                    station_info = station_info)
  
  station_data = gamma_data[gamma_data$index==i,]
  n = dim(station_data)[1]
  m = sapply(1:53, function(j) mean(station_data$prcp[station_data$week_rw==j]))
  print(ggplot(data=NULL, aes(x=1:53)) +
          geom_ribbon(aes(min=exp(linear_combinations$lower), max=exp(linear_combinations$upper)), alpha = 0.3) +
          geom_boxplot(data = station_data, aes(x = week_rw, y = prcp, group = week_rw)) +
          geom_line(aes(y=exp(linear_combinations$mean)), col = 'blue') +
          geom_line(aes(y = m), col = 'red', linetype = 'dashed') +
          geom_label(aes(x=-Inf,y=Inf, hjust=-1, vjust=1, label = paste(c('n=',as.character(n)), collapse = ''))) +
          xlab('Week') + ylab('Observations vs posterior') +
          ggtitle(index_to_station(i, station_info)))
}

posterior_means_df = as.data.frame(matrix(nrow = 0, ncol = 5))
colnames(posterior_means_df) = c('mean', 'lower', 'upper', 'week', 'station')
for(stat in get_close_stations('SN28923', station_info, locations, d_max = 20, n = Inf)$ID){
  posterior_means_df = rbind(posterior_means_df,
                             cbind(extract_linear_combinations(result_gamma, station = stat, station_info = station_info),
                                   station = rep(stat, 53), week = 1:53)                             )
}
ggplot(data = posterior_means_df, aes(x=week)) +
  geom_ribbon(aes(min = lower, max = upper, fill = station), alpha = 0.3) +
  geom_line(aes(y = mean, col = station)) +
  xlab('Week') + ylab('Posterior effect') +
  ggtitle('Comparison of close stations')

  for(i in 1:157){
print(ggplot(data = NULL) +
  geom_line(aes(x=1:53,y=exp(result_gamma$summary.random$index$mean[(0:52)*157+i]+result_gamma$summary.random$week_iid$mean)), col = 'blue') +
  geom_point(aes(x=gamma_data[gamma_data$index==i,]$week_rw, y = gamma_data[gamma_data$index==i,]$prcp)) +
  xlab('Week') + ylab('Observations vs posterior mean') +
  ggtitle(index_to_station(i, station_info)))
}

plot_spatio_temporal_effect = function(stage, stations){
  
  effect_df = data.frame(matrix(ncol = 3, nrow = 0))
  colnames(effect_df) = c('effect', 'station', 'week')
  if(stage == 'gamma'){
    for(station in stations){
      i = station_to_index(station, station_info)
      effect_i = data.frame('effect' = result_gamma$summary.random$index$mean[(0:52)*157+i],
                            'station' = rep(station, 53),
                            'week' = 1:53)
      effect_df = rbind(effect_df, effect_i)
    }
  }else if(stage == 'binomial'){
    for(station in stations){
      i = station_to_index(station, station_info)
      effect_i = data.frame('effect' = result_binom$summary.random$index$mean[(0:52)*157+i],
                            'station' = rep(station, 53),
                            'week' = 1:53)
      effect_df = rbind(effect_df, effect_i)
    }  
  }else if(stage == 'gp'){
    for(station in stations){
      i = station_to_index(station, station_info)
      effect_i = data.frame('effect' = result_gp$summary.random$index$mean[(0:52)*157+i],
                            'station' = rep(station, 53),
                            'week' = 1:53)
      effect_df = rbind(effect_df, effect_i)
    }  
  }
  
  print(ggplot(data = effect_df, aes(x = week)) +
          geom_line(aes(y = effect, col = station))+
          xlab('Week') + ylab('Effect on latent field') +
          ggtitle(paste(c(stage, ' stage at ', stations[1]), collapse = '')))
}

gamma_effect_blindern = extract_linear_combinations(result_gamma, 
                                                    station = 'SN18700', 
                                                    station_info = station_info)
observed_quants_blindern = sapply(1:53, function(i) mean(data$SN18700[week(data$time)==i & data$SN18700>0], na.rm=T))
ggplot(data = NULL, aes(x=1:53)) +
  geom_ribbon(aes(min = exp(gamma_effect_blindern$lower),
                  max = exp(gamma_effect_blindern$upper)), alpha = 0.3) +
  geom_line(aes(y = exp(gamma_effect_blindern$mean)), col = 'red') +
  geom_line(aes(y = observed_quants_blindern), col = 'blue', linetype = 'dashed')
  xlab('Week') + ylab('Gamma mean')
ggsave(filename = 'fig/posterior_gamma_mean.png', width = 5, height = 5)

ggplot(data = as.data.frame(result_gp$marginals.hyperpar$`Tail parameter for the genPareto observations`),
       aes(x = x, y = y)) +
  geom_line(aes(col = 'Posterior'))

# plot weekly quantiles for one station
total_quants = sapply(1:53, function(i) quantile(all_data$prcp[all_data$week == i], alpha))

for(i in 1:148){

station = index_to_station(i, station_info)

gamma_means = exp(extract_linear_combinations(result_gamma, station = station, 
                                              station_info = station_info)[['mean']])
gamma_prec = result_gamma$summary.hyperpar$mean[1]

p = 0.89

u_station = qgamma_mean_prec(p, gamma_means, gamma_prec)
  
probs_station = logit(extract_linear_combinations(result_binom, station = station,
                                                  station_info = station_info)[['mean']])

gp_medians_station = exp(extract_linear_combinations(result_gp, station = station,
                                                     station_info = station_info)[['mean']])*gamma_means

xi = result_gp$summary.hyperpar$mean[1]

alpha = 0.998
if(sum(probs_station<(1-alpha))){warning('Too low fitted probability')}

quants = u_station + gp_medians_station*(((1-alpha)/probs_station)^(-xi)-1)/((0.5)^(-xi)-1)

ggplot(data = NULL, aes(x = 1:53, y = quants)) +
  geom_point(col = 'blue')

stat_obs = all_data[all_data$index == i,]
n = dim(stat_obs)[1]
station_quants = sapply(1:53, function(j) quantile(stat_obs$prcp[stat_obs$week==j], alpha))

close_stations = get_close_stations(station, station_info, locations, n=Inf, d_max = 10)$ID
close_data = all_data[all_data$ID %in% close_stations,]
close_station_quants = sapply(1:53, function(j) quantile(close_data$prcp[close_data$week==j], alpha))

# need to run inla temporal from project thesis before running this code chunk
print(ggplot(NULL, aes(x = 1:53))+
  # geom_ribbon(aes(ymin = quantDf$lower, ymax = quantDf$upper), fill = 'gray', alpha = 0.5)+
  # geom_line(aes(y=quantDf$fitted, col = 'Old'))+
  ggtitle(paste(c('Fitted vs observed on ', station), collapse = ''))+
  xlab('Week') + ylab('0.998 quantile') +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_line(aes(x = 1:53, y = quants, col = 'Fitted')) +
  geom_line(aes(y=station_quants, col = 'Observed'), linetype = 'dashed') +
  geom_line(aes(y=close_station_quants, col = 'Close stations'), linetype = 'dashed') +
  geom_line(aes(x=1:53,y = total_quants, col = 'Population wide'), linetype = 'dashed')+
  geom_label(aes(x=-Inf,y=Inf, hjust=-1, vjust=1, label = paste(c('n=',as.character(n)), collapse = ''))))
}

ggsave(filename = paste(c('fig/old_vs_new_', station, '.png'), collapse = ''),
       width = 10, height = 10)



loss_plot = ggplot(data = NULL, aes(x = p_vec, y = loss)) +
  geom_line(col = 'blue') +
  xlab('Threshold probability') + 
  ylab('Total loss')
ggsave(filename = 'fig/loss_plot.png', plot = loss_plot, width = 700*0.0139, height = 400*0.0139)











