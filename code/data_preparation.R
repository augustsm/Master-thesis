if(!exists('qgamma_mean_prec')){
  source('code/utils.R')
}

get_p = function(){
  p = 0.89
}

prepare_gamma_data = function(data, station_info){
  gamma_data =  data %>% gather(ID, prcp, -time) %>%
    filter(!is.na(prcp), prcp > 0) %>%
    full_join(.,station_info) %>%
    mutate(week_rw = floor(yday(time)/7)) %>%
    mutate(week_iid = week_rw)
  
  gamma_data[c('time', 'ID', 'masl')] = NULL
  gamma_data
}

prepare_binom_data = function(data, station_info, temp, p = NULL){
  
  nstat = max(station_info$index)
  nweek = 53
  
  if(temp){
    load('files/result_gamma_temp')
  } else {
    load('files/result_gamma')
  }
  
  load('files/gamma_data')
  
  if(is.null(p)){
    p = get_p()
  }
  
  gamma_prec = result_gamma$summary.hyperpar$mean[1]
  
  fitted_means = exp(result_gamma$summary.lincomb.derived$mean)
  gamma_data = gamma_data %>% mutate(means = fitted_means[(index-1)*nweek + week_rw +1]) %>%
    mutate(u = qgamma_mean_prec(p, means, gamma_prec))
  gamma_data$means = NULL
  
  extreme_obs = gamma_data %>% filter(u<prcp)
  
  binom_data = data.frame(index = rep(1:nstat, each = nweek),
                          week_rw = rep(0:(nweek-1), nstat),
                          week_iid = rep(0:(nweek-1), nstat))
  
  y = sapply(1:(nstat*nweek), function(i) sum(extreme_obs$index == binom_data$index[i] &
                                                extreme_obs$week_rw == binom_data$week_rw[i]))
  binom_data$y = y
  
  all_data = data %>% gather(ID, prcp, -time) %>%
    filter(!is.na(prcp)) %>%
    mutate(week = floor(yday(time)/7)) %>%
    full_join(., station_info)
  
  all_data[c('ID', 'masl', 'X', 'Y')] = NULL
  
  n = sapply(1:(nstat*nweek), function(i) sum(all_data$index == binom_data$index[i] &
                                                all_data$week == binom_data$week_rw[i]))
  
  binom_data$n = n
  binom_data = binom_data %>% filter(n != 0)
  
  binom_data
}

prepare_gp_data = function(data, station_info, temp, p = NULL){
  
  nstat = max(station_info$index)
  nweek = 53
  
  if(temp){
    load('files/result_gamma_temp')
  } else {
    load('files/result_gamma')
  }
  
  load('files/gamma_data')
  
  if(is.null(p)){
    p = get_p()
  }
  
  gamma_prec = result_gamma$summary.hyperpar$mean[1]
  
  gamma_log_mean = result_gamma$summary.lincomb.derived$mean
  
  gp_data = gamma_data %>% mutate(log_mean = gamma_log_mean[(index-1)*nweek + week_rw +1]) %>%
    mutate(u = qgamma_mean_prec(p, exp(log_mean), gamma_prec)) %>%
    filter(u < prcp) %>%
    mutate(y = prcp - u)
  
  gp_data[c('prcp',  'u')] = NULL
}
