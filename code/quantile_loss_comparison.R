source('code/fit_binom_stage.R')
source('code/fit_gp_stage.R')

data = read_data()
station_info = read_station_information(data)
all_data = prepare_all_data(data, station_info)

n_weeks = 53
n_stat = dim(station_info)[1]

load('files/result_gamma_temp')

alpha = 0.998
alpha_ref = 0.995


gamma_means = exp(extract_linear_combinations(result_gamma, station_info = station_info)[['mean']])
gamma_prec = result_gamma$summary.hyperpar$mean[1]

p_vec = seq(0.8, 0.93, 0.01)


loss = vector(length = length(p_vec))

for(i in 1:length(p_vec)){
  p = p_vec[i]
  u = qgamma_mean_prec(p, gamma_means, gamma_prec)
  
  fit_binom_stage(p = p)
  fit_gp_stage(p = p)
  
  load('files/result_binom_temp')
  load('files/result_gp_temp')
  
  probs = logit(extract_linear_combinations(result_binom, station_info = station_info)[['mean']])
  
  if(sum(probs<(1-alpha))){
    loss[i] = NULL
    next
  }
  
  gp_medians = exp(extract_linear_combinations(result_gp, station_info = station_info)[['mean']])*gamma_means
  xi = result_gp$summary.hyperpar$mean[1]

  quants = u + gp_medians*(((1-alpha)/probs)^(-xi)-1)/((0.5)^(-xi)-1)
  
  dat = all_data %>% mutate(quant = quants[(index-1)*n_weeks + week])
  
  loss[i] = quantile_loss(dat$prcp, dat$quant, alpha_ref)
}

save(loss, file = 'files/loss')










