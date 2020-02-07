library(INLA)

source('code/utils.R')

data = read_data()
station_info = read_station_information(data)

temp = FALSE
if(temp){
  load('files/result_gamma_temp')
} else {
  load('files/result_gamma')
}

# stupid method, revise
gamma_weekly_rw_effect = result_gamma$summary.random$week_rw$mean
gamma_weekly_iid_effect = result_gamma$summary.random$week_iid
gamma_spatial_effect = result_gamma$summary.random$index

gamma_prec = result_gamma$summary.hyperpar$mean[1]

# gamma_mean = function(week, index){
#   mean = exp(gamma_weekly_rw_effect[week] + gamma_weekly_iid_effect[week] + gamma_spatial_effect[index])
# } 
# 
# p = 0.89
# 
# u = qgamma(p, shape = fittedGammaMeans^2*precGamma, scale = 1/(precGamma*fittedGammaMeans))









