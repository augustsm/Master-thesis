library(INLA)

source('code/utils.R')

data = read_data()
station_info = read_station_information(data)

locations = get_locations(station_info = station_info)

nstat = max(station_info$index)
nweek = 53

temp = FALSE
if(temp){
  load('files/result_gamma_temp')
} else {
  load('files/result_gamma')
}

load('files/gamma_data')

p = 0.89

gamma_prec = result_gamma$summary.hyperpar$mean[1]

gamma_log_mean = result_gamma$summary.linear.predictor$mean
fitted_means = exp(gamma_log_mean)

gamma_data$gamma_log_mean = gamma_log_mean
gamma_data$u = qgamma(p, shape = fitted_means^2*gamma_prec, scale = 1/(gamma_prec*fitted_means))

gp_data = gamma_data %>% filter(u<prcp) %>%
  mutate(y = prcp-u)

gp_data[c('prcp',  'u')] = NULL

sdRef = 0.5
sdRefProb = 0.1
hyper_rw_prec = list(prec = list(prior = 'pc.prec', param = c(sdRef, sdRefProb)))

hyper_matern = list(range = list(prior = 'pc.range', param = c(40000, 0.1)))

form = y ~ f(week_rw, model = 'rw2', hyper = hyper_rw_prec, cyclic = T, scale.model = T, constr = T) +
  f(week_iid, model = 'iid', constr = T) +
  f(index, model = 'dmatern', locations = locations, hyper = hyper_matern, constr = T) +
  offset(gamma_log_mean)

result_gp = inla(form,
                data = gp_data,
                family = 'gp',
                control.family = list(control.link = list(quantile = 0.5),
                                      hyper = list(xi = list(prior = 'pc.gevtail', param = c(4.5, 0, 0.99)))),
                #control.predictor = list(compute = T, link = 1),
                control.predictor = list(compute = T, quantiles = c(0.5)),
                control.compute=list(openmp.strategy="pardiso.parallel"),
                num.threads = 30,
                verbose = T)

save(result_gp, file = 'files/result_gp_temp')


