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

fitted_means = exp(result_gamma$summary.linear.predictor$mean)
gamma_data$u = qgamma(p, shape = fitted_means^2*gamma_prec, scale = 1/(gamma_prec*fitted_means))

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

sdRef = 0.5
sdRefProb = 0.1
hyper_rw_prec = list(prec = list(prior = 'pc.prec', param = c(sdRef, sdRefProb)))

hyper_matern = list(range = list(prior = 'pc.range', param = c(40000, 0.1)))

form = y ~ f(week_rw, model = 'rw2', hyper = hyper_rw_prec, cyclic = T, scale.model = T, constr = T) +
  f(week_iid, model = 'iid', constr = T) +
  f(index, model = 'dmatern', locations = locations, hyper = hyper_matern, constr = T)

result_binom = inla(formula = form,
                      data = binom_data,
                      family = 'binomial',
                      Ntrials = n,
                      control.family = list(link = 'logit'),
                      control.predictor = list(compute = T),
                      control.compute = list(config = T))

save(result_binom, file = 'files/result_binom_temp')

