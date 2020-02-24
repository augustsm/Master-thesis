library(INLA)

source('code/utils.R')

data = read_data()
station_info = read_station_information(data)

locations = get_locations(station_info = station_info)

nstat = max(station_info$index)
nweek = 53

temp = TRUE
if(temp){
  load('files/result_gamma_temp')
} else {
  load('files/result_gamma')
}

load('files/gamma_data')

p = 0.89

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

lin_combs = get_linear_combinations(nweek, nstat)

sdRef = 0.5
sdRefProb = 0.1
hyper_rw_prec = list(prec = list(prior = 'pc.prec', param = c(sdRef, sdRefProb), initial = 1.4))
hyper_iid_prec = list(prec = list(prior = 'pc.prec', param = c(0.5, 0.1), initial = 2.5))

hyper_matern = list(range = list(prior = 'pc.range', param = c(100, 0.1), initial = 1.8),
                    prec = list(prior = 'pc.prec', param = c(0.5, 0.1), initial = 2.8))

form = y ~ f(week_rw, model = 'rw2', hyper = hyper_rw_prec, cyclic = T, scale.model = T, constr = T) +
  f(week_iid, model = 'iid', hyper = hyper_iid_prec, constr = T) +
  f(index, model = 'dmatern', locations = locations, hyper = hyper_matern, constr = T)

result_binom = inla(formula = form,
                    data = binom_data,
                    family = 'binomial',
                    Ntrials = n,
                    control.family = list(link = 'logit'),
                    lincomb = lin_combs,
                    control.compute = list(config = T, openmp.strategy="pardiso.parallel"),
                    verbose = T)

save(result_binom, file = 'files/result_binom_temp')
save(binom_data, file = 'files/binom_data')
