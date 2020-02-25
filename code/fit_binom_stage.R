library(INLA)

source('code/utils.R')
source('code/data_preparation.R')

data = read_data()
station_info = read_station_information(data)
locations = get_locations(station_info = station_info)

load_bool = TRUE
if(load_bool){
  load('files/binom_data')
}else{
  binom_data = prepare_binom_data(data = data, station_info = station_info, temp = TRUE)
  save(binom_data, file = 'files/binom_data')
}

nstat = max(station_info$index)
nweek = 53
lin_combs = get_linear_combinations(nweek, nstat)

sdRef = 2
sdRefProb = 0.1
hyper_rw_prec = list(prec = list(prior = 'pc.prec', param = c(sdRef, sdRefProb), initial = 1.4))
hyper_iid_prec = list(prec = list(prior = 'pc.prec', param = c(0.1, 0.1), initial = 2.5))

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

