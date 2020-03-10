library(INLA)

# get utility functions
source('code/utils.R')
source('code/data_preparation.R')

# read data
raw_data = read_raw_data()
data = remove_bad_data(raw_data)
station_info = read_station_information(data)

# extract station location info
locations = get_locations(station_info = station_info)

# prepare data for fit
gamma_data = prepare_gamma_data(data, station_info)

# define linear combinations for which to compute marginals
n_weeks = 53
n_stations = max(unique(gamma_data$index))

lin_combs = get_linear_combinations(n_weeks, n_stations, model = 'periodic')

# define priors
sdRef = 1
sdRefProb = 0.1
hyper_rw_prec = list(prec = list(prior = 'pc.prec', param = c(sdRef, sdRefProb), initial = 1.5))
hyper_iid_prec = list(prec = list(prior = 'pc.prec', param = c(0.5, 0.1), initial = 2.6))
hyper_group_prec = list(prec = list(prior = 'pc.prec', param = c(0.01, 0.1), initial = 2.6))

hyper_matern = list(range = list(prior = 'pc.range', param = c(100, 0.1), initial = 1.7),
                    prec = list(prior = 'pc.prec', param = c(0.1, 0.1), initial = 1))

# formula with rw2 and iid for week and dmatern for space
# form = prcp ~ f(week_rw, model = 'rw2', hyper = hyper_rw_prec, cyclic = T, scale.model = T, constr = T) +
#   f(week_iid, model = 'iid', hyper = hyper_iid_prec, constr = T) +
#   f(index, model = 'dmatern', locations = locations, hyper = hyper_matern, constr = T)

# formula with rw2 and iid for week and grouped rw2 dmatern for space
# form = prcp ~
#   f(week_rw,
#     model = 'rw2',
#     hyper = hyper_rw_prec,
#     cyclic = T,
#     scale.model = T,
#     constr = T) +
#   f(week_iid,
#     model = 'iid',
#     hyper = hyper_iid_prec,
#     constr = T) +
#   f(index,
#     model = 'dmatern',
#     locations = locations,
#     hyper = hyper_matern,
#     constr = T,
#     group = iweek,
#     control.group = list(model = 'rw2',
#                          hyper = hyper_group_prec,
#                          cyclic = T,
#                          scale.model = F,
#                          fixed = F)
#     )

form = prcp ~ f(index, model = 'dmatern', locations = locations, hyper = hyper_matern, constr = T) +
  f(index_cos, cos_week, model = 'dmatern', locations = locations, hyper = hyper_matern) +
  f(index_sin, sin_week, model = 'dmatern', locations = locations, hyper = hyper_matern)
  

#gamma_data = as.data.frame(gamma_data)[rep(c(T, rep(F,100)),7000),]

# fit gamma stage
cat('Fit gamma stage \n')
result_gamma = inla(formula = form,
                    family = 'gamma',
                    data = as.data.frame(gamma_data),
                    control.family = list(hyper = list(prec = list(prior = 'loggamma', 
                                                                   param = c(1, 0.01), 
                                                                   initial = -0.02))),
                    num.threads = 20,
                    control.compute=list(openmp.strategy="pardiso.parallel"),
                    control.fixed = list(prec.intercept = 1, prec = 1),
                    lincomb = lin_combs, 
                    verbose = T)


save(result_gamma, file = 'files/result_gamma_temp')
save(gamma_data, file = 'files/gamma_data')


