library(INLA)

# get utility functions
source('code/utils.R')

# read data
raw_data = read_raw_data()
data = remove_bad_data(raw_data)
station_info = read_station_information(data)

# extract station location info
locations = get_locations(station_info = station_info)

# set station index


# prepare data for fit
gamma_data =  data %>% gather(ID, prcp, -time) %>%
  filter(!is.na(prcp), prcp > 0) %>%
  full_join(.,station_info) %>%
  mutate(week_rw = floor(yday(time)/7)) %>%
  mutate(week_iid = week_rw)

gamma_data[c('time', 'ID', 'masl')] = NULL

# define linear combinations for which to compute marginals
n_weeks = 53
n_stations = max(unique(gamma_data$index))

lin_combs = get_linear_combinations(n_weeks, n_stations)

# define priors
sdRef = 0.5
sdRefProb = 0.1
hyper_rw_prec = list(prec = list(prior = 'pc.prec', param = c(sdRef, sdRefProb), initial = 3.1))
hyper_iid_prec = list(prec = list(prior = 'pc.prec', param = c(0.5,0.1), initial = 4.7))

hyper_matern = list(range = list(prior = 'pc.range', param = c(100, 0.1), initial = 1.6),
                    prec = list(prior = 'pc.prec', param = c(0.5, 0.1), initial = 3.1))

# define formula object
form = prcp ~ f(week_rw, model = 'rw2', hyper = hyper_rw_prec, cyclic = T, scale.model = T, constr = T) +
  f(week_iid, model = 'iid', hyper = hyper_iid_prec, constr = T) +
  f(index, model = 'dmatern', locations = locations, hyper = hyper_matern, constr = T)

# fit gamma stage
cat('Fit gamma stage \n')
result_gamma = inla(formula = form,
                    family = 'gamma',
                    data = as.data.frame(gamma_data),
                    control.family = list(hyper = list(prec = list(prior = 'loggamma', 
                                                                   param = c(1, 0.01), 
                                                                   initial = -0.06))),
                    num.threads = 20,
                    control.compute=list(openmp.strategy="pardiso.parallel"),
                    control.fixed = list(prec.intercept = 1),
                    lincomb = lin_combs, 
                    verbose = T)


save(result_gamma, file = 'files/result_gamma_temp')
save(gamma_data, file = 'files/gamma_data')


