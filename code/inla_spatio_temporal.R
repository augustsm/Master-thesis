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

# define data for which predictions are needed
n_weeks = 53
n_stations = max(unique(gamma_data$index))

pred_data = data.frame('prcp' = rep(NA, n_weeks*n_stations),
                       'week_rw' = rep(0:(n_weeks-1), n_stations),
                       'week_iid' = rep(0:(n_weeks-1), n_stations),
                       'index' = rep(1:n_stations, each = n_weeks))
# 
# gamma_data = rbind(gamma_data, pred_data)

# define priors
sdRef = 0.5
sdRefProb = 0.1
hyper_rw_prec = list(prec = list(prior = 'pc.prec', param = c(sdRef, sdRefProb), initial = 3.9))
hyper_iid_prec = list(prec = list(prior = 'pc.prec', param = c(0.01,0.1), initial = 4.4))

hyper_matern = list(range = list(prior = 'pc.range', param = c(40, 0.1), initial = 4.3),
                    prec = list(prior = 'pc.prec', param = c(0.01, 0.1), initial = 2.7))

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
                    #control.predictor = list(compute = T, link = 1),
                    verbose = T)


save(result_gamma, file = 'files/result_gamma_temp')
save(gamma_data, file = 'files/gamma_data')


