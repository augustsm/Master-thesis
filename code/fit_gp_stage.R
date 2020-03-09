library(INLA)

source('code/utils.R')
source('code/data_preparation.R')

data = read_data()
station_info = read_station_information(data)
locations = get_locations(station_info = station_info)

load_bool = TRUE
if(load_bool){
  load('files/gp_data')
}else{
  gp_data = prepare_gp_data(station_info, temp = TRUE)
  save(gp_data, file = 'files/gp_data')
}

nstat = max(station_info$index)
nweek = 53
lin_combs = get_linear_combinations(nweek, nstat)

sdRef = 0.5
sdRefProb = 0.1
hyper_rw_prec = list(prec = list(prior = 'pc.prec', param = c(sdRef, sdRefProb), initial = 2.5))
hyper_iid_prec = list(prec = list(prior = 'pc.prec', param = c(0.5, 0.1), initial = 4))

hyper_matern = list(range = list(prior = 'pc.range', param = c(100, 0.1), initial = 2.6),
                    prec = list (prior = 'pc.prec', param = c(0.5, 0.1), initial = 4.2))

# form = y ~ f(week_rw, model = 'rw2', hyper = hyper_rw_prec, cyclic = T, scale.model = T, constr = T) +
#   f(week_iid, model = 'iid', hyper = hyper_iid_prec, constr = T) +
#   f(index, model = 'dmatern', locations = locations, hyper = hyper_matern, constr = T) +
#   offset(log_mean)

form = y ~ 
  f(week_rw, 
    model = 'rw2', 
    hyper = hyper_rw_prec, 
    cyclic = T, 
    scale.model = T, 
    constr = T) +
  f(week_iid, 
    model = 'iid', 
    hyper = hyper_iid_prec, 
    constr = T) +
  f(index, 
    model = 'dmatern', 
    locations = locations, 
    hyper = hyper_matern, 
    constr = T) +
  offset(log_mean)

result_gp = inla(form,
                data = gp_data,
                family = 'gp',
                control.family = list(control.link = list(quantile = 0.5),
                                      hyper = list(xi = list(prior = 'pc.gevtail', 
                                                             param = c(4.5, 0, 0.99)))),
                lincomb = lin_combs, 
                control.compute=list(openmp.strategy="pardiso.parallel"),
                num.threads = 20,
                verbose = T)

save(result_gp, file = 'files/result_gp_temp')


