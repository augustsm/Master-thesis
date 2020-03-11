library(INLA)

source('code/utils.R')
source('code/data_preparation.R')

fit_gp_stage = function(p = NULL, load_bool = TRUE, initial = NULL){
  
# set default initial values and update according to input
init = list(xi = -2.2, rw = 2.5, iid = 4, mp = 4.2, mr = 2.6)
for(name in names(initial)){
  init[[name]] = initial[[name]]
}
  
  
data = read_data()
station_info = read_station_information(data)
locations = get_locations(station_info = station_info)

if(!is.null(p)){
  load_bool = FALSE
}
if(load_bool){
  load('files/gp_data')
}else{
  gp_data = prepare_gp_data(station_info, temp = TRUE, p = p)
  save(gp_data, file = 'files/gp_data')
}

nstat = max(station_info$index)
nweek = 53
lin_combs = get_linear_combinations(nweek, nstat)

sdRef = 0.5
sdRefProb = 0.1
hyper_rw_prec = list(prec = list(prior = 'pc.prec', param = c(sdRef, sdRefProb), initial = init$rw))
hyper_iid_prec = list(prec = list(prior = 'pc.prec', param = c(0.5, 0.1), initial = init$iid))

hyper_matern = list(prec = list (prior = 'pc.prec', param = c(0.5, 0.1), initial = init$mp),
                    range = list(prior = 'pc.range', param = c(100, 0.1), initial = init$mr))

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
                                                             param = c(4.5, 0, 0.99),
                                                             initial = init$xi))),
                lincomb = lin_combs, 
                control.compute=list(openmp.strategy="pardiso.parallel"),
                num.threads = 20,
                verbose = T)

save(result_gp, file = 'files/result_gp_temp')
}

