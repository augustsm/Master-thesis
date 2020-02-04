library(tidyverse)
library(lubridate)
library(INLA)

source('code/utils.R')

raw_data = read_raw_data()
data = remove_bad_data(raw_data)
station_info = read_station_information(data)

locations = station_info[c('X', 'Y')]
locations = as.data.frame(latlon_to_laea(as.matrix(locations))@coords)
locations_rescaled = locations %>% mutate(x = (X-min(X))/(max(X)-min(X))) %>%
  mutate(y = (Y-min(Y))/(max(X)-min(X)))
locations_rescaled[c('X', 'Y')] = NULL
station_info[c('X', 'Y')] = NULL
station_info$index = 1:(dim(station_info)[1])

for(i in 2009:2019){
gamma_data =  data %>% gather(ID, prcp, -time) %>%
  filter(!is.na(prcp), prcp > 0) %>%
  full_join(.,station_info) %>%
  filter(year(time)==i) %>%
  mutate(week = floor(yday(time)/7))

gamma_data[c('time', 'ID', 'masl')] = NULL
# test_data = data.frame('prcp'=rep(NA, 53*3), 'week'=rep(1:53, 3), 'index'= rep(c(1,4,10), each=53))
# gamma_data = rbind(gamma_data, test_data)

sdRef = 0.001
sdRefProb = 0.1
hyper_rw_prec = list(prec = list(prior = 'pc.prec', param = c(sdRef, sdRefProb)))

hyper_matern = list(range = list(prior = 'pc.range', param = c(1, 0.1)))

form = prcp ~ f(week, model = 'rw2', hyper = hyper_rw_prec, cyclic = T, scale.model = T, constr = T)+
  f(index, model = 'dmatern', locations = as.matrix(locations_rescaled), hyper = hyper_matern, constr = T)

cat('Fit gamma stage \n')
result_gamma = inla(formula = form,
                    family = 'gamma',
                    data = as.data.frame(gamma_data),#[rep(c(T, rep(F,10)),60000),],
                    control.family = list(hyper = list(prec = list(prior = 'loggamma', 
                                                                   param = c(1, 0.01)))),
                    # num.threads = 10,
                    # blas.num.threads = 10,
                    # control.compute=list(openmp.strategy="pardiso.parallel"),
                    verbose = T)

print(ggplot(data = as.data.frame(locations), aes(x=X, y=Y)) +
  geom_point(aes(col=result_gamma$summary.random$index$mean)) +
  labs(col = 'Spatial effect') +
  ggtitle(as.character(i)))

plot(result_gamma)
}
