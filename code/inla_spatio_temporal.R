library(tidyverse)
library(lubridate)
library(INLA)

# get utility functions
source('code/utils.R')

# read data
raw_data = read_raw_data()
data = remove_bad_data(raw_data)
station_info = read_station_information(data)

# extract station location info
locations = station_info[c('X', 'Y')]
locations = latlon_to_laea(as.matrix(locations))@coords
station_info[c('X', 'Y')] = NULL

# set station index
station_info$index = 1:(dim(station_info)[1])

# choose years to fit model for
years = c(2014)

for(i in years){

# prepare data for fit
gamma_data =  data %>% gather(ID, prcp, -time) %>%
  filter(!is.na(prcp), prcp > 0) %>%
  full_join(.,station_info) %>%
  filter(year(time)==i) %>%
  mutate(week = floor(yday(time)/7))

gamma_data[c('time', 'ID', 'masl')] = NULL

# define priors
sdRef = 0.001
sdRefProb = 0.1
hyper_rw_prec = list(prec = list(prior = 'pc.prec', param = c(sdRef, sdRefProb)))

hyper_matern = list(range = list(prior = 'pc.range', param = c(40000, 0.1)))

# define formula object
form = prcp ~ f(week, model = 'rw2', hyper = hyper_rw_prec, cyclic = T, scale.model = T, constr = T)+
  f(index, model = 'dmatern', locations = locations, hyper = hyper_matern, constr = T)

# fit gamma stage
cat('Fit gamma stage for', i, '\n')
result_gamma = inla(formula = form,
                    family = 'gamma',
                    data = as.data.frame(gamma_data),
                    control.family = list(hyper = list(prec = list(prior = 'loggamma', 
                                                                   param = c(1, 0.01)))),
                    num.threads = 10,
                    control.compute=list(openmp.strategy="pardiso.parallel"),
                    verbose = T)

# plot results
plot(result_gamma)

print(ggplot(data = as.data.frame(locations), aes(x=X, y=Y)) +
  geom_point(aes(col=result_gamma$summary.random$index$mean)) +
  labs(col = 'Spatial effect') +
  ggtitle(as.character(i)))
}
