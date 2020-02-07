library(tidyverse)
library(INLA)

source('code/utils.R')

raw_data = read_raw_data()
data = remove_bad_data(raw_data)
station_info = read_station_information(data)

station_info$index = 1:dim(station_info)[1]
locations = station_info[c('X', 'Y')]
locations = as.data.frame(latlon_to_laea(locations)@coords)
locations = locations %>% mutate(x = (X-min(X))/(max(X)-min(X)), y = (Y-min(Y))/(max(X)-min(X)))
locations_rescaled = locations[c('x', 'y')]
station_info[c('X', 'Y', 'masl')] = NULL

monthly_max_values = data %>%
  gather(ID, prcp, -time) %>%
  filter(prcp > 0) %>%
  mutate(month = month(time), year= year(time)) %>%
  group_by(ID, year, month) %>%
  summarise(max = max(prcp)) %>%
  full_join(.,station_info) %>%
  filter(!is.na(max))

monthly_max_values[c('ID', 'year')] = NULL

prediction_data = data.frame(max = rep(NA, 12*157), 
                             month = rep(1:12, each = 157), 
                             index = rep(1:157, 12))
monthly_max_values = rbind(as.data.frame(monthly_max_values), prediction_data)

# define priors
hyper_matern = list(range = list(prior = 'pc.range', param = c(0.2, 0.1)))
hyper_gev = list(tail = list(prior="pc.gevtail", param=c(7, 0, 0.99)))
sdRef = 0.5
sdRefProb = 0.1
hyper_rw_prec = list(prec = list(prior = 'pc.prec', param = c(sdRef, sdRefProb)))


# define formula object
form = max ~ f(index, model = 'dmatern', locations = as.matrix(locations_rescaled), hyper = hyper_matern, constr = T) +
  f(month, model = 'rw2', hyper = hyper_rw_prec, constr = T, cyclic = T, scale.model = T)

result_gev = inla(data = monthly_max_values,
                  family = 'gev',
                  formula = form,
                  control.family = list(hyper = hyper_gev),
                  control.predictor = list(compute = T, link = 1),
                  control.fixed = list(prec.intercept = 1),
                  verbose = T)



# results = list()
# for(i in 1:12){
#   gev_data = monthly_max_values %>%
#     filter(month == i)
#   
#   gev_data[c('month', 'year', 'ID')] = NULL
#   
#   results[[i]] = inla(formula = form,
#                     data = gev_data,
#                     family = 'gev',
#                     scale = 1/900,
#                     control.family = list(hyper = hyper_gev),
#                     control.predictor = list(compute = T, link = 1),
#                     )#verbose = T)
#   plot(results[[i]])
#   print(ggplot(data = NULL, aes(x = 1:dim(gev_data)[1])) +
#     geom_line(aes(y = results[[i]]$summary.linear.predictor$mean), col = 'red') +
#     geom_line(aes(y = gev_data$max), col = 'blue') +
#     geom_ribbon(aes(min = results[[i]]$summary.linear.predictor$`0.025quant`,
#                     max = results[[i]]$summary.linear.predictor$`0.975quant`), alpha = 0.3) +
#     ggtitle(as.character(i)))
# }
# 
# ggplot(data = locations, aes(x=X,y=Y)) +
#   geom_point(aes(col = sapply(station_info$index, function(x) mean(results[[12]]$summary.linear.predictor$mean[gev_data$index==x])))) +
#   labs(col = 'eta')
#              


ggplot(data = NULL, aes(x = 1:dim(monthly_max_values)[1])) +
  geom_line(aes(y = result_gev$summary.linear.predictor$mean/3), col = 'red') +
  #geom_line(aes(y = monthly_max_values$max), col = 'blue') +
  geom_ribbon(aes(min = result_gev$summary.linear.predictor$`0.025quant`/3,
                  max = result_gev$summary.linear.predictor$`0.975quant`/3), alpha = 0.3)

ggplot(data = result_gev$summary.random$index[1:50,], aes(x=ID, y=mean)) +
  geom_line(col = 'red') +
  geom_ribbon(aes(min = `0.025quant`, max = `0.975quant`), alpha = 0.3)

ggplot(data = locations_rescaled, aes(x=x, y=y)) +
  geom_point(aes(col = result_gev$summary.random$index$mean)) +
  labs(col = 'Mean effect')

ggplot(data = NULL, aes(x = 1:(157), y = tail(result_gev$summary.linear.predictor$mean, 157)))+
  geom_line(col = 'red')
