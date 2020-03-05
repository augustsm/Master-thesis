library(lubridate)
library(sp)
library(tidyverse)
library(INLA)

remove_bad_data = function(data){
  time = data$time
  data$SN17640[time<ymd(20190909)] = NA
  data$SN17875[time > ymd(20190810) & time < ymd(20190813)] = NA
  data$SN18265 = NULL
  data$SN18690 = NULL
  data$SN19490[time<ymd(20140201)] = NA
  data$SN19660[time>ymd(20190212)] = NA
  data[c('SN27470', 'SN30275', 'SN30278', 'SN30282', 'SN30285', 'SN30288', 'SN30293', 'SN19820', 'SN30325', 'SN30340', 'SN30350', 'SN3190')] = NULL
  data$SN4455[time>ymd(20190910)] = NA
  data$SN4781 = NULL
  data$SN4825[time>ymd(20190330)] = NA
  data$SN30320[year(time) == 2010] = NA
  data
}

read_raw_data = function() {
  raw_data = read_csv(file = 'data/data_2009-2019.csv', 
                      col_types = paste(c('T', rep('d', 172)), collapse = ''))
  raw_data
}

read_data = function(){
  raw_data = read_raw_data()
  data = remove_bad_data(raw_data)
  data
}

read_station_information = function(data) {
  station_info = read_csv(file = 'data/stations.csv', 
                          col_types = 'ccdcccddTTcc')
  station_info = station_info[(station_info$ID %in% colnames(data)),]
  station_info = station_info[c('ID', 'masl', 'X', 'Y')]
  station_info$index = 1:(dim(station_info)[1])
  station_info
}

latlon_to_laea = function(coord){
  proj_latlon = CRS("+proj=longlat +datum=WGS84")
  
  proj_laea = CRS("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y0=3210000 +ellps=GRS80 +units=m +no_defs")
  
  latlon = SpatialPoints(coord, proj4string = proj_latlon)
  coord_1 = spTransform(latlon, CRSobj = proj_laea)
  coord_1
}

get_locations = function(data = NULL, station_info = NULL){
  if(is.null(station_info)){
    if(is.null(data)){
      data = read_data()
    }
    station_info = read_station_information(data)
  }
  locations = latlon_to_laea(station_info[c('X', 'Y')])@coords
  locations = locations/1000
  locations[,1] = locations[,1]-min(locations[,1])
  locations[,2] = locations[,2]-min(locations[,2])
  locations
}

station_to_index = function(station, station_info) {
  index = station_info$index[station_info$ID == station]
}

index_to_station = function(index, station_info){
  station = station_info$ID[station_info$index == index]
}

get_close_stations = function(station, station_info, locations){
  x = locations[,1][station_info$ID==station]
  y = locations[,2][station_info$ID==station]
  station_info = station_info %>% mutate(d = sqrt((x-locations[,1])^2+(y-locations[,2])^2)) %>%
    arrange(., d)
  as.data.frame(station_info)[1:5,]
}

get_linear_combinations = function(n_weeks, n_stations){
  lin_combs = vector(length = n_stations*n_weeks)
  for(i in 1:n_stations){
    for(j in 1:n_weeks){
      stat_vec = rep(NA, n_stations*n_weeks)
      stat_vec[(j-1)*n_stations + i] = 1
      week_vec = rep(NA, n_weeks)
      week_vec[j] = 1
      lin = inla.make.lincomb('(Intercept)' = 1, 'week_rw' = week_vec, 'index' = stat_vec)
      names(lin) = paste(c('lc', as.character((i-1)*n_weeks + j)), collapse = '')
      lin_combs[(i-1)*n_weeks + j] = lin
    }
  }
  
  lin_combs
}

extract_linear_combinations = function(result, n_weeks = 53, station = NULL, station_info = NULL){
  if(is.null(station)){
    mean = result$summary.lincomb.derived$mean
    lower = result$summary.lincomb.derived$`0.025quant`
    upper = result$summary.lincomb.derived$`0.975quant`
  }
  else{
    if(is.null(station_info)){
      stop('Station info not passed as input.')
    }
    index = station_to_index(station, station_info)
    mean = result$summary.lincomb.derived$mean[((index-1)*n_weeks + 1):(index*n_weeks)]
    lower = result$summary.lincomb.derived$`0.025quant`[((index-1)*n_weeks + 1):(index*n_weeks)]
    upper = result$summary.lincomb.derived$`0.975quant`[((index-1)*n_weeks + 1):(index*n_weeks)]
  }
  list(mean = mean, lower = lower, upper = upper)
}

update_data = function(gamma = FALSE, binom = FALSE, gp = FALSE){
  if(gamma){
    load('files/result_gamma_temp')
    save(result_gamma, file = 'files/result_gamma')
  }
  if(binom){
    load('files/result_binom_temp')
    save(result_binom, file = 'files/result_binom')
  }
  if(gp){
    load('files/result_gp_temp')
    save(result_gp, file = 'files/result_gp')
  }
}

qgamma_mean_prec = function(p, x, prec){
  qgamma(p, shape = x^2*prec, scale = 1/(prec*x))
}

logit = function(x){
  1/(1+exp(-x))
}


