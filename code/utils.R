library(lubridate)
library(sp)

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

read_station_information = function(data) {
  station_info = read_csv(file = 'data/stations.csv', 
                          col_types = 'ccdcccddTTcc')
  station_info = station_info[(station_info$ID %in% colnames(data)),]
  station_info = station_info[c('ID', 'masl', 'X', 'Y')]
  station_info
}

latlon_to_laea = function(coord){
  proj_latlon = CRS("+proj=longlat +datum=WGS84")
  
  proj_laea = CRS("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y0=3210000 +ellps=GRS80 +units=m +no_defs")
  
  latlon = SpatialPoints(coord, proj4string = proj_latlon)
  coord_1 = spTransform(latlon, CRSobj = proj_laea)
  coord_1
}
