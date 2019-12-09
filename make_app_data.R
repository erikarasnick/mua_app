library(tidyverse)
library(sf)
library(tigris)
options(tigris_class ='sf')
options(tigris_use_cache = TRUE)

# MUA area shapefile
temp <- tempfile()
download.file('https://data.hrsa.gov//DataDownload/DD_Files/MUA_SHP.zip', temp)
unzip(temp, exdir = './MUA_SHP/')
d <- read_sf('MUA_SHP/MUA_SHP_DET_CUR_VX.shp')
unlink(temp)
unlink('./MUA_SHP/', recursive = TRUE)
d <- sf::st_transform(d, 5072)

d %>%
  select(rural_status = RurStatDes) %>%
  saveRDS('mua_shp_5072.rds')

# Rural area shapefile
temp <- tempfile()
download.file('https://www.hrsa.gov/sites/default/files/hrsa/ruralhealth/aboutus/definition/nonmetrocountiesandcts2016.xlsx', temp)
d_rural <- readxl::read_excel(temp)
unlink(temp)

### if CT is blank, the whole county is rural
rural_counties <- d_rural %>% filter(is.na(CT))
counties <- tigris::counties(cb=TRUE)
rural_counties <- right_join(counties, rural_counties, by=c('GEOID' = 'CTY FIPS')) %>% 
  select(STATEFP, COUNTYFP, GEOID, NAME, CountyName)

### if CT is not blank, tracts within a metro county are considered rural
rural_tracts <- d_rural %>% filter(!is.na(CT))
tracts <- tigris::tracts(unique(rural_tracts$ST)[1], cb=TRUE)
for(i in 2:50){
  tracts1 <- tigris::tracts(unique(rural_tracts$ST)[i], cb=TRUE)
  tracts <- rbind(tracts, tracts1)
}
rural_tracts <- right_join(tracts, rural_tracts, by=c('GEOID' = 'CT')) %>% 
  select(STATEFP, COUNTYFP, GEOID, NAME, CountyName)

rural_counties_tracts <- rbind(rural_counties, rural_tracts)
all_rural <- st_union(rural_counties_tracts) %>% 
  st_as_sf()
  st_transform(5072) %>% 
  saveRDS('rural_shp_5072.rds')
