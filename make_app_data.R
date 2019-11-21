library(tidyverse)
library(sf)

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
