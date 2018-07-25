# general visualisation
library('ggplot2') # visualisation
library('scales') # visualisation
library('grid') # visualisation
library('gridExtra') # visualisation
library('RColorBrewer') # visualisation
library('corrplot') # visualisation

# general data manipulation
library('dplyr') # data manipulation
library('readr') # input/output
library('data.table') # data manipulation
library('tibble') # data wrangling
library('tidyr') # data wrangling
library('stringr') # string manipulation
library('forcats') # factor manipulation

# specific visualisation
library('ggfortify') # visualisation
library('ggrepel') # visualisation
library('ggridges') # visualisation
library('ggExtra') # visualisation

# specific data manipulation
library('lazyeval') # data wrangling
library('broom') # data wrangling
library('purrr') # string manipulation

# Date plus forecast
library('lubridate') # date and time
library('timeDate') # date and time
library('tseries') # time series analysis
library('forecast') # time series analysis
library('prophet') # time series analysis

# Maps / geospatial
library('geosphere') # geospatial locations
library('leaflet') # maps
library('leaflet.extras') # maps
library('maps') # maps


#Loading data 

air_visits <- as.tibble(fread('input/air_visit_data.csv'))
air_reserve <- as.tibble(fread('input/air_reserve.csv'))
hpg_reserve <- as.tibble(fread('input/hpg_reserve.csv'))
air_store <- as.tibble(fread('input/air_store_info.csv'))
hpg_store <- as.tibble(fread('input/hpg_store_info.csv'))
holidays <- as.tibble(fread('input/date_info.csv'))
store_ids <- as.tibble(fread('input/store_id_relation.csv'))
test <- as.tibble(fread('input/sample_submission.csv'))

#Individual variable analysis 
#air_visits-Summary statistics 
summary(air_visits)
glimpse(air_visits)
air_visits %>% distinct(air_store_id) %>% nrow()

#air_reserve-Summary statistics 
summary(air_reserve)
glimpse(air_reserve)
air_reserve%>% distinct(air_store_id) %>% nrow()

#hog_reserve-Summary statistics 
summary(hpg_reserve)
glimpse(hpg_reserve)
hpg_reserve%>% distinct(hpg_store_id) %>% nrow()

#air_store_info-Summary statistics 
summary(air_store)
glimpse(air_store)

#hpg_store_info-Summary statistics 
summary(hpg_store)
glimpse(hpg_store)

#Holidays -Summary statistics 
summary(holidays)
glimpse(holidays)

#store_id_relation -Summary statistics 
summary(store_ids)
glimpse(store_ids)

#test -Summary statistics 
summary(test)
glimpse(test)

#Missing values 


