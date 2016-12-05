#install.packages("install.load")
library(install.load)
install_load("dplyr")
install_load("stringr")
install_load("lubridate")
install_load("RMySQL")
install_load("readr")
install_load("ggplot2")
install_load("scales")
install_load("lubridate")
install_load("eeptools")
install_load("ggmap")
library(DBI)

conDplyr = src_mysql(dbname = "openmrs", user = "root", password = "", host = "localhost")

person <- conDplyr %>% 
  tbl("person") %>% 
  filter(voided == 0) %>% 
  collect(n=Inf)

person_address <- conDplyr %>% 
  tbl("person_address") %>% 
  select(person_id, postal_code, latitude, longitude, city_village, state_province, country, voided) %>% 
  rename(add_personId = person_id) %>% 
  filter(voided == 0) %>% 
  collect(n=Inf) %>% 
  inner_join(person,by=c("add_personId"="person_id")) %>% 
  mutate(date_created = ymd_hms(date_created)) %>% 
  filter(year(date_created) >=2015 , month(date_created) >=3)

write_csv(person_address,"bayalpata_pat_map.csv")

qmap(location = "Achham District", zoom = 11)
