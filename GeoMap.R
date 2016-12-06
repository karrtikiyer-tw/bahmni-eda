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

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}
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
  mutate(date_created = ymd_hms(date_created),
         city_village = sapply(city_village, simpleCap)) %>% 
  filter(year(date_created) >=2015 , month(date_created) >=3)

person_address_group <- as.data.frame(person_address %>% 
                        group_by(city_village) %>% 
                        summarise(count = n()) %>% 
                        arrange(desc(count)) %>% 
                        filter(count >=10) %>% 
                        mutate(city_village = 
                                 case_when(
                                          (.$city_village == "Baradadevi") ~ "Baradadivi",
                                          (.$city_village == "Batulasain") ~ "Batulasen",
                                          (.$city_village == "Chaphamandaun") ~ "Chaphamandau",
                                          (.$city_village == "Mastabandali") ~ "Mashtanamdali",
                                          (.$city_village == "Mastamandaun") ~ "Mastamandau",
                                          (.$city_village == "Hattikot") ~ "Hatikot",
                                          (.$city_village == "Ghodasain") ~ "Dhodasain",
                                          (.$city_village == "Kamalbazar Municipality") ~ "Kamal Bazar",
                                          (.$city_village == "Attariya Municipality") ~ "Attariya",
                                          TRUE ~ .$city_village
                                          )
                              ) %>% 
                        mutate(address = paste(city_village, " ", "Nepal"))) %>% 
                        mutate_geocode(address) %>% 
                        na.omit()


                        

write_csv(person_address_group, "distinct_city_villages_since_Mar_2015.csv")

write_csv(person_address,"bayalpata_pat_map.csv")



#qmap(location = "Achham District", zoom = 11)
