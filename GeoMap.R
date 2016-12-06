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

person_address <- person_address %>% 
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
                              )


                        

write_csv(person_address_group, "distinct_city_villages_since_Mar_2015.csv")

write_csv(person_address,"bayalpata_pat_map.csv")

person_address_final <- person_address %>% 
  inner_join(person_address_group, by=c("city_village"="city_village")) %>% 
  select(-count)

achham_map_g_str <- get_map(location = "Achham, Nepal", maptype="terrain",zoom = 13)

gmap1 <- ggmap(achham_map_g_str, extent = "device") 
gmap1 <- gmap1 + geom_density2d(data = person_address_final, 
                                aes(x = lon, y = lat), size = 0.3) 
gmap1 <- gmap1 + stat_density2d(data = person_address_final,
                                aes(x = lon, y = lat, fill = ..level.., alpha = ..level..), 
                                size = 0.01, 
                                bins = 16, geom = "polygon") 
gmap1 <- gmap1 + scale_fill_gradient(low = "green", high = "red") 
gmap1 <- gmap1 + scale_alpha(range = c(0, 0.3), guide = FALSE)
gmap1
bb <- attr(achham_map_g_str, "bb")
bbox <- bb2bbox(bb)

# use the bounding box to get a stamen map
#stamMap <- get_stamenmap(bbox)
#mymap <- get_map(location = c(28.747002,81.921352,29.461203,80.818646), source = "osm")
mymap <- get_map(location = bbox, source = "osm", maptype = "terrain-labels" ,zoom = 13)

gmap1.1 <- ggmap(mymap, extent = "device") 
gmap1.1 <- gmap1.1 + geom_density2d(data = person_address_final, 
                                aes(x = lon, y = lat), size = 0.3) 
gmap1.1 <- gmap1.1 + stat_density2d(data = person_address_final,
                                aes(x = lon, y = lat, fill = ..level.., alpha = ..level..), 
                                size = 0.01, 
                                bins = 16, geom = "polygon") 
gmap1.1 <- gmap1.1 + scale_fill_gradient(low = "green", high = "red") 
gmap1.1 <- gmap1.1 + scale_alpha(range = c(0, 0.3), guide = FALSE)
gmap1.1


achham_map_g_str_sat <- get_map(location = "Achham, Nepal", maptype = "satellite", zoom = 13)
gmap2 <- ggmap(achham_map_g_str_sat, extent = "device") 
gmap2 <- gmap2 + geom_point(aes(x = lon, y = lat), colour = "red", 
                            alpha = 0.1, size = 2, data = person_address_final)
gmap2

mymap2 <- get_map(location = bbox, source = "osm", maptype = "terrain", zoom = 13)
gmap2.1 <- ggmap(mymap2, extent = "device") 
gmap2.1 <- gmap2.1 + geom_point(aes(x = lon, y = lat), colour = "red", 
                            alpha = 0.1, size = 2, data = person_address_final)
gmap2.1
#qmap(location = "Achham District", zoom = 11)
