devtools::install_github("trestletech/rydn")
library(rydn)
options(RYDN_KEY="yourAPIkey", RYDN_SECRET="yourSecret")
b2 <- find_place("Sanfebagar municipality, Achham, Nepal")  
b2[c("latitude", "longitude")]


devtools::install_github("hrbrmstr/nominatim")
library(nominatim)
b1 <- osm_geocode("Berlin, Germany")
b1[c("lat", "lon")]

library(geonames)


cities <- c("Gereshk", "Lahkar Gah", "Lashkar Gah", "Marjah", "Nad-e Ali")  

GNsearchAF <- function(x) {  
  res <- GNsearch(name=x, country="AF",username="adminName1")  
  return(res[1, ])  
}

# loop over city names and reformat  
GNresult <- sapply(cities, GNsearchAF)  
GNresult <- do.call("rbind", GNresult)  
GNresult <- cbind(city=row.names(GNresult),  
                  subset(GNresult, select=c("lng", "lat", "adminName1")))  

tmp <- round(geocode("Sanfebagar Nepal"), 4)
geocode("Baradadivi Nepal")
tmp1 <- as.numeric( geocode("Kailashmandau, Bājura"))

df <- data.frame(
  address = c("1600 Pennsylvania Avenue, Washington DC", "", "houston texas"),
  stringsAsFactors = FALSE
)

df %>% mutate_geocode(address)
