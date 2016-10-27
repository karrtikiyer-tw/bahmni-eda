#con <- dbConnect(RMySQL::MySQL(), dbname = "openmrs", host = "localhost", port = 3306, user = "root", password="")
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
library(DBI)

conDplyr = src_mysql(dbname = "openmrs", user = "root", password = "", host = "localhost")


person <- conDplyr %>% 
          tbl("person") %>% 
          filter(voided == 0) 
 

obs <- conDplyr %>% 
        tbl("obs") %>% 
        filter(!(value_coded %in% c(1L,2L,17L,18L,20L,21L))) #other items in obs group which are not of our interest as of such as true/false, primary/presumed, confirmed, secondary, etc..

obsgroup <- conDplyr %>% 
            tbl("obs") %>% 
            filter(concept_id %in% c(13L,15L,16L)) %>% 
            select(-value_coded) #obs_group concept ID of visit diagnosis, Coded Diagnosis and Diagnosis Certainity against which diagnosis are entered in possible DB

conceptnames <- conDplyr %>% 
                tbl("concept_name") %>% 
                filter(concept_name_type=="FULLY_SPECIFIED", voided==0, locale == "en")

person_address <- conDplyr %>% 
                  tbl("person_address") %>% 
                  select(person_id, city_village) %>% 
                  rename(add_personId = person_id) 

person_age <- conDplyr %>% 
              tbl("person") %>% 
              filter(voided == 0) %>% 
              select(person_id, birthdate, birthdate_estimated) %>% 
              rename(age_personId = person_id)

pat_diag_db.df <- person %>% 
                  inner_join(obs, by = c("person_id"="person_id")) %>% 
                  inner_join(obsgroup, by= c("obs_group_id" = "obs_id")) %>% 
                  group_by(person_id.x, gender, value_coded) %>% 
                  summarise(count = n()) %>% 
                  left_join(conceptnames, by = c("value_coded"="concept_id")) %>% 
                  select(person_id.x, gender, value_coded, name, count) %>% 
                  inner_join(person_address, by = c("person_id.x"="add_personId")) %>% 
                  inner_join(person_age, by = c("person_id.x"= "age_personId")) %>% 
                  arrange(desc(count)) %>% 
                  collect(n = Inf) 

#Create a CSV below so that we can categorize them as chronic or non chronic
write_csv(as.data.frame(unique(pat_diag_db.df$name)),"diagnosis_master.csv")


pat_diag.df <- tbl_df(pat_diag_db.df) %>% 
              select(-age_personId, -add_personId, -birthdate_estimated) %>% 
              mutate(gender = as.factor(gender),
                     name = as.factor(name),
                     birthdate = ymd(birthdate),
                     age = round(age_calc(birthdate, units="years"),digits = 1)) %>% 
              rename(person_id = person_id.x,
                     diagnosis_id = value_coded,
                     diagnosis = name)

pat_diag.top_10.df <- pat_diag.df %>% 
                      group_by(diagnosis) %>% 
                      summarise(diag_count = n()) %>% 
                      arrange(diagnosis, desc(diag_count)) %>% 
                      mutate(inv_rank = dense_rank(desc(diag_count))) %>% 
                      top_n(10, wt = diag_count) %>% 
                      arrange(inv_rank)

pat_diag.top_10.bysex.df <- pat_diag.df %>% 
                            inner_join(pat_diag.top_10.df, by = c("diagnosis"="diagnosis")) %>% 
                            group_by(diagnosis, gender) %>% 
                            summarise(diag_sex_count = n()) %>% 
                            arrange(diagnosis, desc(diag_sex_count))

g2 <- ggplot(pat_diag.top_10.bysex.df, aes(x = reorder(diagnosis, -diag_sex_count)))
g2 <- g2 + geom_bar(aes(weight = diag_sex_count, fill = gender))
g2 <-
  g2 + labs(title = "Diagnosis vs Count", x = "Diagnosis", y =
              "Count")
g2 <- g2 + scale_y_continuous(labels = comma)
g2 <- g2 + theme(axis.text.x = element_text(angle = 90, hjust = 1))
print(g2)

#no dead patients in the DB in person table

pat_diag.top_3.byregion.df <- pat_diag.df %>% 
                              inner_join(pat_diag.top_10.df, by = c("diagnosis"="diagnosis")) %>% 
                              group_by(diagnosis, city_village) %>% 
                              summarise(diag_region_count = n()) %>% 
                              arrange(diagnosis, desc(diag_region_count)) %>% 
                              mutate(reg_rank = dense_rank(desc(diag_region_count))) %>% 
                              top_n(5, wt = -reg_rank) %>% 
                              arrange(reg_rank)

pat_diag.top_10.byregion.df <- pat_diag.df %>% 
                                inner_join(pat_diag.top_3.byregion.df, by = c("diagnosis"="diagnosis", "city_village"="city_village")) %>% 
                                group_by(diagnosis, gender, city_village) %>% 
                                summarise(diag_sex_region_count = n()) %>% 
                                mutate(reg_rank = dense_rank(desc(diag_sex_region_count))) %>% 
                                arrange(reg_rank)

#region distribution for gastritis
g3 <- ggplot(pat_diag.top_10.byregion.df %>% filter(diagnosis=="Gastritis"), 
             aes(x = reorder(city_village, -diag_sex_region_count)))
g3 <- g3 + geom_bar(aes(weight = diag_sex_region_count, fill = gender))
g3 <-
  g3 + labs(title = "Gastritis Region vs Count", x = "Gastritis Region", y =
              "Count")
g3 <- g3 + scale_y_continuous(labels = comma)
g3 <- g3 + theme(axis.text.x = element_text(angle = 90, hjust = 1))
print(g3)


#age analysis for gastritis
#boxplot
age_df <- pat_diag.df %>% 
  filter(diagnosis=="Gastritis") %>% 
  inner_join(pat_diag.top_3.byregion.df, by = c("diagnosis"="diagnosis","city_village"= "city_village")) %>% 
  select(-diag_region_count, -reg_rank) 

gg <- ggplot(age_df, aes(x=city_village, y=age))
gg <- gg + geom_boxplot()
gg <- gg + facet_wrap(~gender)
gg <- gg + labs(x="")
gg <- gg + theme_bw()
gg <- gg + theme(strip.background=element_rect(fill="black"))
gg <- gg + theme(strip.text=element_text(color="white", face="bold")) + coord_flip()
print(gg)
#histogram
gg1 <- ggplot(age_df, aes(age, fill=gender)) + 
  geom_histogram(bins = 20,position="dodge") + 
  scale_x_continuous(labels = comma) + facet_wrap(~city_village)
gg1 <- gg1 + labs(x="Age" ,y="Count",title="Gastritis Histogram")
print(gg1)

#year wise and month wise analysis for gastritis
obs_gastritis_year <- conDplyr %>% 
                      tbl("obs") %>% 
                      filter(value_coded==5660, voided==0) %>% 
                      mutate(obs_year = year(obs_datetime),
                             obs_month = month(obs_datetime)) %>% 
                      collect(n=Inf) %>% 
                      mutate(obs_year = factor(obs_year, ordered = T),
                             obs_month = factor(obs_month, ordered = T)) %>% 
                      group_by(obs_month, obs_year) %>% 
                      summarise(count = n())
#bar chart
g4 <- ggplot(obs_gastritis_year,
             aes(x = obs_month))
g4 <- g4 + geom_bar(aes(weight = count))
g4 <-
  g4 + labs(title = "Gastritis Trend", x = "Gastritis Month", y =
              "Count")
g4 <- g4 + scale_y_continuous(labels = comma) + facet_wrap(~obs_year)
print(g4)


dbDisconnect(conDplyr$con)
