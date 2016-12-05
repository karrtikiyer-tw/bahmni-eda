#install.packages("install.load")
library(install.load)
install_load("dplyr")
install_load("stringr")
install_load("lubridate")
install_load("RMySQL")
install_load("readr")
install_load("ggplot2")̄
install_load("scales")
install_load("lubridate")
install_load("eeptools")
library(DBI)

conDplyr <- src_mysql(dbname = "openmrs", user = "root", password = "", host = "localhost")


person <- conDplyr %>% 
  tbl("person") %>% 
  filter(voided == 0) 

#diagnosis class type value coded obs
concepts <- conDplyr %>% 
                  tbl("concept") %>% 
                  select(concept_id, class_id) %>% 
                  rename(conceptid = concept_id)
#diagnosis class id 4
obs <- conDplyr %>% 
        tbl("obs") %>% 
        inner_join(concepts, by=c("value_coded"="conceptid")) %>% 
        filter(class_id==4)

conceptnames <- conDplyr %>% 
  tbl("concept_name") %>% 
  filter(concept_name_type=="FULLY_SPECIFIED", voided==0, locale == "en") %>% 
  select(name, concept_id)

person_address <- conDplyr %>% 
  tbl("person_address") %>% 
  select(person_id, city_village) %>% 
  rename(add_personId = person_id) 

person_age <- conDplyr %>% 
  tbl("person") %>% 
  filter(voided == 0) %>% 
  select(person_id, birthdate, birthdate_estimated) %>% 
  rename(age_personId = person_id)

patients <- conDplyr %>% 
  tbl("patient") %>% 
  filter(voided ==0)

#patient trend across different regions/villages
pat_across_region <- patients %>% 
                    inner_join(person_address, by =c("patient_id"="add_personId")) %>% 
                    group_by(city_village) %>% 
                    summarise(Count = n()) %>% 
                    collect(n=Inf) %>% 
                    arrange(city_village, desc(Count)) %>% 
                    mutate(inv_rank = dense_rank(desc(Count))) %>% 
                    top_n(10, wt = Count) %>% 
                    arrange(inv_rank)

pat_across_region_gender<- patients %>% 
                          inner_join(person_address, by =c("patient_id"="add_personId")) %>% 
                          inner_join(person, by =c("patient_id"="person_id")) %>% 
                          group_by(city_village, gender) %>% 
                          summarise(PatCount = n()) %>% 
                          collect(n=Inf) %>% 
                          inner_join(pat_across_region, by=c("city_village"="city_village")) %>% 
                          mutate(Percentage = round((PatCount/Count)*100,2)) %>% 
                          select(city_village, gender, PatCount, Percentage)

g1 <- ggplot(pat_across_region_gender, aes(x = reorder(city_village, -PatCount)))
g1 <- g1 + geom_bar(aes(weight = PatCount, fill = gender))
g1 <-
   g1 + labs(title = "Region vs Patient Count", x = "Region", y =
               " Patient Count")
g1 <- g1 + scale_y_continuous(labels = comma)
g1 <- g1 + theme(axis.text.x = element_text(angle = 90, hjust = 1))
g1

#Percent plot
g1.1 <- ggplot(pat_across_region_gender, aes(x=reorder(city_village, -PatCount),weight=PatCount, fill=gender))
g1.1 <- g1.1 + geom_bar(position = "fill") + scale_y_continuous(labels = percent) 
g1.1 <- g1.1 + geom_text(aes(label=Percentage,y=Percentage/100), position = "stack",vjust = 1.25, size=2.5)
g1.1 <- g1.1 + labs(title = "Region-Gender %age distribution", x = "Region", y =
              "")
g1.1
pat_across_region_gender_percent<- patients %>% 
  inner_join(person_address, by =c("patient_id"="add_personId")) %>% 
  inner_join(person, by =c("patient_id"="person_id")) %>% 
  collect(n=Inf) %>% 
  inner_join(pat_across_region, by=c("city_village"="city_village")) 

g1.2 <- ggplot(pat_across_region_gender_percent, aes(x =city_village))   
g1.2 <- g1.2 + geom_bar(aes(y = (..count..)/sum(..count..), fill = gender))  
g1.2 <- g1.2 + geom_text(aes(y = ((..count..)/sum(..count..)), 
                             label = scales::percent((..count..)/sum(..count..))), 
                         stat = "count", vjust = -0.25) 
g1.2 <- g1.2 + scale_y_continuous(labels = percent) + labs(title = "Top 10 regions %age distribution", x = "Region", y =
                                                     "Percent")
g1.2
#Percent plot

#Top 10 diagnosis
pat_diag_db.df <- person %>% 
  inner_join(obs, by = c("person_id"="person_id")) %>% 
  group_by(person_id, gender, value_coded) %>% 
  summarise(count = n()) %>% 
  left_join(conceptnames, by = c("value_coded"="concept_id")) %>% 
  select(person_id, gender, value_coded, name, count) %>% 
  inner_join(person_address, by = c("person_id"="add_personId")) %>% 
  inner_join(person_age, by = c("person_id"= "age_personId")) %>% 
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
  rename(diagnosis_id = value_coded,
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
#Top 10 diagnosis

#Top diagnosis region distribution
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
gg <- gg + labs(title = "BoxPlot - Age Distribution")
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

#Explore results: get all lab results for patients having gastritis
patients_with_gastritis <- pat_diag.df %>% 
                            filter(diagnosis=="Gastritis") %>% 
                            select(person_id, gender)

obs_lab_results <- conDplyr %>% 
                    tbl("obs") %>% 
                    filter(voided==0, (!is.na(value_numeric) | !is.na(value_text))) %>% 
                    inner_join(concepts, by=c("concept_id"="conceptid")) %>% 
                    filter(class_id==26) %>% 
                    left_join(conceptnames, by = c("concept_id"="concept_id")) %>% 
                    collect(n=Inf) %>% 
                    inner_join(patients_with_gastritis, by=c("person_id"="person_id")) %>% 
                    select(obs_id, person_id, gender, concept_id, name, value_numeric, 
                           value_text, comments, obs_datetime) %>% 
                    rename(lab_test = name)
imp_tests <- c("Helminths parasite/Ova/Cyst/Trophozoites","Occult Blood Test",
               "Blood (Stool)","SGOT/AST",
               "SGPT/ALT","Alkaline Phosphate",
               "Bilirubin Total","Bilirubin Direct",
               "Protein (Serum)","Albumin (Serum)",
               "A-G Ratio")
obs_lab_results_female <- obs_lab_results %>% filter(gender=="F")
obs_imp_test_res <- obs_lab_results_female %>%
                filter(lab_test %in% imp_tests)
occ_postive_cases <- obs_imp_test_res %>% 
                    filter(lab_test=="Occult Blood Test", value_text=="Positive") %>% 
                    select(person_id)
helminth_postive_cases <- obs_imp_test_res %>% 
  filter(lab_test == "Helminths parasite/Ova/Cyst/Trophozoites", 
         person_id %in% unlist(occ_postive_cases))
#since the population is village females we have to look upon their dietary 
#habits whats their staple food?timing of their meals whether its only 
#gastritis or colitis too due to worm infestation ..their housing ..rule out 
#h.pylori infection as cause of gastritis due to lower 
#Socioeconomic status whether they taking excess quantity of 
#citrus substances.then stress related factors like mental agony too much work load etc..
#has to be assessed...

#Chronic Diabetes
patients_with_diabetes <- pat_diag.df %>% 
  filter(diagnosis=="Diabetes Mellitus") %>% 
  select(person_id, gender, age)

obs_lab_results_diabetes <- conDplyr %>% 
                            tbl("obs") %>% 
                            filter(voided==0, (!is.na(value_numeric) | !is.na(value_text))) %>% 
                            inner_join(concepts, by=c("concept_id"="conceptid")) %>% 
                            filter(class_id==26) %>% 
                            left_join(conceptnames, by = c("concept_id"="concept_id")) %>% 
                            collect(n=Inf) %>% 
                            inner_join(patients_with_diabetes, by=c("person_id"="person_id")) %>% 
                            select(obs_id, person_id, gender, age, concept_id, name, value_numeric, 
                                   value_text, comments, obs_datetime) %>% 
                            rename(lab_test = name)

diab_hb <- obs_lab_results_diabetes %>% 
  filter(lab_test=="Haemoglobin (Blood)", value_numeric<=11)

diab_hba1c_hb <- obs_lab_results_diabetes %>% 
  filter(lab_test =="HbA1c") %>% 
  inner_join(diab_hb, by = c("person_id"= "person_id")) %>% 
  select(person_id, gender.x, age.x, lab_test.x,value_numeric.x,
         value_text.x,lab_test.y,value_numeric.y, value_text.y)

#Check the heamogloblin levels for female patients, class ID 26 is for lab results
obs_HB_results <- conDplyr %>% 
                  tbl("obs") %>% 
                  filter(voided==0, (!is.na(value_numeric) | !is.na(value_text))) %>% 
                  inner_join(concepts, by=c("concept_id"="conceptid")) %>% 
                  filter(class_id==26) %>% 
                  inner_join(conceptnames, by = c("concept_id"="concept_id")) %>% 
                  filter(name == "Haemoglobin (Blood)"|name =="Haemoglobin (Serum)") %>% 
                  inner_join(person, by=c("person_id"="person_id")) %>% 
                  collect(n=Inf) %>% 
                  mutate(gender = as.factor(gender),
                        birthdate = ymd(birthdate),
                        age = round(age_calc(birthdate, units="years"),digits = 1)) %>% 
                  group_by(person_id, age, gender, birthdate, name, value_numeric) %>% 
                  summarise(HB_value = mean(value_numeric), count = n(), sum(value_numeric))
                  #select(person_id, age, gender, birthdate, name, value_numeric)

scatter_plot_1 <- ggplot(obs_HB_results, aes(x=age, y=HB_value, col=gender)) 
scatter_plot_1 + geom_point()+ geom_smooth(method = "lm", se = TRUE)
#check the quantile distribution of HB results
quantile_HB_dist <- quantile(obs_HB_results$HB_value, probs = seq(0, 1, 0.01))
#Looks like there are 3 entries with values such as 135, 156.2 and 6200, let's correct them, seems like data entry error
obs_HB_results <- tbl_df( obs_HB_results) %>% 
                  mutate(HB_value = case_when(
                                          .$HB_value >= 100 & .$HB_value <= 999.99 ~ .$HB_value/100,
                                          .$HB_value >= 1000 & .$HB_value <= 9999.99 ~ .$HB_value/1000,
                                          TRUE ~ .$HB_value))
#lets draw scatter plot again
scatter_plot_2 <- ggplot(obs_HB_results, aes(x=age, y=HB_value, col=gender)) 
scatter_plot_2 + geom_point()+ geom_smooth(method = "lm", se = TRUE)
#lets draw a line plot to make it more clear
line_plot_1 <- ggplot(obs_HB_results, aes(x=age, y=HB_value))
line_plot_1 <- line_plot_1 + geom_line(size=1, aes(color=gender))
line_plot_1 <- line_plot_1 +  labs(title = "Line Plot - Age vs Hemoglobin")
line_plot_1
#Lets add a facet wrap for male and female separately for 12 to 18 age group
scatter_plot_3 <- ggplot(obs_HB_results  %>% 
                           filter(age>=12, age <=18), aes(x=age, y=HB_value))
scatter_plot_3 <- scatter_plot_3 + geom_point(size=1, color = "blue") 
scatter_plot_3 <- scatter_plot_3 + geom_hline(yintercept = 12, color="red")  
scatter_plot_3<- scatter_plot_3 + facet_wrap(~gender) + labs(title = "Scatter Plot - Age vs Hemoglobin for 12 to 18 years")
scatter_plot_3
# number_of_teenagers <- person %>% 
#                         filter(!is.na(birthdate)) %>% 
#                         collect(n=Inf) %>% 
#                         mutate(gender = as.factor(gender),
#                                birthdate = ymd(birthdate),
#                                age = round(age_calc(birthdate, units="years"),digits = 1)) %>% 
number_of_teenagers <- obs_HB_results %>% 
                        filter(age>=12, age <=18) %>% 
                        group_by(gender) %>% 
                        summarise(count = n())

install_load("gridExtra")

grid.arrange(g1, g1.1, g1.2, g3,gg, gg1 ,g4,line_plot_1,scatter_plot_3,top = "Bahmni Exploratory Data Analysis")

dbDisconnect(conDplyr$con)
