library(dplyr)
library(tidyr)
library(readxl)
library(readr)
library(stringr)

daily <- read_csv("daily_SPEC_2014.csv.bz2")

#Question 1
#What is average Arithmetic.Mean for “Bromine PM2.5 LC” in the state of Wisconsin in this dataset?

daily %>% 
  select("Parameter Name", "State Name", "Arithmetic Mean") %>%
  rename(p_name = "Parameter Name",
         s_name = "State Name",
         a_mean = "Arithmetic Mean") %>%
  filter(s_name == "Wisconsin" & p_name == "Bromine PM2.5 LC") %>%
  summarize(w_mean = mean(a_mean))

#Question 2
#Calculate the average of each chemical constituent across all states, monitoring sites and all time points.
#Which constituent Parameter.Name has the highest average level?

daily %>%
  select("Parameter Name", "Arithmetic Mean") %>%
  rename(p_name = "Parameter Name",
        a_mean = "Arithmetic Mean") %>%
  group_by(p_name) %>%
  summarize(mean_levels = mean(a_mean)) %>%
  arrange(desc(mean_levels)) %>%
  head(20)

#Question 3
#Which monitoring site has the highest average level of “Sulfate PM2.5 LC” across all time?
#Indicate the state code, county code, and site number.

daily %>%
  select("State Code", "County Code", "Site Num", "Parameter Name", "Arithmetic Mean") %>%
  rename(sc = "State Code",
         cc = "County Code",
         sn = "Site Num",
         p_name = "Parameter Name",
         a_mean = "Arithmetic Mean") %>%
  filter(p_name == "Sulfate PM2.5 LC") %>%
  group_by(sc,cc,sn) %>%
  summarize(a_max = mean(a_mean)) %>%
  arrange(desc(a_max))
  
#Question 4
#What is the absolute difference in the average levels of “EC PM2.5 LC TOR” between the states California and Arizona, across all time and all monitoring sites?

q4 <- daily %>%
  select("State Name", "Parameter Name", "Arithmetic Mean") %>%
  rename(s_name = "State Name",
         p_name = "Parameter Name",
         a_mean = "Arithmetic Mean") %>%
  filter(p_name == "EC PM2.5 LC TOR" & (s_name == "California" | s_name == "Arizona")) %>%
  group_by(s_name) %>%
  summarize(means = mean(a_mean))

abs(q4$means[1] - q4$means[2])

rm(q4)

#Question 5
#What is the median level of “OC PM2.5 LC TOR” in the western United States, across all time? Define western as any monitoring location that has a Longitude LESS THAN -100.

daily %>%
  select("Longitude", "Parameter Name", "Arithmetic Mean") %>%
  rename(long = "Longitude",
         p_name = "Parameter Name",
         a_mean = "Arithmetic Mean") %>%
  filter(long < -100 & p_name == "OC PM2.5 LC TOR") %>%
  summarize(west_mean = median(a_mean))

#PART 2

#read in new data

aqs <- read_excel("aqs_sites.xlsx")

#Question 6
#How many monitoring sites are labelled as both RESIDENTIAL for "Land Use" and SUBURBAN for "Location Setting"?

aqs %>%
  select("Land Use", "Location Setting") %>%
  rename(land_use = "Land Use",
         loc_set = "Location Setting") %>%
  filter(land_use == "RESIDENTIAL" & loc_set == "SUBURBAN") %>%
  summarize(n = n())

#Question 7
#What is the median level of “EC PM2.5 LC TOR” amongst monitoring sites that are labelled as both “RESIDENTIAL” and “SUBURBAN” in the eastern U.S., where eastern is defined as Longitude greater than or equal to -100?

aqs_subset <- aqs %>%
  select("State Code", "County Code", "Site Number", "Land Use", "Location Setting", "Longitude") %>%
  rename(land_use = "Land Use",
         loc_set = "Location Setting",
         long = "Longitude",
         sc = "State Code",
         cc = "County Code",
         sn = "Site Number") %>%
  filter(long >= -100 & land_use == "RESIDENTIAL" & loc_set == "SUBURBAN") %>%
  mutate(sc = str_pad(sc, width = 2, side = "left", pad = "0")) %>%
  mutate(cc = str_pad(cc, width = 3, side = "left", pad = "0")) %>%
  mutate(sn = str_pad(sn, width = 4, side = "left", pad = "0")) %>%
  unite("id", c("sc", "cc", "sn")) %>%
  select(-long)

daily %>%
  select("State Code", "County Code", "Site Num", "Parameter Name", "Arithmetic Mean", "Longitude") %>%
  rename(long = "Longitude",
         p_name = "Parameter Name",
         a_mean = "Arithmetic Mean") %>%
  filter(p_name == "EC PM2.5 LC TOR" & long >= -100) %>%
  unite("id", c("State Code", "County Code", "Site Num")) %>%
  select(-long) %>%
  inner_join(aqs_subset, by = "id") %>%
  summarize(east_median = median(a_mean))

rm(aqs_subset)

#Question 8
#Amongst monitoring sites that are labeled as COMMERCIAL for "Land Use", which month of the year has the highest average levels of "Sulfate PM2.5 LC"?

aqs_subset_2 <- aqs %>%
  select("State Code", "County Code", "Site Number", "Land Use") %>%
  rename(land_use = "Land Use",
         sc = "State Code",
         cc = "County Code",
         sn = "Site Number") %>%
  filter(land_use == "COMMERCIAL") %>%
  mutate(sc = str_pad(sc, width = 2, side = "left", pad = "0")) %>%
  mutate(cc = str_pad(cc, width = 3, side = "left", pad = "0")) %>%
  mutate(sn = str_pad(sn, width = 4, side = "left", pad = "0")) %>%
  unite("id", c("sc", "cc", "sn"))

daily %>%
  select("State Code", "County Code", "Site Num", "Parameter Name", "Arithmetic Mean", "Date Local") %>%
  rename(p_name = "Parameter Name",
         a_mean = "Arithmetic Mean",
         date = "Date Local") %>%
  filter(p_name == "Sulfate PM2.5 LC") %>%
  unite("id", c("State Code", "County Code", "Site Num")) %>%
  mutate(month = str_extract(date, "-(\\d{2})-")) %>%
  mutate(month = str_extract(month, "\\d{2}")) %>%
  select(-date) %>%
  left_join(aqs_subset_2, by = "id") %>%
  filter(!is.na(land_use)) %>%
  group_by(month) %>%
  summarize(by_month = mean(a_mean)) %>%
  arrange(desc(by_month))

rm(aqs_subset_2)

#Question 9
#Take a look at the data for the monitoring site identified by State Code 6, County Code 65, and Site Number 8001 (this monitor is in California). At this monitor, for how many days is the sum of "Sulfate PM2.5 LC" and "Total Nitrate PM2.5 LC" greater than 10?
#For each of the chemical constituents, there will be some dates that have multiple `Arithmetic Mean` values at this monitoring site. When there are multiple values on a given date, take the average of the constituent values for that date.

daily_subset <- daily %>%
  select("State Code", "County Code", "Site Num", "Parameter Name", "Arithmetic Mean", "Date Local") %>%
  rename(p_name = "Parameter Name",
         a_mean = "Arithmetic Mean",
         date = "Date Local") %>%
  unite("id", c("State Code", "County Code", "Site Num")) %>%
  filter(id == '06_065_8001' & p_name %in% c("Sulfate PM2.5 LC", "Total Nitrate PM2.5 LC")) %>%
  group_by(p_name, date) %>%
  summarize(date_mean = mean(a_mean))

nitrate <- daily_subset %>%
  filter(p_name == "Total Nitrate PM2.5 LC") %>%
  rename(date_mean_n = date_mean)

sulfate <- daily_subset %>%
  filter(p_name == "Sulfate PM2.5 LC") %>%
  rename(date_mean_s = date_mean)

nitrate %>%
  inner_join(sulfate, by = "date") %>%
  mutate(total = date_mean_n + date_mean_s) %>%
  filter(total > 10) %>%
  summarize(n = n())
  
rm(list = c('daily_subset', 'nitrate', 'sulfate'))

#Question 10
#Which monitoring site in the dataset has the highest correlation between "Sulfate PM2.5 LC" and "Total Nitrate PM2.5 LC" across all dates? Identify the monitoring site by it's State, County, and Site Number code.
#For each of the chemical constituents, there will be some dates that have multiple Sample.Value's at a monitoring site. When there are multiple values on a given date, take the average of the constituent values for that date.  
  
daily_subset_2 <- daily %>%
  select("State Code", "County Code", "Site Num", "Parameter Name", "Arithmetic Mean", "Date Local") %>%
  rename(p_name = "Parameter Name",
         a_mean = "Arithmetic Mean",
         date = "Date Local") %>%
  unite("id", c("State Code", "County Code", "Site Num")) %>%
  filter(p_name %in% c("Sulfate PM2.5 LC", "Total Nitrate PM2.5 LC")) %>%
  group_by(id, p_name, date) %>%
  summarize(grouped_mean = mean(a_mean)) %>%
  ungroup() %>%
  unite("id_date", c(id, date))
  
nitrate_2 <- daily_subset_2 %>%
  filter(p_name == "Total Nitrate PM2.5 LC") %>%
  rename(nitrate_level = grouped_mean) %>%
  select(-p_name)

sulfate_2 <- daily_subset_2 %>%
  filter(p_name == "Sulfate PM2.5 LC") %>%
  rename(sulfate_level = grouped_mean) %>%
  select(-p_name)
  
nitrate_2 %>%
  inner_join(sulfate_2, by = "id_date") %>%
  mutate(id = substr(id_date, 1,11)) %>%
  select(-id_date) %>%
  group_by(id) %>%
  summarize(correls = cor(nitrate_level, sulfate_level)) %>%
  arrange(desc(correls))

rm(list = c("daily_subset_2", "nitrate_2", "sulfate_2", "q10"))
rm(list = c("aqs", "daily"))
