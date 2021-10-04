# Import Packages
library(tidyverse)
library(lubridate)
library(rstudioapi)

# Set WD
setwd(dirname(getActiveDocumentContext()$path))

# Read data
shoot <- read_csv("https://raw.githubusercontent.com/washingtonpost/data-police-shootings/master/fatal-police-shootings-data.csv")

shoot$date <- date(shoot$date)
shoot <- shoot %>% 
   mutate(year = year(date))


employ <- read_csv("employment.csv")
names(employ) <- c("state", "year", "o_count", "o_rate", "c_count", "c_rate", "pop")

# NOTE: THE FBI DATA ON EMPLOYMENT DOES NOT HAVE 2020 VALUES. KEEPING ALL SHOOT DATA SO THAT MAY STILL BE USED FOR OTHER ANALYSES.
data <- merge(shoot, employ, by = c("year", "state"), all.x = T)

shoot_final <- data %>% 
   select(year, state, o_rate, c_rate, pop) %>% 
   group_by(year, state) %>% 
   summarise(fatalities = n(), 
             o_per_1000 = first(o_rate), 
             c_per_1000 = first(c_rate),
             pop = first(pop)) %>% 
   select(year, state, fatalities, o_per_1000, c_per_1000, pop)

shoot_final <- shoot_final %>% 
   rename(state_abb = state, 
          population = pop,
          officers_per_1000 = o_per_1000,
          civilians_per_1000 = c_per_1000)

# Goal: Get state data for average age, total pop, and percent male, hisp, black
# Check: Against 2010 national averages

# Import census data
census <- read_csv("census.csv")
names(census) <- tolower(names(census))

# Factor labels introduced: see link below for details
# https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2010-2019/sc-est2019-alldata6.pdf
census$region <- factor(census$region, levels = 1:4, labels = c("NE", "MW", "S", "W"))

census$sex <- factor(census$sex, levels = 0:2, labels = c("total", "male", "female"))

census$hisp <- factor(census$origin, levels = 0:2, labels = c("total", "non-hispanic", "hispanic"))

census$race <- factor(census$race, levels = 1:6, labels = c("white", "black", "native", "asian", "islander", "mixed"))

# Total population and average age for each state
census_total <- census %>% 
   filter(sex == "total", hisp == "total") %>%   
   mutate(age_pop_prod = age * census2010pop) %>% 
   group_by(name) %>% 
   summarise(region = first(region),
             age_pop_prod_total = sum(age_pop_prod),
             total_pop = sum(census2010pop, na.rm = T)) %>% 
   mutate(mean_age = age_pop_prod_total / total_pop) %>% 
   select(-age_pop_prod_total)

# Collect population by sex, hisp, and race
census_male <- census %>% 
   mutate(pop = census2010pop) %>% 
   select(region, name, sex, hisp, race, age, pop) %>% 
   filter(hisp != "total", sex == "male") %>% 
   group_by(name) %>% 
   summarise(male_pop = sum(pop, na.rm = T))

census_hisp <- census %>% 
   mutate(pop = census2010pop) %>% 
   select(region, name, sex, hisp, race, age, pop) %>% 
   filter(sex != "total", hisp == "hispanic") %>% 
   group_by(name) %>% 
   summarise(hisp_pop = sum(pop, na.rm = T))

census_black <- census %>% 
   mutate(pop = census2010pop) %>% 
   select(region, name, sex, hisp, race, age, pop) %>% 
   filter(sex != "total", hisp != "total", race == "black") %>% 
   group_by(name) %>% 
   summarise(black_pop = sum(pop, na.rm = T))

# Merge count tables
census_final <- census_total %>% 
   left_join(census_male, by = "name") %>% 
   left_join(census_hisp, by = "name") %>% 
   left_join(census_black, by = "name")

# Check against national averages in 2010 - CORRECT DATA MANIPULATION
census_CHECK <- census_final %>% 
   summarise(age = weighted.mean(mean_age, total_pop),
             pop = sum(total_pop),
             male = sum(male_pop) / pop,
             hisp = sum(hisp_pop) / pop,
             bl = sum(black_pop) / pop)

for (i in 1:dim(census_final)[1]){
   if (census_final$name[i] == "District of Columbia"){
      census_final$state_abb[i] <- "DC"
   } else {
      census_final$state_abb[i] <- state.abb[state.name == census_final$name[i]]
   }
}

census_final <- census_final %>% 
   mutate(male_prop = male_pop / total_pop,
          hisp_prop = hisp_pop / total_pop,
          black_prop = black_pop / total_pop) %>% 
   select(-c(name, total_pop, male_pop, hisp_pop, black_pop))

crime <- read.csv("crime.csv")

for (i in 1:dim(crime)[1]){
   crime$max_rape[i] <- max(crime$rape_legacy[i], crime$rape_revised[i], na.rm = T)
}

crime_final <- crime %>% filter(year >= 2015, state_abbr != "") %>% 
   mutate(violent = homicide + max_rape + robbery + aggravated_assault,
          total = violent + property_crime + burglary + larceny + motor_vehicle_theft,
          crime_per_1000 = total / population * 1000, 
          violent_rate = violent / total,
          pop = population) %>% 
   select(year, state_abbr, state_name, pop, violent_rate, crime_per_1000)

# Choosing to use population from employment data instead of crime (both similar, from same source)
crime_final <- crime_final %>% 
   rename(state_abb = state_abbr,
          violent_prop = violent_rate) %>% 
   select(year, state_abb, violent_prop, crime_per_1000)

main <- shoot_final %>% 
   left_join(census_final, by = "state_abb") %>% 
   left_join(crime_final, by = c("state_abb", "year")) %>% 
   ungroup()
