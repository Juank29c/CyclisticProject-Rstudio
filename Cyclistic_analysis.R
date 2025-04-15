Div_Trips_2019_Q1 <- read.csv('D:/JUAN/Data Analyst/Google analytics/Capstone-Case study/Cyclistic/2019_Q1-Trips.csv')
Div_Trips_2020_Q1 <- read.csv('D:/JUAN/Data Analyst/Google analytics/Capstone-Case study/Cyclistic/2020_Q1-Trips.CSV')

#install.packages("tidyverse")
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("readr")
#install.packages("here")
#install.packages("skimr")
#install.packages("janitor")

#library(tidyverse)
#library(ggplot2)
#library(dplyr)
#library(readr)
#library(here)
#library(skimr)
#library(janitor)



Div_Trips_2019_Q1 <- Div_Trips_2019_Q1 %>%
  mutate(start_time = as.POSIXct(start_time, format = "%Y-%m-%d %H:%M:%S"),
  end_time = as.POSIXct(end_time, format = "%Y-%m-%d %H:%M:%S"),
  tripduration = round(as.numeric(gsub(",","",tripduration)),2),
  day_of_week = weekdays(start_time,1),
  trip_id = as.character(trip_id)
  )

Div_Trips_2020_Q1 <- Div_Trips_2020_Q1 %>%
  rename(start_time = started_at,
         end_time = ended_at,
         usertype = member_casual,
         trip_id = ride_id,
         from_station_id = start_station_id,
         from_station_name=start_station_name,
         to_station_id = end_station_id,
         to_station_name = end_station_name) %>%
  mutate(start_time = as.POSIXct(start_time, format = "%Y-%m-%d %H:%M:%S"),
         end_time = as.POSIXct(end_time, format = "%Y-%m-%d %H:%M:%S"),
         tripduration = round(as.numeric(difftime(end_time,start_time,units = "secs")),2),
         day_of_week = weekdays(start_time,1),
         usertype = case_when(
           usertype == "member" ~ "Subscriber",
           usertype == "casual" ~ "Customer")
         )
#verificaciC3n valores C:nicos
#conteo <- unique(Div_Trips_2010_Q1$rideable_type)
#conteo2 <- unique(Div_Trips_2020_Q1$rideable_type)

#limpieza campos vacios nombre columnas
Div_Trips_2019_Q1 <- janitor::clean_names(Div_Trips_2019_Q1)
Div_Trips_2020_Q1 <- janitor::clean_names(Div_Trips_2020_Q1)

# Conteo de NAs por columna
colSums(is.na(Div_Trips_2019_Q1))
colSums(is.na(Div_Trips_2020_Q1))

Div_Trips_2019_Q1 <- Div_Trips_2019_Q1 %>%
  mutate(birthyear = ifelse(is.na(birthyear),0,birthyear))

Div_Trips_2020_Q1 <- Div_Trips_2020_Q1 %>%
  mutate(to_station_id = ifelse(is.na(to_station_id),0,to_station_id),
         end_lat = ifelse(is.na(end_lat),0,end_lat),
         end_lng = ifelse(is.na(end_lng),0,end_lng))

Div_Trips_2019_Q1 %>% 
  duplicated() %>% sum()

Div_Trips_2020_Q1 %>% 
  duplicated() %>% sum()

dep <- Div_Trips_2020_Q1 %>% 
  select(start_time,end_time,tripduration)%>%
  filter(start_time == end_time)


Div_Trips_2020_Q1 <- Div_Trips_2020_Q1 %>%
  mutate(
    start_time = if_else(tripduration < 0, end_time, start_time),
    end_time   = if_else(tripduration < 0, start_time, end_time)
  )

Div_Trips_2020_Q1 <- Div_Trips_2020_Q1 %>%
  mutate(tripduration = as.numeric(difftime(end_time, start_time, units = "secs")))

Div_correct_Trips_2020_Q1 <- Div_Trips_2020_Q1 %>%
  filter(start_time != end_time)

sum(Div_Trips_2019_Q1$gender == "", na.rm = TRUE)

Gender_correct <- Div_Trips_2019_Q1 %>%
  filter(gender != "") %>%
  count(gender,usertype)

ggplot(data = Gender_correct) + geom_bar(mapping = aes(x = gender, y= n, fill = usertype),
            stat = "identity", position = "dodge") + facet_grid(~usertype) + 
            labs(title="Trips per gender and user type", x="Gender", y = "Number of trips", fill = "Type of user")

Rage <- Div_Trips_2019_Q1 %>%
  filter(birthyear > 1929) %>%
  mutate(years_old = 2020 - birthyear,
         age_group = cut(years_old,
                         breaks = c(0 , 18, 24, 34, 44, 54, 64, 90),
                         labels = c("-18","18-24", "25-34", "35-44", "45-54","55,64", "65+"),
                         right = FALSE))
  
Rage %>%
  count(age_group,usertype) %>%
  ggplot(aes(x = age_group, y= n, fill = usertype)) +
                                                      geom_bar(stat = "identity", position = "dodge") +
                                                      facet_grid(~usertype) +
                                                      labs(title="Trips per age ranges", x="Age ranges", y = "Number of trips", fill = "Type of user")

Dep_Trips_2019 <- Div_Trips_2019_Q1 %>%
  select(trip_id,start_time,day_of_week,end_time, tripduration, from_station_id, from_station_name, to_station_id, usertype)

Dep_Trips_2020 <- Div_correct_Trips_2020_Q1 %>%
  select(trip_id,start_time,day_of_week,end_time, tripduration, from_station_id, from_station_name, to_station_id, usertype)


Total_base <- bind_rows(Dep_Trips_2019, Dep_Trips_2020)

summary (Total_base)

weekday_counts <- Total_base %>%
  count(day_of_week,usertype)%>%
  group_by(usertype)%>%
  mutate(day_of_week=reorder(day_of_week,n))

ggplot(weekday_counts,aes(x = day_of_week, y= n, fill = usertype)) +
  geom_bar(stat = "identity") +
  facet_grid(~usertype) +
  labs(title="Number of trips per day of week", x="Day of the week", y = "Number of trips", fill = "Type of user")

Rtripduration <- Total_base %>%
  mutate(tripd_group = cut(tripduration,
                         breaks = c(1 , 328, 658, 984, Inf),
                         labels = c("1-328","329-658", "659-984", "+985"),
                         right = FALSE))

Rtripduration %>%
  count(tripd_group,usertype) %>%
  ggplot(aes(x = tripd_group, y= n, fill = usertype)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(~usertype) +
  labs(title="Trips by dyration range and user type", x="Trip duration group (seconds)", y = "Number of trips", fill = "User Type")

conteo <- Total_base %>%
  count(from_station_name,usertype,sort = TRUE) %>%
  group_by(usertype)%>%
  slice_max(n,n=5)

ggplot(conteo,aes(x=reorder(from_station_name,n),y=n, fill = usertype)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  facet_wrap(~usertype, scales = "free") +
  labs(
    title="Top 5 stations more used by user type",
    x= "From station name",
    y= "Trips number",
    fill = "User type"
  )


