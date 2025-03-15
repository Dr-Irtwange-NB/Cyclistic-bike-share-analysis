# **CYCLISTIC BIKE-SHARE ANALYSIS CASE STUDY**


## SCENARIO 

As a junior data analyst working on the marketing analyst team at Cyclistic, a bike-share
company in Chicago. The director of marketing believes the company’s future success
depends on maximizing the number of annual memberships. Therefore, the team wants to
understand how casual riders and annual members use Cyclistic bikes differently, to design a new marketing strategy so as to convert casual riders into annual members. Backed up Analysis with compelling data insights and professional data visualizations.


## BACKGROUND:

Cyclistic’s finance analysts have concluded that annual members are much more profitable than casual riders. My manager, Lily Moreno, believes that maximizing the number of annual members will be key to future growth. Rather than creating a marketing campaign that targets all-new customers, Moreno believes there is a very good chance to convert casual riders into members.

Moreno has set a clear goal: Design marketing strategies aimed at converting casual riders into annual members. In order to do that, however, the team needs to better understand how annual members and casual riders differ, why casual riders would buy a membership, and how digital media could act their marketing tactics. 

Moreno and her team are interested in analyzing the Cyclistic historical bike trip data to identify trends.

## DATA ANALYSIS PHASES:

### ASK PHASE

#### Business Objectives

To Understand the behavioral patterns of casual riders and annual members by;
•	Trip Characteristics:
Duration of trips.
Start and end stations.
Times of trips 
Types of bikes used
•	User Segmentation:
Membership type (casual vs. annual member).
•	Geospatial Data:
Popular routes for each user group.
Neighborhood trends.
•	Temporal Patterns:
Seasonal differences in usage.

#### Key stakeholders: 

1.	Lily Moreno: The director of marketing and manager
2.	Cyclistic marketing analytics team
3.	Cyclistic executive team

### PREPARE PHASE

*	Extracted and worked with Cyclistic’s historical bike trip data of the previous 12 months (November_2023 to October_2024). The data has been made available by
Motivate International Inc. under this [license](https://divvybikes.com/data-license-agreement).) 
Months of Cyclistic trip data .
*	Description of all data sources used
1.	Data location; public data, made available by Motivate International Inc.
2.	Data organization; 
a)	No issue of bias or credibility with data
b)	ROCCC- Data is reliable, original, comprehensive, current and cited.
c)	Data is licensed, secure, private and accessible to only me
*	Sort and filter data

### PROCESSING PHASE

Tools to used are : 
a)	R- due to Large data set too big for SQL and Google spreadsheets
b)	Tableau- For some data visualization

#### Processing steps

##### import,combine and save data

1) load all packages needed
```{r load packages to use, message=FALSE, warning=FALSE, paged.print=FALSE}

# load needed packages
library(tidyverse)
library(lubridate)
library(janitor)
library(readr)
library(ggplot2)
library(rlang)
library(here)
library(skimr)
library(janitor)
library(dplyr)
library(stringr)
library(hms)
library(here)
```

2) Import data sets needed for analysis (previous 12 months of trip data)
```{r import data, message=FALSE, eval=FALSE}

november_2023 <- read_csv("~/R/Capstone/Case 1/202311-divvy-tripdata.csv")
december_2023 <- read_csv("~/R/Capstone/Case 1/202312-divvy-tripdata.csv")
january_2024 <- read_csv("~/R/Capstone/Case 1/202401-divvy-tripdata.csv")
febuary_2024 <- read_csv("~/R/Capstone/Case 1/202402-divvy-tripdata.csv")
march_2024 <- read_csv("~/R/Capstone/Case 1/202403-divvy-tripdata.csv")
april_2024 <- read_csv("~/R/Capstone/Case 1/202404-divvy-tripdata.csv")
may_2024 <- read_csv("~/R/Capstone/Case 1/202405-divvy-tripdata.csv")
june_2024 <- read_csv("~/R/Capstone/Case 1/202406-divvy-tripdata.csv")
july_2024 <- read_csv("~/R/Capstone/Case 1/202407-divvy-tripdata.csv")
august_2024 <- read_csv("~/R/Capstone/Case 1/202408-divvy-tripdata.csv")
september_2024 <- read_csv("~/R/Capstone/Case 1/202409-divvy-tripdata.csv")
october_2024 <- read_csv("~/R/Capstone/Case 1/202410-divvy-tripdata.csv")
```

3) check data validity
```{r inspect data set, eval=FALSE, results='hide'}

View(may_2024)
View(november_2023)
View(october_2024)

colnames(may_2024)
colnames(october_2024) 
colnames(november_2023)
```
All have the same column names and number

4) Join all 12 data set together using rbind() function
```{r Join all 12 dataset, eval=FALSE}

Cyclistic_trip <- rbind(november_2023,december_2023,january_2024,febuary_2024,march_2024,april_2024,may_2024,june_2024,july_2024,august_2024,september_2024,october_2024)
```

5) Ensure all data set was combined successfully
```{r view or validate joined data_set, results='hide', eval=FALSE}

View(Cyclistic_trip) 
colnames(Cyclistic_trip) 
summary(Cyclistic_trip) 
names(Cyclistic_trip) 
str(Cyclistic_trip) 
```

6) Save combined data set to use
```{r save the combined file, eval=FALSE}

# based on file path provided.
write.csv(Cyclistic_trip,file = "~/R/Capstone/Cyclistic_trip.csv",row.names = FALSE)
```

##### Data Cleaning

Clean names, remove NA values, empty rows and rows where started at is greater than ended at
```{r Data Cleaning, eval=FALSE}

clean_Cyclistic_trip <- Cyclistic_trip %>%
  drop_na() %>%                                       # Remove rows with NA values
  filter(started_at < ended_at) %>%                   # Remove rows where started_at >= ended_at
  distinct() %>%                                      # Remove duplicate rows
  remove_empty("rows") %>%                            # Remove empty rows
  clean_names() %>%                                   # Ensure column names are unique and consistent
  rename(customer_type = member_casual,               # Rename columns for clarity
         bike_type = rideable_type)
```

##### Data manipulation

(a) Create a new column to cal the length of each ride showing the difference between ended_at and started_at in mins called ride length.
(b) Create a column called day of the week for the day each ride started via extracting weekdays from dates via 'weekday(.as_Dates())' function.
(c) Create a column with only dates
(d) Create a column with only months
(e) Create a column with only years
(f) Create a column for only time
(g) arrange week_day and month in calendar order

```{r Data manipulation, eval=FALSE}

clean_Cyclistic_trip <- clean_Cyclistic_trip %>%
  mutate(
   ride_length = round(as.numeric(difftime(ended_at, started_at, units = "mins")), 2),  # Calculate ride length in minutes and round it to 2 decimal places 
    week_day = weekdays(as.Date(started_at)),                                 # Extract day of the week
    week_day = factor(week_day, levels = c("Sunday", "Monday", "Tuesday", 
                                           "Wednesday", "Thursday", "Friday", 
                                           "Saturday"), ordered = TRUE),      # Arrange week_day in calendar order
    dates = as.Date(started_at),                                              # Extract only the date
    month = months(dates, abbreviate = FALSE),                                # Extract month name (full)
    month = factor(month, levels = month.name, ordered = TRUE),               # Arrange month in calendar order
    year = year(dates),                                                       # Extract year
    time = format(as.POSIXct(started_at), "%H:%M:%S")                         # Extract only the time
  ) 
```

To ensure the accuracy and reliability of the data, get rid of excessively long ride more than 24hrs (>1440 mins), as rides
last typically for a day
Also data below 5 minutes (<5 mins), due to it begin too small.
To get rid of stolen bikes

```{r remove stolen bikes data, eval=FALSE}

clean_Cyclistic_trip <- clean_Cyclistic_trip[!clean_Cyclistic_trip$ride_length>1440,] 
clean_Cyclistic_trip <- clean_Cyclistic_trip[!clean_Cyclistic_trip$ride_length<5,] 
```

Determine duration when data was collected

```{r duration of data, eval=FALSE}

mindate <- min(clean_Cyclistic_trip$dates)
maxdate <- max(clean_Cyclistic_trip$dates)
```

Select relevant data for analysis

```{r Select relevant data for analysis, eval=FALSE}

clean_Cyclistic_trip_v1 <- clean_Cyclistic_trip %>% 
  select(bike_type, customer_type, start_station_name, end_station_name, started_at, time, ride_length, week_day, month, year)
```

Save Cleaned and Relevant data

```{r Save Cleaned and Relevant data, eval=FALSE}

write.csv(clean_Cyclistic_trip,file = "~/R/Capstone/clean_Cyclistic_trip.csv",row.names = FALSE)

write.csv(clean_Cyclistic_trip_v1,file = "~/R/Capstone/clean_Cyclistic_trip_v1.csv",row.names = FALSE)
```

### ANALYZING PHASE

```{r load packages, message=FALSE, warning=FALSE, echo=FALSE}
library(readr)
library(dplyr)
library(tidyr)
library(knitr)
library(gt)
library(gtsummary)
library(kableExtra)
library(ggplot2)
```

```{r load dataframe, message=FALSE, warning=FALSE, echo=FALSE}
clean_Cyclistic_trip_v1 <- read_csv("~/R/Capstone/clean_Cyclistic_trip_v1.csv")
```

View and Validate data set

```{r Validate data, message=FALSE, warning=FALSE, results='hide'}
str(clean_Cyclistic_trip_v1)

summary(clean_Cyclistic_trip_v1)

colnames(clean_Cyclistic_trip_v1)
```

#### Understand the general trend and pattern of all Cyclistic bike users.

##### Ride and Ride durations

###### Determine the number of customer type and bike type of all Cyclistic bike users.

Total of Cyclistic users and bikes

```{r Total of Cyclistic users and bikes, echo=FALSE, message=FALSE, warning=FALSE}

Total_number_of_Cyclistic_bike_users <- nrow(clean_Cyclistic_trip_v1)

Total_number_of_Cyclistic_bike_users
```

Total number of customer type

```{r Total of different Cyclistic users, echo=FALSE, message=FALSE, warning=FALSE}
Number_of_customers <- clean_Cyclistic_trip_v1 %>%
   group_by(customer_type) %>%
  summarize(number_of_customers = n())

Number_of_customers$percentage <- (Number_of_customers$number_of_customers/ sum(Number_of_customers$number_of_customers))*100

Number_of_customers
```

From the result, the Member customers makes up 61.0% of the total customers as compared to the Casual customer at 39.0% of the total customers.

Total number of bike type

```{r Total of different bike type, echo=FALSE, message=TRUE, warning=FALSE}
Number_of_bikes <- clean_Cyclistic_trip_v1 %>%
    group_by(bike_type) %>% 
  summarize(number_of_bikes = n()) %>% 
  arrange(bike_type)

Number_of_bikes$percentage <- (Number_of_bikes$number_of_bikes/sum(Number_of_bikes$number_of_bikes))*100

Number_of_bikes
```

The classic bike is more popular among customers , followed by the electric bike and least popular the electric scooter.

Total number of bike type per customer type.

```{r customer type via bike type, echo=FALSE, message=TRUE, warning=FALSE}
Cyclistic_users_bike_preference <- clean_Cyclistic_trip_v1 %>%
  group_by(customer_type, bike_type) %>%
  summarize(
    number_of_bikes = n(),
    .groups = "drop"
  ) %>%
  arrange(customer_type, bike_type)

Cyclistic_users_bike_preference$percentage <- (Cyclistic_users_bike_preference$number_of_bikes/sum(Cyclistic_users_bike_preference$number_of_bikes))*100

Cyclistic_users_bike_preference
```

Preference of bike type among customer type are as follows;

For the Casual customers, the classic bike is the most popular at 66.3%, followed by the electric bike at 32.3% and the electric scooter at 1.4%. Among casual users.

For the Member customers, the classic bike is the most popular at 66.9%, followed by the electric bike at 32.4% and the electric scooter at 0.7%. Among member users.

Visualization on the Analysis on all Cyclistic bike users

[![Tableau Visualization](https://github.com/Dr-Irtwange-NB/Cyclistic-bike-share-analysis/blob/main/images/Population%20of%20Cyclistic%20bike's%20customer%20and%20bike%20types.png?raw=true)](https://public.tableau.com/views/AnalysisonallCyclisticbikeusers/PopulationofCyclisticbikescustomerandbiketypes?:language=en-US&:embed=y&:sid=&:redirect=auth&:embed_code_version=3&:loadOrderID=0&:display_count=y&:origin=viz_share_link)

These bar charts shows the comparative differences between customer types, bike types and customer's bike preference of Cyclistic bike users.

###### Determine ride duration of all Cyclistic bike users.

(a) Ride duration: All users

```{r ride duration: all users, echo=FALSE, message=FALSE, warning=FALSE}
Ride_duration_for_all_users <- clean_Cyclistic_trip_v1 %>%
  summarize(
    average_ride_duration = round (mean(ride_length), 2),
    minimum_ride_duration = min(ride_length),
    median_ride_duration = median(ride_length),
    maximum_ride_duration = round (max(ride_length), 2)
  )

Ride_duration_for_all_users
```

This table shows that the average ride duration was 19.73 mins indicating that most trips are relatively short, maybe for such as commuting or recreational rides, with a minimum of 5mins (set a minimum duration), a medium of 12.42 mins, since medium is lower than the average, it indicates a right-skewed distribution and a maximum of 24hrs (set as maximum duration).

(b) Ride duration: customer type

```{r ride duration: customer type, echo=FALSE, message=FALSE, warning=FALSE}
Ride_duration_for_customers_types <- clean_Cyclistic_trip_v1 %>%
  group_by(customer_type) %>%
  summarize(
    average_ride_duration = round (mean(ride_length), 2),
    minimum_ride_duration = min(ride_length),
    median_ride_duration = median(ride_length),
    maximum_ride_duration = round (max(ride_length), 2)
  )

Ride_duration_for_customers_types
```

This table shows that Casual customers tend to take longer trips (recreational or leisure purposes) than the Member customer type (commuting purposes).

(c) Ride duration: bike type

```{r ride duration: bike type, echo=FALSE, message=FALSE, warning=FALSE}
Ride_duration_for_bike_types <- clean_Cyclistic_trip_v1 %>%
  group_by(bike_type) %>%
  summarize(
    average_ride_duration = round (mean(ride_length), 2),
    minimum_ride_duration = min(ride_length),
    median_ride_duration = median(ride_length),
    maximum_ride_duration = round (max(ride_length), 2)
  )

Ride_duration_for_bike_types
```

This table shows that more to least time is spent on the classic_bike (longer trips : leisure or recreational use),electric_bike (Moderate trips : practical / time-sensitive usage e.g commuting) and electric_scooter (short quick trips : short errands) respectively.

(d) ride duration : customer type and bike type

```{r ride duration: customer type with bike type, echo=FALSE, message=FALSE, warning=FALSE}
Ride_duration_for_customer_and_bike_types <- clean_Cyclistic_trip_v1 %>%
  group_by(customer_type, bike_type) %>%
  summarize(
    average_ride_duration = round(mean(ride_length), 2),
    minimum_ride_duration = min(ride_length),
    median_ride_duration = median(ride_length),
    maximum_ride_duration = round(max(ride_length), 2)
  )

Ride_duration_for_customer_and_bike_types

```

This table shows that in general, the casual customer take longer trips across all bike types than the member type. More to least time is spent on the classic_bike,electric_bike and electric_scooter respectively for both customer. type. 

Members consistently has short average duration across all bike types indicating goal oriented usage such as time sensitive tasks or commuting.

Visualization on the Analysis of Average ride duration of all Cyclistic bike users

[![Tableau Visualization](https://github.com/Dr-Irtwange-NB/Cyclistic-bike-share-analysis/blob/main/images/Average%20ride%20durations%20for%20all%20Cyclistic%20bike's%20users.png?raw=true)](https://public.tableau.com/views/AnalysisofAverageridedurationsofallCyclisticbikeusers/AverageridedurationsforallCyclisticbikesusers?:language=en-US&:embed=y&:sid=&:redirect=auth&:embed_code_version=3&:loadOrderID=1&:display_count=y&:origin=viz_share_link)

This visualization shows, the most to least popular bike type in descending order among both customer groups as the classic_bike, electric_bike and electric_scooter respectively. The highest ride duration is by the Casual customer type which shows that although they makes up 39.0% of the total customers of Cyclistic users, more time is spent on rides by these group compared to the Member type that makes up 61.0% of the total customers. This may indicates Casual users preference for leisure trips and less time sensitive tasks.

##### Analyze data on weekly basis

###### Weekly rides

(a) Total number of weekly rides : All

```{r Total number of weekly rides, echo=FALSE, message=FALSE, warning=FALSE}
Number_of_weekly_rides <- clean_Cyclistic_trip_v1 %>%
  group_by(week_day) %>%
  summarise(total_number_of_rides = n()) %>%
  arrange(desc(total_number_of_rides))

Number_of_weekly_rides

```

From this table, Saturday (peak weekend) is the day where most rides occur in a year, this may be due to leisure rides. Followed by Wednesday and Thursday (peak weekdays), this may be due to commuting or errand. Lower rides occur on early weekdays (Tuesday and Monday), which may be attributed to a quite start of the week.

(b) Total number of weekly Cyclistic rides : customer types

```{r Total number of weekly rides by customer type, echo=FALSE, message=FALSE, warning=FALSE}
Number_of_weekly_Cyclistic_riders <- clean_Cyclistic_trip_v1 %>%
  group_by(week_day, customer_type) %>%
  summarise(total_number_of_rides = n(), .groups = "drop") %>%
  arrange(customer_type, desc(total_number_of_rides))

Number_of_weekly_Cyclistic_riders
```

Table shows that  Member customers dominate the weekdays (Monday to Friday), reflecting a trend that most rides are primarily for commuting purposes. While, Casual customers dominate the weekends (Sunday and Saturday), which aligns to leisure and recreational purposes. Wednesday has the highest ride for the Member customers which makes up 61.0% of the total customers. The 5 top rides occurs on weekdays by the member type and the lowest ride is on Tuesday by the Casual customers.

(c) Total number of weekly Cyclistic rides : bike types

```{r Total number of weekly rides by bike type, echo=FALSE, message=FALSE, warning=FALSE}
Number_of_weekly_Cyclistic_riders_Bike_type <- clean_Cyclistic_trip_v1 %>%
  group_by(week_day, bike_type) %>%
  summarise(total_number_of_rides = n(), .groups = "drop") %>%
  arrange(bike_type, desc(total_number_of_rides))

Number_of_weekly_Cyclistic_riders_Bike_type
```

Table shows that most rides are taken by the classic bikes across all days with it's peak on Saturdays, Electric bikes are moderate and consistent throughout the week with peak on Wednesday and the least by electric scooter with peak on Thursday.For rides based on bike types, the highest rides occur on Saturday with the classic bikes and least rides also occur on Saturday with the electric scooter. This may be due to limited availability or niche appeal.

(d) Total number of weekly rides : customer types and bike type

```{r Total number of weekly rides by customer and bike type, echo=FALSE, message=FALSE, warning=FALSE}
Number_of_weekly_Cyclistic_riders_customer_and_Bike_type <- clean_Cyclistic_trip_v1 %>%
  group_by(week_day, customer_type, bike_type) %>%
  summarise(
    total_number_of_rides = n(),
    .groups = "drop"
  ) %>%
  arrange(customer_type, bike_type, desc(total_number_of_rides))

Number_of_weekly_Cyclistic_riders_customer_and_Bike_type
```

Across all customer groups, the classic bike dominates, with member customers for weekdays and casual customers for weekends. The electric bike shows peak on Wednesday, Thursday and Tuesday for member customers and drops on Saturday and Sunday while for the Casual users peaks on weekends.Preference for electric scooter is minimum across all customer types, with a slight preference by casual users on Monday and Sunday and a peak for Member customers on Thursday. The most weekly rides by customers types on their preferred bike type are the Member customers on the classic bike type on Wednesday and the least on Sunday by the member type on the electric scooter.

Visualization on the Analysis of Cyclistic users weekly rides 1

[![Tableau Visualization](https://github.com/Dr-Irtwange-NB/Cyclistic-bike-share-analysis/blob/main/images/Analysis%20of%20Cyclistic%20users%20weekly%20rides%201.png?raw=true)](https://public.tableau.com/views/AnalysisofCyclisticusersweeklyrides1/AnalysisofCyclisticusersweeklyrides1?:language=en-US&:embed=y&:sid=&:redirect=auth&:embed_code_version=3&:loadOrderID=2&:display_count=y&:origin=viz_share_link)

Bar chart showing the distribution of customer type rides to total rides within a week of a whole year.

Visualization on the Analysis of Cyclistic users weekly rides 2

[![Tableau Visualization](https://github.com/Dr-Irtwange-NB/Cyclistic-bike-share-analysis/blob/main/images/Analysis%20of%20Cyclistic%20users%20weekly%20rides%202.png?raw=true)](https://public.tableau.com/views/AnalysisofCyclisticusersweeklyrides1/AnalysisofCyclisticusersweeklyrides2?:language=en-US&:embed=y&:sid=&:redirect=auth&:embed_code_version=3&:loadOrderID=3&:display_count=y&:origin=viz_share_link)

Bar chart showing the distribution of rides on bike types and rides of customer type on preferred bike types within a week of a whole year.

From the above, when a whole year is grouped into a week, it's discovered that; 

Most rides in general occur on Saturdays indicating Recreational or leisure purposes, with most ride by Casual customers.

Among the customer types, the most ride was by the Member customers on Wednesday, indicating commuting purposes.

When grouped by bike type, data shows that most rides are taken with the classic bike type which peaks on Saturday showing a preference for leisure trips, with moderate rides by the electric bike which peaks on Wednesday showing a preference for commuting purpose and least by the scooter type which may indicate a small niche or unpopular bike type.

The Member customers seem have more rides on all bike types on weekdays with less on weekends while the Casual customers seem to have more rides on all bike types on weekends with less on weekdays. 

###### Average weekly ride duration

(e) Average weekly ride duration : All

```{r Average ride duration weekly : All, echo=FALSE, message=FALSE, warning=FALSE}
Average_weekly_ride_duration <- clean_Cyclistic_trip_v1 %>%
  group_by(week_day) %>%
  summarise(
    average_ride_duration = round(mean(ride_length), 2),
    .groups = "drop"
  ) %>%
  arrange(desc(average_ride_duration))

Average_weekly_ride_duration

```

Table shows, on average, The most trip duration occur on Sunday at 23.6 mins and Saturdays 23.5 mins, this indicates that users tend to engage in longer trips during the weekends, could be for non commuting purposes such as recreation or for leisure. The weekdays show less trip duration with the least on Tuesday at 17.3 mins. Shorter trips on weekdays may indicate a commuter- focused purpose.

(f) Average weekly ride duration : customer types

```{r Average ride duration weekly : customer types, echo=FALSE, message=FALSE, warning=FALSE}
Average_weekly_customer_type_ride_duration <- clean_Cyclistic_trip_v1 %>%
  group_by(week_day, customer_type) %>%
  summarise(
    average_ride_duration = round(mean(ride_length), 2),
    .groups = "drop"
  ) %>%
  arrange(desc(average_ride_duration))

Average_weekly_customer_type_ride_duration
```

Table shows, on average, The highest trip duration occur on Sunday by the Casual customer and least trip on Thursday by the Member customers.

For Casual customers, Sunday and Saturday has the highest trip duration which decreases on weekdays and Tuesday being the least. This indicates recreational and leisure purposes of service

For Member customers, the Highest trip duration occur on Sunday and Saturday.An almost constant shorter trip duration on weekdays with Thursday being the least. This may indicate a commute based usage.

(g) Average weekly ride duration : bike types

```{r Average ride duration weekly : bike types, echo=FALSE, message=FALSE, warning=FALSE}
Average_weekly_bike_type_ride_duration <- clean_Cyclistic_trip_v1 %>%
  group_by(week_day, bike_type) %>%
  summarise(
    average_ride_duration = round(mean(ride_length), 2),
    .groups = "drop"
  ) %>%
  arrange(bike_type, desc(average_ride_duration)) 

Average_weekly_bike_type_ride_duration
```

Table shows, on average, the Most popular bike is the classic bike type used on Sunday for a duration of 26.3 mins and the least is the electric scooter used on Tuesday for 11.21 mins.

The Classic bike is most popular, with the highest trip duration on Weekends and least on Weekdays, this indicates that riders are more inclined to take longer trips for recreational or leisure purposes on weekend and commute on weekdays.

The electric bike trip duration is generally shorter than the classic bikes. It shows longer ride duration on weekends and consistent and shorter duration on weekdays especially on Tuesday and Thursday. This may indicate a preference for shorter leisure trips on weekends and commuting for quick efficient rides on weekdays.

The electric scooter has the shortest trip duration across all week. A slightly longer duration on weekends and an even shorter duration on weekdays especially Tuesday and Thursday. This may indicate a short, quick trip or for fast short distance transportation.

(h) Average weekly ride duration : customer type and bike type

```{r Average ride duration weekly : customer type and bike type, echo=FALSE, message=FALSE, warning=FALSE}
Average_weekly_customer_type_ride_duration_for_bike_types <- clean_Cyclistic_trip_v1 %>%
  group_by(week_day, customer_type, bike_type) %>%
  summarise(
    average_ride_duration = round(mean(ride_length), 2),
    .groups = "drop"
  ) %>%
  arrange(customer_type, bike_type, desc(average_ride_duration))

Average_weekly_customer_type_ride_duration_for_bike_types
```

Table shows, on average, the Highest ride/trip duration is by the Casual customers on the Classic bike for a duration of 34.5 mins on Sunday and the least by Member customers on the Electric_scooter for a duration of 10.02 mins on Tuesday.

Casual riders; Use Classic bikes - for long trips on weekends (Sunday and Saturday) maybe for recreational rides or leisure. This is due to the shorter trips taken on weekdays (least on Tuesday) . Electric bike - Peak on weekends (Saturday and Sunday) maybe for leisure trips and least on Tuesday, may be for short distance trips or errands. Electric scooter - Peak on Sunday and least on Tuesday, may be use for short distance trips or errands.

Member riders; Classic bike - Peak on weekends (Sunday and Saturday), least on Friday, shorter and consistent duration compared to casual users indicating transportation rather than leisure. Electric bike - peak on Sunday, least on Tuesday, may be used for quick weekday trips. Electric scooter - Peaks on weekends (Sunday and Saturday) and least on Monday and Tuesday, have the shortest ride duration may be primarily used for very short, fast trips.

Visualization on the Analysis of Cyclistic user's weekly ride duration 1

[![Tableau Visualization](https://github.com/Dr-Irtwange-NB/Cyclistic-bike-share-analysis/blob/main/images/Analysis%20of%20Cyclistic%20user's%20weekly%20ride%20durations%201.png?raw=true)](https://public.tableau.com/views/AnalysisofCyclisticusersweeklyridedurations/AnalysisofCyclisticusersweeklyridedurations1?:language=en-US&:embed=y&:sid=&:redirect=auth&:embed_code_version=3&:loadOrderID=4&:display_count=y&:origin=viz_share_link)

This visual shows comparison Of average Trip/ride  duration of week of a whole year, between total and distribution of each customer type.

Visualization on the Analysis of Cyclistic user's weekly ride duration 2

[![Tableau Visualization](https://github.com/Dr-Irtwange-NB/Cyclistic-bike-share-analysis/blob/main/images/Analysis%20of%20Cyclistic%20user's%20weekly%20ride%20durations%202.png?raw=true)](https://public.tableau.com/views/AnalysisofCyclisticusersweeklyridedurations/AnalysisofCyclisticusersweeklyridedurations2?:language=en-US&:embed=y&:sid=&:redirect=auth&:embed_code_version=3&:loadOrderID=5&:display_count=y&:origin=viz_share_link)

This visual shows comparison On average trip duration of a week of a whole year, between bike types used in total and distribution of each bike type based on customer type preference. 

From the above, when a whole year is grouped into a week, it's discovered that; 

Longest trip duration in general occur on Sunday followed closely by Saturdays (Weekend) indicating Recreational or leisure purposes, with most trips by Casual customers.

Among the customer types, the most duration was by the Casual customers on weekends, indicating leisure purposes, while the Member customers maintain an almost constant trip duration on weekdays and a slight rise on weekends indicating commuting purposes.

When grouped by bike type, data shows that Longest trips are taken with the classic bike type which peaks on the weekends showing a preference for leisure trips, with moderate trip duration by the electric bike which slightly peaks on weekend but maintain an almost consistent duration on weekdays showing a preference for commuting purpose and least by the scooter type which has a slightly longer duration on weekends and short duration on weekdays especially Tuesday and Thursday. Indicating a short, quick trip or for fast short distance transportation.

The Casual customers have longer trip duration on all bike types especially on weekends and less on weekdays while the Member customers seem to maintain an almost constant short trip duration among all bike types with a slight rise during the weekends. 

##### Analyze data on monthly basis

###### Monthly rides 
Seasons of Chicago

![Chicago Seasons](https://github.com/Dr-Irtwange-NB/Cyclistic-bike-share-analysis/blob/main/images/Chicago%20seasons.png?raw=true)

Cyclistic, a bike-share company is located in Chicago.The seasonal changes of Chicago includes;

December — Feb. = Winter; characterized by Cold temperatures often below freezing, Snowfall is common, with potential for heavy storms and Shorter days and longer nights.

March — May = Spring; characterized by Gradually warming temperatures,Frequent rain showers (sometimes referred to as "April showers") and Increasing daylight hours.

June — Aug. = Summer; characterized by Hot and humid with temperatures often in the 70s–90s °F (21–37 °C), Occasional thunderstorms and Longest days of the year with abundant sunshine.

Sept. — Nov. = Fall characterized by Cooling temperatures, Crisp air and occasional rain and Shortening days and lengthening nights.

(a) Total number of monthly rides : All

```{r Monthly rides : all, echo=FALSE, message=FALSE, warning=FALSE}
Number_of_monthly_rides <- clean_Cyclistic_trip_v1 %>%
  group_by(month) %>%
  summarise(total_number_of_rides = n()) %>%
  arrange(desc(total_number_of_rides))

Number_of_monthly_rides
```

Table shows that most rides occur in July, August and September which is the summer and beginning of Autumn. This aligns with warmer summer when outdoor activities are popular.

least rides occur in January, December and February which are Winter Months.

This distribution of rides reflects seasonal changes, with distinct patterns for winter, spring, summer, and autumn. This can be associated with changes in Chicago's seasons, showing a seasonal trend. Bike rides peaks in summer, slightly drops in spring and a clear drop in Winter. This indicate that rides is dependent on the season.

(b) Number of monthly rides : customer type

```{r Monthly rides : customer type, echo=FALSE, message=FALSE, warning=FALSE}
Number_of_monthly_Cyclistic_riders <- clean_Cyclistic_trip_v1 %>%
  group_by(month, customer_type) %>%
  summarise(total_number_of_rides = n(), .groups = "drop") %>%
  arrange(desc(total_number_of_rides))

Number_of_monthly_Cyclistic_riders
```

Table shows that through out the year, most rides are by Member customers when compared to the Casual customers. This may be due to consistent regular use (commuting), more regular and flexible access to the bikes, loyalty to the bike program or year round use/access. The overall riding trend shows that there's a sharp increase of rides during the summer months and a steep decrease during the winter months. There's also a Large significant decrease of rides by casual customers during the Winter months. This shows that Casual customers are likely to use bike during optimal weather conditions when compared to member customers who continue to ride year round.  

This distribution reflects seasonal changes, with distinct patterns for winter, spring, summer, and autumn.

(c) Number of monthly rides : bikes types

```{r Monthly rides : bike type, echo=FALSE, message=FALSE, warning=FALSE}
Number_of_monthly_Cyclistic_riders_Bike_type <- clean_Cyclistic_trip_v1 %>%
  group_by(month, bike_type) %>%
  summarise(
    total_number_of_bikes = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(total_number_of_bikes))

Number_of_monthly_Cyclistic_riders_Bike_type
```

Table shows that the classic bike type are more favored year-round than the Electrical bike and Electric scooter (used only in the Month of September and August). This distribution reflects seasonal changes, with distinct patterns for winter, spring, summer, and autumn.

Classic bikes; Most Popular year-round, peaks during the summer months (Highest-July) with a steep Winter decline (Lowest-January).

Electrical bike; Moderately used year-round, peaks during the summer months (Highest-August) with a steep Winter decline (Lowest-January).

Electric scooter; Has a marginal and negligible contribution. Used only during the months of August (Lowest) and September (Highest). This could indicate no appeal to customers, limited availability or Seasonality.

(d) Number of monthly rides : customer type and bike type

```{r Monthly rides : customer type and bike type, echo=FALSE, message=FALSE, warning=FALSE}
Number_of_monthly_Cyclistic_customers_type_Bike_type <- clean_Cyclistic_trip_v1 %>%
  group_by(month, customer_type, bike_type) %>%
  summarise(
    total_number_of_bikes = n(),
    .groups = "drop"
  ) %>%
  arrange(bike_type, desc(total_number_of_bikes))


Number_of_monthly_Cyclistic_customers_type_Bike_type
```

Table shows that the most favored bike type year-round by both customer group, is the Classic bike type, with distribution of rides following seasonal trend. It shows high number of rides by Member customers to Casual customers.

Classic bike; Dominated by Member customers with peak in July for both customer group.There's a clear difference in rides during the Winter months when there's a sharp drop in usage by Casual customers and a steady resurgence by the Member type following seasonal trend.

Electric bike; There's a growing reliance of bike type by the Member customers indicated by it's frequent and moderate year-round use even during winter months, while there's a lower adaptation for the Casual customers indicated by it" preference during the summer months.

Electric scooter; There's limited bike usage across data set (only in 2 months : August and September) which may indicate Limited availability, niche, adoption or seasonality.

Visualization on the Analysis of Cyclistic users Monthly rides 1

[![Tableau Visualization](https://github.com/Dr-Irtwange-NB/Cyclistic-bike-share-analysis/blob/main/images/Analysis%20of%20monthly%20Cyclistic%20rides%201.png?raw=true)](https://public.tableau.com/views/AnalysisofCyclisticusersMonthlyrides/AnalysisofmonthlyCyclisticrides1?:language=en-US&:embed=y&:sid=&:redirect=auth&:embed_code_version=3&:loadOrderID=6&:display_count=y&:origin=viz_share_link)

Bar chart shows seasonal changes to the number of bike rides and distribution between each customer type.

Visualization on the Analysis of Cyclistic users Monthly rides 2

[![Tableau Visualization](https://github.com/Dr-Irtwange-NB/Cyclistic-bike-share-analysis/blob/main/images/Analysis%20of%20monthly%20Cyclistic%20rides%202.png?raw=true)](https://public.tableau.com/views/AnalysisofCyclisticusersMonthlyrides/AnalysisofmonthlyCyclisticrides2?:language=en-US&:embed=y&:sid=&:redirect=auth&:embed_code_version=3&:loadOrderID=7&:display_count=y&:origin=viz_share_link)

Bar chart shows the number of rides by each bike type and it's distribution between each customer type.

From the above, when a whole year is grouped into a months, it's discovered that;
Highest number of rides occur during Summer and Autumn with less ride in Winter as rides are shown to be affected by Chicago's seasonal pattern.

Casual customers seem to use bike rentals for mostly leisure and recreational purposes as seen by a sharp increase in rentals during warmer and conducive months with a steep fall during winter months compared to the Member customers who maintain year-round rides indicating commuting purposes.

When grouped by bike type, data shows that Most rides are taken with the classic bike type which peaks during the warmer months with a decline in Winter months.Showing  a preference for leisure or recreational rides with this bike type. The electric bike shows moderate increase in rentals during the warmer months showing it's preference for commuting purposes. The electric scooter shows rentals in August and September only, this may due to it's seasonality, a small niche, unavailability or disinterest.

The Casual customers have more rides during the warmer months and less during the winter months for all bike type showing that their interest in rides are mostly geared towards recreational purposes while the Member customers seem to maintain an almost constant year-round rides among all bike types with a slight rise during the warmer months showing that their interest in rides are mostly geared towards commuting purposes. 

###### Monthly ride durations 

(e) Average monthly ride duration : All Cyclistic riders

```{r Average monthly ride duration : All, echo=FALSE, message=FALSE, warning=FALSE}
Average_monthly_ride_duration <- clean_Cyclistic_trip_v1 %>%
  group_by(month) %>%
  summarise(
    average_ride_duration = round(mean(ride_length), 2),
    .groups = "drop"
  ) %>%
  arrange(desc(average_ride_duration))

Average_monthly_ride_duration
```

Table shows ride duration showing seasonal trend, with long duration in July, June and May (summer), Moderate in April, September and October (spring) and Least in December, January and November (Winter). Bike trip duration seems to be heavily affected by the seasons as seen by the short duration of march when compared by Winter months indicating the beginning of spring. 

(f) Average monthly ride duration : customer types

```{r Average monthly ride duration : customer types, echo=FALSE, message=FALSE, warning=FALSE}
Average_monthly_customer_type_ride_duration <- clean_Cyclistic_trip_v1 %>%
  group_by(month, customer_type) %>%
  summarise(
    average_ride_duration = round(mean(ride_length), 2),
    .groups = "drop"
  ) %>%
  arrange(customer_type, desc(average_ride_duration))

Average_monthly_customer_type_ride_duration
```

Table shows that there is more long rides from the Casual customer group than the Member group.

Casual customers; Peaks in May. Ride duration follow seasonal trend. High duration during the late spring and early summer months, maybe due to the favorable weather indicating recreational or leisure usage, Slight decline in late summer and fall due to cooler weather Lowest duration during the winter months.

Member customers; Peaks in June. They have a consistent year-long, short trip duration, this indicate usage for commuting or piratical purposes instead of leisure.Ride duration follow seasonal trend.

Ride duration of Casual customers are more weather sensitive when compared to Member customers.

(g)Average monthly ride duration : bike type

```{r Average monthly ride duration : bike types, echo=FALSE, message=FALSE, warning=FALSE}
Average_monthly_bike_type_ride_duration <- clean_Cyclistic_trip_v1 %>%
  group_by(month, bike_type) %>%
  summarise(
    average_ride_duration = round(mean(ride_length), 2),
    .groups = "drop"
  ) %>%
  arrange(bike_type, desc(average_ride_duration))

Average_monthly_bike_type_ride_duration
```

Table shows that the favored bike type from most to least for long rides is the classic bike, Electric bike and the electric scooter except in the August where the pattern changes from most to least as classical bike, Electrical scooter and Electric bike.

Classic bike; Gradual increase late spring to summer (May to July (Peaks)): Warmer weather for leisure trips, Gradual decline late summer to fall (August to November) with lowest duration during the winter months: colder weather that discourages long rides.

Electric bike: Have consistent and low ride duration indicating utility focused usage and aligns with the classical bike seasonal trend.

Electrical scooter: Has a high average duration in August, higher than the Electric bike which suggests it's limited usage, possible for short distance leisure trips.

(h) Average monthly ride duration : customer types and bike type

```{r Average monthly ride duration : customer type and bike type, echo=FALSE, message=FALSE, warning=FALSE}
Average_monthly_customer_type_ride_duration_for_bike_types <- clean_Cyclistic_trip_v1 %>%
  group_by(month, customer_type, bike_type) %>%
  summarise(
    average_ride_duration = round(mean(ride_length), 2),
    .groups = "drop"
  ) %>%
  arrange(customer_type, bike_type, desc(average_ride_duration))

Average_monthly_customer_type_ride_duration_for_bike_types
```

Table shows that longer ride duration occur by the Casual customers than the Member type with Classic bike being the most favored bike type, followed by the electric bike and least favored, the electric scooter. Except in August where the electric scooter has a longer duration when compared to the electric bike.

Casual customer; Shows seasonal trend for the Classic bike with peak month - May and Least duration - January. They have shorter duration for the Electric bike with peak month - July and Least duration - January. Electric scooter have a negligible usage with peak month in August (longer duration than the Electric bike) and a decline in September.

Member customer; Maintain a consistent short trip duration year-round which indicates utility-focused and piratical rides. For classic bike, it peaks - May/June with Least duration - December. Electric bike has the shortest overall duration and peaks - July with Least duration - December.Electric scooter have a negligible usage with peak month in August (longer duration than the Electric bike) and a decline in September.  

Visualization on the Analysis of Cyclistic user's monthly ride duration 1

[![Tableau Visualization](https://github.com/Dr-Irtwange-NB/Cyclistic-bike-share-analysis/blob/main/images/Analysis%20of%20Cyclistic%20user's%20monthly%20ride%20durations%201.png?raw=true)](https://public.tableau.com/views/AnalysisofCyclisticusersmonthlyridedurations/AnalysisofCyclisticusersmonthlyridedurations1?:language=en-US&:embed=y&:sid=&:redirect=auth&:embed_code_version=3&:loadOrderID=8&:display_count=y&:origin=viz_share_link)

Bar chart shows the average monthly ride duration and distribution of each customer type. It shows changes in ride duration by seasonal trend 

Visualization on the Analysis of Cyclistic user's monthly ride duration 2

[![Tableau Visualization](https://github.com/Dr-Irtwange-NB/Cyclistic-bike-share-analysis/blob/main/images/Analysis%20of%20Cyclistic%20user's%20monthly%20ride%20durations%202.png?raw=true)](https://public.tableau.com/views/AnalysisofCyclisticusersmonthlyridedurations/AnalysisofCyclisticusersmonthlyridedurations2?:language=en-US&:embed=y&:sid=&:redirect=auth&:embed_code_version=3&:loadOrderID=9&:display_count=y&:origin=viz_share_link)

Bar chart shows the average monthly ride duration on each bike type and the distribution of each customer type bike preference. It shows changes in ride duration by seasonal trend.

From the above, when a whole year is grouped into a months, it's discovered that;

Highest trip duration occur during Summer and late spring with the least duration in Winter.Rides duration are shown to be affected by Chicago's seasonal pattern.

Casual customers seem to have longer trip duration mostly in warmer months and least in colder months indicating leisure and recreational purposes compared to the Member customers who maintain an almost constant year-round rides trip duration with slight increase in warmer months indicating commuting purposes.

When grouped by bike type, data shows that longer rides are taken with the classic bike type which peaks during the warmer months with a decline in Winter months.Showing  a preference for leisure or recreational rides with this bike type. The electric bike shows moderate, consistent and low ride duration which slightly increases during the warmer months showing it's preference for commuting purposes. The electric scooter shows duration in August and September only, with longer rides in August more than the electric bike this may due to maybe a promotion of bike type during these months, it's seasonality, a small niche, unavailability or disinterest. It's limited usage suggest possible use of bike for short distance leisure trips.

The Casual customers have longer rides during the warmer months and less during the winter months for all bike types showing that their interest in rides are mostly geared towards recreational purposes while the Member customers seem to maintain an almost constant year-round short trip duration among all bike types with a slight rise during the warmer months showing that their interest in rides are mostly geared towards commuting purposes. 

##### Week of each month

###### Week of each month rides

(a) all rides in the Week of each month 

```{r Week of each month rides : all, echo=FALSE, message=FALSE, warning=FALSE}
Number_of_weekly_monthly_rides <- clean_Cyclistic_trip_v1 %>%
  group_by(month, week_day) %>%
  summarise(
    total_number_of_rides = n(),
    .groups = "drop"
  ) %>%
  arrange(month, desc(total_number_of_rides))

Number_of_weekly_monthly_rides
```

Table shows that;

For each month, the highest number of rides are ; January-Wednesday, February-Thursday, March-Saturday, April-Tuesday, May-Saturday, June-Sunday, July-Saturday, August-Saturday, September-Monday, October-Wednesday, November-Thursday, December-Friday.

Saturdays dominate the highest ridership in many months, especially during spring and summer, reflecting increased leisure and recreational activities.

Tuesdays and Wednesdays are strong performers during non-peak months like April and October, likely due to consistent work commutes. Weekday Commutes Stay Consistent Year-Round

Winter (December–February) sees the lowest numbers, with January being the most impacted across all days of the week.

Sundays show peak ridership only in June, possibly due to summer activities.

Highest ridership day - August:Saturday, may indicate peak summer activity. Lowest ridership day - December:Sunday, may indicate winter and holiday season.

(b) Week of each month rides : customer types

```{r Week of each month rides : customer type, echo=FALSE, message=FALSE, warning=FALSE}
Number_of_weekly_monthly_Cyclistic_riders <- clean_Cyclistic_trip_v1 %>%
  group_by(month, customer_type,week_day) %>%
  summarise(total_number_of_rides = n(), .groups = "drop") %>%
  arrange(customer_type, desc(total_number_of_rides))

Number_of_weekly_monthly_Cyclistic_riders
```

Table shows that;

Casual Riders: Casual Riders Dominate Weekends showing higher participation on weekends, with Saturday as the most popular day across seasons and Sunday following with a slightly lower ridership. Peak ride counts often occur during weekends (Saturdays and Sundays) in warmer months (e.g., June, August), reflecting favorable weather conditions, increased leisure and recreational activities. Casual riders show the sharpest decline in winter months (December–February), especially on weekends.

Member Riders: Members are the Primary Riders consistently dominating the highest ridership across most months. Peak ride counts are commonly observed midweek (Tuesdays and Wednesdays), likely due to commuting patterns. They maintain a relatively stable ridership across all weekdays, with slight peaks on Wednesday and Thursday.Members continue using the service on weekdays during winter, albeit at lower levels.

Highest Ridership Month: August by Casual customers on Saturday.

Lowest Ridership Month: December by Casual customers on Monday.

(c) Week of each month rides : bike types

```{r Week of each month rides : bike type, echo=FALSE, message=FALSE, warning=FALSE}
Number_of_weekly_monthly_Cyclistic_Bike_type <- clean_Cyclistic_trip_v1 %>%
  group_by(month, week_day, bike_type) %>%
  summarise(
    total_number_of_bikes = n(),
    .groups = "drop"
  ) %>%
  arrange(bike_type, desc(total_number_of_bikes))


Number_of_weekly_monthly_Cyclistic_Bike_type
```

Table shows that;

For both classic bikes and electric bikes, weekends (Saturday and Sunday) tend to show higher ride counts.

Classic bike: The highest number of classic bike rides occurred in August-Saturday and the second-highest peaks on July-Saturday. This suggests that Saturday might be the most popular day for people to ride classic bikes, especially in the summer months, indicating a strong trend for Weekends. Weekdays, particularly Wednesday and Thursday, show high to moderate peaks, indicating that many people may be using electric bikes for commutes or other purposes on these days. Saturday and Sunday are the busiest days for classic bikes. Generally there's a higher ride count compared to the electric bikes.All this suggest that Classic bikes are popular for leisure rides, exercise/recreational activities or commutes purposes.

Electric bike: The highest number of electric bike rides occurred on August-Friday followed by August-Saturday. June to October (Warmer months) are the months with generally higher electric bike usage. Weekdays, particularly Wednesday and Thursday, show moderate peaks, indicating that many people may be using electric bikes for commutes or other purposes. Saturday also sees significant usage in the summer months.In conclusion, Electric bikes gain more traction in the warmer months (June to October) possibly due to the warm weather and commuter trends.

Electric scooters: are relatively less popular in comparison showing low numbers across all months (August and September), indicating that either the data set doesn't capture many scooter rides or that these bikes are used more intermittently (less commonly) and on specific days.Peaks on September-Thursday, Lowest on August-Saturday. This may also be due to may be due to a promotional or a survey event indicated by a month worth of data on bike type.

(d)  Week of each month rides : customer and bike types

```{r Week of each month rides : customer and bike type, echo=FALSE, message=FALSE, warning=FALSE}
Number_of_weekly_monthly_Cyclistic_customers_type_Bike_type <- clean_Cyclistic_trip_v1 %>%
  group_by(month, week_day, customer_type, bike_type) %>%
  summarise(
    total_number_of_bikes = n(),
    .groups = "drop"
  ) %>%
  arrange(customer_type, bike_type, desc(total_number_of_bikes))


Number_of_weekly_monthly_Cyclistic_customers_type_Bike_type
```

Table shows that;

Casual Riders:

Tend to prefer Classic bikes, with their highest ridership on weekends (Saturday and Sunday), suggesting leisure and recreational usage. Highest ridership in Summer, August (Saturday).

Electric bikes are popular during spring and summer weekends. Highest ridership in August (Saturday)

Electric scooters have limited but concentrated use during the Autumn/fall (September), possibly for short trips or fun rides in pleasant weather. Highest ridership in September (Sunday) 

Casual riders are less active on weekdays and cold months, but their usage increases during weekends and warmer months. This reflects their leisurely approach.

Members Riders:

Use classic bikes predominantly, especially during weekdays. This suggests commuting use. Highest ridership in July (Wednesday) followed by  May (Wednesday).

Electric bikes are commonly used by members during weekdays as well, indicating a preference for commutes. Highest ridership in October (Wednesday).

Members use electric scooters minimally, mostly in September. These might be experimental or supplementary mode of transport. Highest ridership in September (Thursday).

Members are active year-round but more on weekdays. There's a slight increase in ridership during warmer months. This reflects their purposeful or commuting use of bikes.Highest ridership in July (Wednesday).

Visualization of the Analysis of Cyclistic user's Week of each month ride 1

[![Tableau Visualization](https://github.com/Dr-Irtwange-NB/Cyclistic-bike-share-analysis/blob/main/images/Analysis%20of%20the%20Week%20of%20each%20month%20Cyclistic%20user's%20ride%201.png?raw=true)](https://public.tableau.com/views/AnalysisofCyclisticusersWeekofeachmonthride/AnalysisoftheWeekofeachmonthCyclisticusersride1?:language=en-US&:embed=y&:sid=&:redirect=auth&:embed_code_version=3&:loadOrderID=10&:display_count=y&:origin=viz_share_link)

Bar charts shows distribution of rides of week in each month and comparison between customer type.

Visualization of the Analysis of Cyclistic user's Week of each month ride 2

[![Tableau Visualization](https://github.com/Dr-Irtwange-NB/Cyclistic-bike-share-analysis/blob/main/images/Analysis%20of%20the%20Week%20of%20each%20month%20Cyclistic%20user's%20ride%202.png?raw=true)](https://public.tableau.com/views/AnalysisofCyclisticusersWeekofeachmonthride/AnalysisoftheWeekofeachmonthCyclisticusersride2?:language=en-US&:embed=y&:sid=&:redirect=auth&:embed_code_version=3&:loadOrderID=11&:display_count=y&:origin=viz_share_link)
  
Bar chart shows distribution of bike type rides by week in each month and comparison between customer type.

From the above, when a whole year is grouped into a weeks in each months, it's discovered that;
Highest number of rides occur during Summer and Autumn with less ride in Winter as rides are shown to be affected by Chicago's seasonal pattern.

Casual customers dominate weekends. Ridership is dependent on the weather pattern shown by high ridership during warmer months and a steep decline during colder months year-round. This indicate use of service for mostly leisure and recreational activities while Member customer are the primary ridership and maintain high almost constant rides year-round with little changes from weather pattern when compared to the Casual members. This indicate use of service piratical and commuting purposes.

When grouped by bike type, data shows that Most rides are taken with the classic bike type which peaks during the warmer months with a decline in Winter months.Showing  a preference for leisure or recreational rides with this bike type. The electric bike shows moderate increase in rentals during the warmer months showing it's preference for commuting purposes. The electric scooter shows rentals in August (Saturday) and September only, this may due to it's seasonality, a small niche, unavailability or disinterest or promotional activities.

The Casual customers have more rides during the warmer months and less during the winter months for all bike type showing that their interest in rides are mostly geared towards recreational purposes while the Member customers seem to maintain an almost constant year-round rides among all bike types with a slight rise during the warmer months showing that their interest in rides are mostly geared towards commuting purposes.

###### Week of each month ride durations

(e) Average Week of each month ride duration : All Cyclistic riders

```{r Average Week of each month ride duration : All, echo=FALSE, message=FALSE, warning=FALSE}
Average_weekly_monthly_ride_duration <- clean_Cyclistic_trip_v1 %>%
  group_by(month,week_day) %>%
  summarise(
    average_ride_duration = round(mean(ride_length), 2),
    .groups = "drop"
  ) %>%
  arrange(desc(average_ride_duration))

Average_weekly_monthly_ride_duration
```

Table shows that;

May (peak), followed by July, June, April and August dominates the top spots for average ride duration. Saturday and Sunday (weekends) account for the longest average ride duration Suggesting a preference for longer rides during non-working days, with rides consistently above 24 minutes in these months.This suggest that longer leisure or casual rides takes place during warm months (May to August) and weekends,Longer duration are consistent with favorable weather conditions and potentially higher tourist or user activity. This indicates an uptick in recreational usage during spring (May) and summer (June-August) Months.

On weekdays (Monday-Friday) Shorter duration dominate, with average ride duration generally staying below 20 minutes. This trend suggests more functional or commuter-oriented rides on workdays.

Shorter average duration are evident in cooler months like October, November, December, and January. Rides are shorter across all days of the week, reflective of colder weather and a shift in user behavior toward more practical, shorter trips. This highlight reduced leisure activity during colder weather. Winter weekdays consistently fall below 15 minutes, showing a stark contrast to summer weekends.

(f) Average Week of each month ride duration : customer types

```{r Average Week of each month ride duration : customer types, echo=FALSE, message=FALSE, warning=FALSE}
Average_weekly_monthly_customer_type_ride_duration <- clean_Cyclistic_trip_v1 %>%
  group_by(month, customer_type,week_day) %>%
  summarise(
    average_ride_duration = round(mean(ride_length), 2),
    .groups = "drop"
  ) %>%
  arrange(customer_type, desc(average_ride_duration)) 

Average_weekly_monthly_customer_type_ride_duration
```

Table shows that;

Casual Users: They have the longest ride duration seen in the data set and primarily on the weekends aligning with leisure or recreational activities. Long ride duration occur in Spring (April-May) and Summer (June-August) particularly on weekends, indicating warm or agreeable weather that encourages outdoor activities and leisure rides. In the Fall and Winter (October-January), ride duration drops with the shortest on January (Friday) reflecting reduced leisure travel during colder months. Casual users show a strong seasonal pattern with longer rides in warmer months (spring/summer) and shorter rides in colder months.

Member Users: They have significantly shorter average ride duration than casual rides, showing minor variation between weekdays and weekends, suggesting a commuter or functional use. Longest rides are recorded on May (Saturday),June (Sunday) and July (Saturday). They maintain relatively consistent ride duration throughout the year, with slight seasonal variation. The Winter months (December-February) have some of the shortest ride duration, for e.g December (Tuesday and Wednesday) and November (Tuesday) which Suggests that members primarily use the service for commuting purposes. Member rides are shorter across all days, but slight peaks occur on weekends, reflecting some recreational use. Members ride duration are relatively stable, with minor seasonal dips.

(g) Average Week of each month ride duration : bike types

```{r Average Week of each month ride duration : bike types, echo=FALSE, message=FALSE, warning=FALSE}
Average_weekly_monthly_bike_type_ride_duration <- clean_Cyclistic_trip_v1 %>%
  group_by(month, bike_type,week_day) %>%
  summarise(
    average_ride_duration = round(mean(ride_length), 2),
    .groups = "drop"
  ) %>%
  arrange(bike_type, desc(average_ride_duration)) 


Average_weekly_monthly_bike_type_ride_duration
```

Table shows that;

Classic Bikes: Popular for longer rides, especially during weekends and in summer months. Peak usage observed in spring and summer (April to July), there's longer weekend trips and stable weekday trip duration, indicating strong usage for recreational purposes during the weekends and commute during weekdays. Classic bikes are favored during warmer months for longer rides with it's usage declining slightly in December and January but still maintains steady activity, suggesting dedicated riders. 
 
Electric Bikes: Shows moderate and consistent distances across all months, with slight increases during warm months. Peaks trips occur in summer months (May to July). There's a steady usage throughout the week, with no sharp rise or drop on weekends. Data shows a Minimal seasonal fluctuation (slight dips observed in December and January) suggesting reliability and preference of bike type in all-weather as a frequent commuting option for shorter trips. Electric Bikes have steady year-round use, suggesting a practical, utility-driven preference for shorter distances.

Electric Scooters: limited data (data only in August(Saturday) and September) may indicate that electric scooters are likely a newer addition, Unpopular ride option or a  seasonal variation due to weather sensitivity (used only in warmer months). It may also indicate a promotional or a survey event due to the month worth of data on bike type. There's moderate usage on weekends but lower on weekdays. This suggest a mix of recreational and utility purposes.

(h) Average Week of each month ride duration : customer type and bike type

```{r Average Week of each month ride duration : customer type and bike type, echo=FALSE, message=FALSE, warning=FALSE}
Average_weekly_monthly_customer_type_ride_duration_for_bike_types <- clean_Cyclistic_trip_v1 %>%
  group_by(month, customer_type, bike_type,week_day) %>%
  summarise(
    average_ride_duration = round(mean(ride_length), 2),
    .groups = "drop"
  ) %>%
  arrange(customer_type, bike_type, desc(average_ride_duration))


Average_weekly_monthly_customer_type_ride_duration_for_bike_types
```
Table shows that;

Casual customers have longer ride duration when compared to the Member customers. Long trips occur on weekends especially during the warmer months and shows seasonal pattern, There's little variation in trip duration in the week of each month. Classic bikes are preferred for longer trips, Electric bikes for moderate trips and Electric scooters for shorter trips.

Member customers also show longer to least trip duration on the classic bike, electric bike and electric scooter respectively.

For both customer types on their preferred bike types, it's shown that longer trip duration occur on the Classic bikes, moderate duration on the electric bike and the least duration on the electric scooter except in August for the member customers where the electric scooter has a longer duration when compared to the electric bike.  

Visualization on the Analysis of Cyclistic user's Week of each month ride duration 1

[![Tableau Visualization](https://github.com/Dr-Irtwange-NB/Cyclistic-bike-share-analysis/blob/main/images/Analysis%20of%20Cyclistic%20user's%20Week%20of%20each%20month%20ride%20durations%201.png?raw=true)](https://public.tableau.com/views/AnalysisofCyclisticusersWeekofeachmonthridedurations/AnalysisofCyclisticusersWeekofeachmonthridedurations1?:language=en-US&:embed=y&:sid=&:redirect=auth&:embed_code_version=3&:loadOrderID=12&:display_count=y&:origin=viz_share_link)

Bar chart shows distribution of trip duration by week in each month and comparison between customer type.

Visualization on the Analysis of Cyclistic user's Week of each month ride duration 2

[![Tableau Visualization](https://github.com/Dr-Irtwange-NB/Cyclistic-bike-share-analysis/blob/main/images/Analysis%20of%20Cyclistic%20user's%20Week%20of%20each%20month%20ride%20durations%202.png?raw=true)](https://public.tableau.com/views/AnalysisofCyclisticusersWeekofeachmonthridedurations/AnalysisofCyclisticusersWeekofeachmonthridedurations2?:language=en-US&:embed=y&:sid=&:redirect=auth&:embed_code_version=3&:loadOrderID=13&:display_count=y&:origin=viz_share_link)

Bar chart shows distribution of bike type duration by week in each month and comparison between customer type.

From the above, when a whole year is grouped into a weeks in each months, it's discovered that;

Longest ride occurs during the spring (May) and summer (June-August) Months especially on weekends of these months which may an increase in  trip duration for recreational usage and shorter rides during the colder months which indicates a decrease in trip duration for piratical usage e.g, commuting.

Casual users show a strong seasonal pattern with longer rides in warmer months (spring/summer) and shorter rides in colder months indicating rides for primarily recreational purposes while Members ride duration are relatively stable, with minor seasonal dips indicating rides for piratical purposes.

When grouped by bike type, data shows that, for the Classic bikes there's longer weekend trips and stable weekday trip duration, indicating strong usage for recreational purposes during the weekends and commute during weekdays. Electric bike shows a moderate and consistent duration year-round, throughout the weeks indicating steady year-round use, suggesting a practical, utility-driven preference for shorter distances while the Electric scooter has limited data (month worth) which may be due to a promotional/survey event, seasonality, small niche or disinterest. It shows moderate usage on weekends but lower on weekdays. This suggest a mix of recreational and utility purposes.

When grouped by customer bike preference, it shows that both customer types have longer trip duration occur on the Classic bikes, moderate duration on the electric bike and the least duration on the electric scooter except in August for the member customers where the electric scooter has a longer duration when compared to the electric bike.

##### Determine Ride and ride durations based on a 24hrs bin or clock

(a) General Rides and average ride duration by time within 24hrs.

First change time format into a 24hr clock bin using started_at (in POSIXct format) for easy analysis and group data 

(i) all

```{r Rides and average ride duration : all, echo=FALSE, message=FALSE, warning=FALSE}

# Group data by hour and summarize statistics

general_time_stats <- clean_Cyclistic_trip_v1 %>%
  mutate(hour = format(started_at, "%H")) %>%  # Extract hour as a grouping variable
  group_by(hour) %>%
  summarise(
    total_rides = n(),  # Count the number of rides
    avg_ride_duration = round(mean(ride_length, na.rm = TRUE), 2)  # Average ride length, rounded to 2 decimal places
  ) %>%
  arrange(hour)  # Arrange the results by hour

# based on customer_types

general_time_stats_cus <- clean_Cyclistic_trip_v1 %>%
  mutate(hour = format(started_at, "%H")) %>%  # Extract hour as a grouping variable
  group_by(hour,customer_type) %>%
  summarise(
    total_rides = n(),  # Count the number of rides
    avg_ride_duration = round(mean(ride_length, na.rm = TRUE), 2)  # Average ride length, rounded to 2 decimal places
  ) %>%
  arrange(hour, customer_type)  # Arrange the results by hour

general_time_stats
```

Table shows that;

When whole year is summarized into a 24 hour bin'

Total Rides Trends: Peak hours for the Morning are at 8 (8AM), 7(AM), 6(6AM) which may indicate increase ridership to commute and in the evenings at 17 (5PM), 16 (4PM), 18 (6PM) which may indicate return from commute and rides for leisure. In 24hours, the Highest ridership is at 17(5PM) which may indicate rides for recreational activities or leisure and the lowest at 4(4AM) which may indicate rest time of most customers. Ridership decreases greatly from 10PM to 4AM.

Ride Duration Trends: Rides tend to last longer during mid-morning 10 (10AM) to early afternoon (14)2PM, peaking at 11 (11 AM). Short trips occur from 5 to 8 (5-8AM) and least duration at 6 (6AM) in the early morning. 

Overall, Peak duration in the morning occur at 11 (11AM) and peak in the evenings at 13 (1PM).

There appears to be an inverse relationship between total rides and average ride duration. When total rides are high, average duration are relatively short indicating a focused, piratical or time sensitive use e.g commuting to work. Also during off-peak hours, such as mid-morning and early afternoon, ride duration slightly increases despite fewer rides being recorded indicating recreational or leisure rides and trips.

There's a steady decline in total rides but relatively stable ride duration during evening hours 20-23(8-11PM) with minimal activity at late night hours 00-05(12-5AM)  with average duration slightly higher than morning rush hours, possibly reflecting leisure or longer, planned trips.

Longer average ride durations are observed during off-peak hours (00:00 - 05:00), likely reflecting leisure trips or longer commutes when roads are less congested. Shorter durations during peak hours (08:00 - 18:00) suggest frequent but short trips, possibly related to work commutes or errands.

Hourly Patterns in Ridership and Ride Duration

1. Morning Rush (7:00 AM - 9:00 AM)

Ridership: This is the peak ridership morning period. People are commuting to work or school, leading to a significant spike in ridership.

Ride Duration: Ride durations during these hours are shorter. The trips are quick and purpose-driven, often focused on getting to a destination efficiently.

Key Insight: Short ride durations could indicate no congestion or short routes being taken, favorable weather conditions or sufficient infrastructure (roads,  bike lanes).

2. Mid-Morning (9:00 AM - 12:00 PM)

Ridership: Ridership during mid-morning continues to increase after the rush hour. This may be due to increase in some commuters or people running errands.

Ride Duration: Average ride duration increase as people take less time-sensitive trips, such as appointments or casual errands.

Key Insight: An increase in ride duration, combined with an increase in ridership, suggests that people are not in a rush and are using the service for leisure or non-time constraint purposes.

3. Afternoon (12:00 PM - 5:00 PM)

Ridership: There is be a second peak in ridership, ridership continues to increase during the lunch hour and peaks (5PM) later in the afternoon. This may be because commuters head back from errands, lunch breaks or casual trips.

Ride Duration: Ride duration plateaus and gradually declines during this period as activities such as leisure trips, commutes from errands and work begin to decline.

Key Insight: Ridership increases and spikes during this period, indicating people may be making personal trips, running errands or taking pleasure rides during work hours.

4. Evening Rush (5:00 PM - 7:00 PM)

Ridership: The evening rush is another peak period (5PM). Much like the morning rush (8AM), this is when commuters are heading home, so you see high ridership numbers which peaks and a gradual decline.

Ride Duration: Ride durations are shorter, similar to the morning rush. People are generally focused on getting home quickly.

Key Insight: This is the busiest time as the highest ridership peaks at 5PM. A slightly longer duration as compared to the morning peak may be attributed to high number of riders on roads, congestion or people not in a rush.

5. Late Evening (7:00 PM - 10:00 PM)

Ridership: sharp decline in ridership as most people have finished their daily activities. This period often reflects more social and leisure trips.

Ride Duration: Average ride durations decreases with the lowest at 9PM in the late evening as people may be taking shorter errands or leisure trips, heading home, to restaurants, events, or social gatherings.

Key Insight: A decrease in duration with a sharp drop in ridership is typical for this time of day, indicating that while fewer people are using the service, those who do may be traveling for non-commuting reasons.

6. Late Night (10:00 PM - 3:00 AM)

Ridership: Ridership during this period is typically very low. People are either home or heading out for late-night activities like socializing or events.

Ride Duration: There's a gradual increase in ride duration,this may indicate a more casual or planned in advance late-night trips with little or no time constraints.

Key Insight: Although the ridership is low, this period might be valuable for people who need transport for specific activities (e.g., nightlife, emergency rides).

7. Early Morning (3:00 AM - 7:00 AM)

Ridership: Ridership declines, is minimal at 4AM with a gradual increase, often limited to people heading to early jobs, late-night workers, or early travelers.

Ride Duration: Long ride duration which sharply declines as rides may become more utilitarian as hour passes.

Key Insight: ridership during early morning hours indicate specific demographic needs, such as workers in certain industries (e.g., healthcare, transport) who have non-traditional hours.

(ii) customer type

```{r general_time_stats_cus, echo=FALSE, message=FALSE, warning=FALSE}

general_time_stats_cus
```

Table shows that;

In general, the largest ridership is by Member customers at 5PM and least ridership by Casual customers at 4AM with the longest trip duration by Casual customers at 10AM and least duration by Member customer at 6AM. 

For each customer types:

Casual customer: 

Ride counts are generally lower than members except from 11PM-3AM with their average ride duration consistently higher than the member customers, peaking at 10AM.The lower ridership and longer duration may likely reflects leisure trips, extended travel or tourists exploring at a relaxed pace.Ridership increase steadily, peaking at 5PM  with slightly longer duration (~19 minutes). Decrease in Ridership by 11PM-3AM with Longer duration at this hours may indicate rest/leisure hours for Casual customers, rush/rest hours from 5AM-8AM where shorter trip duration to slightly increasing ridership.

Casual riders reflect more recreational use (longer trips concentrated in leisure hours).

Member customers:

Dominate Ridership during peak commuting hours, especially 6–9AM and 4-6PM with their average trip duration shorter compared to the Casual customers indicating efficiency and purposeful trips (e.g., commuting).Trip duration peaks at 3-4AM with least ridership at these hours indicative of leisure trips, routine travel or errands.   Inverse Relationship is seen where a high ride counts coincide with short duration (e.g., rush hours), while low ride counts align with longer duration (e.g., leisure hours).

Members are primarily commuters (short, frequent trips during rush hours).

Visualizations on the Analysis of Cyclistic rides and ride duration based on hourly usage patterns

[![Tableau Visualization](https://github.com/Dr-Irtwange-NB/Cyclistic-bike-share-analysis/blob/main/images/Analysis%20of%20Cyclistic%20rides%20and%20ride%20duration%20based%20on%20hourly%20usage%20patterns.png?raw=true)](https://public.tableau.com/views/AnalysisofCyclisticridesandridedurationbasedonhourlyusagepatterns/AnalysisofCyclisticridesandridedurationbasedonhourlyusagepatterns?:language=en-US&:embed=y&:sid=&:redirect=auth&:embed_code_version=3&:loadOrderID=14&:display_count=y&:origin=viz_share_link)

This line chart shows hourly usage pattern for ridership, average ride duration and comparison with customer types.

(b) General Rides and average ride duration by time weekly.

(i) all

```{r Rides and average ride duration : weekly, echo=FALSE, message=FALSE, warning=FALSE}

# Weekly data frame: Rides and average ride duration grouped by day of the week and time of day

weekly_time_stats <- clean_Cyclistic_trip_v1 %>%
  mutate(hour = format(started_at, "%H")) %>%  # Extract hour as a grouping variable
  group_by(hour,week_day) %>%
  summarise(
    total_rides = n(),  # Count the number of rides
    avg_ride_duration = round(mean(ride_length, na.rm = TRUE), 2)  # Average ride length, rounded to 2 decimal places
  ) %>%
  arrange(hour, week_day)  # Arrange the results by hour

# based on customer types

weekly_time_stats_cus <- clean_Cyclistic_trip_v1 %>%
  mutate(hour = format(started_at, "%H")) %>%  # Extract hour as a grouping variable
  group_by(hour,week_day,customer_type) %>%
  summarise(
    total_rides = n(),  # Count the number of rides
    avg_ride_duration = round(mean(ride_length, na.rm = TRUE), 2)  # Average ride length, rounded to 2 decimal places
  ) %>%
  arrange(hour, week_day,customer_type)  # Arrange the results by hour

weekly_time_stats
```

Table shows that;

In general, the largest ridership is on Wednesday at 5PM and least ridership by on Tuesday at 3AM with the longest trip duration on Tuesday at 3AM and least duration on Monday at 6AM.

Two (2) Ride peaks in data set, in the morning at 8AM, and in the evening at 5PM indicating rush hours at weekdays. Weekends peaks around midday (11AM-3PM) suggesting leisure or recreational activities. 

For week:

Sunday- Ridership Peaks at 3PM and least at 4AM, Duration peaks at 6AM and least at 9PM.

Monday- Ridership Peaks at 5PM and least at 3AM, Duration peaks at 2AM and least at 6AM.

Tuesday- Ridership Peaks at 5PM and least at 3AM, Duration peaks at 3AM and least at 5AM.

Wednesday- Ridership Peaks at 5PM and least at 3AM, Duration peaks at 10AM and least at 6AM.

Thursday- Ridership Peaks at 5PM and least at 3AM, Duration peaks at 12AM and least at 5AM.

Friday- Ridership Peaks at 5PM and least at 3AM, Duration peaks at 3AM and least at 6AM

Saturday- Ridership Peaks at 1PM and least at 4AM, Duration peaks at 1PM and least at 6AM

From late night to early morning (12-5AM), more ridership on weekends with longer duration suggesting leisure or recreational activities and minimal ridership with shorter duration on weekdays suggesting late night commutes. 

During the early morning rush (6-9AM), More rides peaking at 8AM on weekdays with shorter duration indicating commuting patterns. Lower rides with longer duration on weekends reflecting leisure trips.

At mid morning (10-11), Moderate rides and duration on weekdays suggesting errands or non-commute trips. Spike in ridership with long duration suggesting leisure/recreational activities.

From midday to early afternoon (12-3PM), Consistent ride counts with shorter trips on weekdays reflecting routine trips like errands. Significant spike in ridership on weekends with long duration indicating recreational rides.

During the evening rush (4-6PM), Peak ridership with shorter duration indicating commuting purposes. On weekends, there's a steady decline in rides and ride duration (higher than weekdays) indicating leisure or social activities.

At night (7-11PM), There's a steep decline in ridership with alternating but short duration on weekdays suggesting commuting or essential travel. On weekends, decline in rides but higher ridership than weekdays with longer duration indicating social or recreational usage.

(ii) customer type

```{r weekly_time_stats_cus, echo=FALSE, message=FALSE, warning=FALSE}

weekly_time_stats_cus
```

Table shows that;

In general, the largest ridership for customer type is by the member type on Wednesday at 5PM and least ridership by member type on Tuesday at 3AM with the longest trip duration by Casual customers on Sunday at 6AM and least duration by the Member customer on Monday at 6AM.

Data shows that on weekdays, there's more Member ridership to Casual users and on weekends, more Casual ridership to Member users, With longer Casual users trip duration compared to an almost consistent shorter Member duration. 

For customer type in each week:

Casual customers:

Highest ridership: Saturday at 3PM. Least ridership: Tuesday at 3AM.

Longest duration: Sunday at 6AM. Least duration: Tuesday at 5AM.

They exhibit higher activity late in the day

They display small ridership but with a significantly longer duration when compared to the member type on weekdays and more ridership and longer duration than member type on weekends, indicating use of bikes mostly for leisure, outings and recreational activities.

On weekdays from 5AM TO 8AM, there's a significant decrease in ride duration compared to the steady increase in ridership indicating a goal oriented or focused utility of bike e.g to commute or run errands.

Member customers:

Highest ridership: Wednesday at 5PM. Least ridership: Tuesday at 3AM.

Longest duration: Tuesday at 3AM. Least duration: Monday at 6AM.

Dominate ridership, especially during commuting hours showing surges during the rush hours at 8AM and 5PM  with shorter consistent average ride duration compared to casual users reflecting efficiency and utility/purpose-driven rides, such as commuting or errands usage on weekdays. This reinforces a workday-driven pattern. On weekends, there's Lower ridership compared to Casual users, but duration slightly increase, possibly reflecting leisure or fitness usage. 

Visualization on the Analysis of Cyclistic users weekly rides based on hourly usage patterns.

[![Tableau Visualization](https://github.com/Dr-Irtwange-NB/Cyclistic-bike-share-analysis/blob/main/images/Analysis%20of%20%20Cyclistic%20users%20weekly%20rides.png?raw=true)](https://public.tableau.com/views/AnalysisofCyclisticusersweeklyridesandridesdurationsbasedonhourlyusuagepatterns/AnalysisofCyclisticusersweeklyrides?:language=en-US&:embed=y&:sid=&:redirect=auth&:embed_code_version=3&:loadOrderID=15&:display_count=y&:origin=viz_share_link)

This line chart shows hourly usage pattern for ridership and comparison with customer types for a whole year in a week.

Visualization on the Analysis of Cyclistic users rides duration based on hourly usage patterns. 

[![Tableau Visualization](https://github.com/Dr-Irtwange-NB/Cyclistic-bike-share-analysis/blob/main/images/Analysis%20of%20%20Cyclistic%20users%20weekly%20rides%20durations.png?raw=true)](https://public.tableau.com/views/AnalysisofCyclisticusersweeklyridesandridesdurationsbasedonhourlyusuagepatterns/AnalysisofCyclisticusersweeklyridesdurations?:language=en-US&:embed=y&:sid=&:redirect=auth&:embed_code_version=3&:loadOrderID=16&:display_count=y&:origin=viz_share_link)

This line chart shows hourly usage pattern for ride duration and comparison with customer types for a whole year in a week.

(c) General Rides and average ride duration by time monthly.

(i) all

```{r Rides and average ride duration : monthly, echo=FALSE, message=FALSE, warning=FALSE}
# Monthly data frame: Rides and average ride duration grouped by month and time of day

monthly_time_stats <- clean_Cyclistic_trip_v1 %>%
  mutate(hour = format(started_at, "%H")) %>%  # Extract hour as a grouping variable
  group_by(hour,month) %>%
  summarise(
    total_rides = n(),  # Count the number of rides
    avg_ride_duration = round(mean(ride_length, na.rm = TRUE), 2)  # Average ride length, rounded to 2 decimal places
  ) %>%
  arrange(hour, month)  # Arrange the results by hour

# based on customer types

monthly_time_stats_cus <- clean_Cyclistic_trip_v1 %>%
  mutate(hour = format(started_at, "%H")) %>%  # Extract hour as a grouping variable
  group_by(hour,month,customer_type) %>%
  summarise(
    total_rides = n(),  # Count the number of rides
    avg_ride_duration = round(mean(ride_length, na.rm = TRUE), 2)  # Average ride length, rounded to 2 decimal places
  ) %>%
  arrange(hour, month,customer_type)  # Arrange the results by hour

monthly_time_stats
```

Table shows that;

In general, the largest ridership for months is July at 5PM and least ridership January at 3AM with the longest trip duration is June at 4AM and least duration March at 6AM.

Data shows that, Most ridership occur from late spring (May), summer (June, July and August) and autumn (September and October) during the evenings by (4PM to 6PM). Least ridership occurs in the winter (December, January and February), spring (March and April) and late autumn (November) during the early mornings (2 to 4 AM).

All months Ridership peaks at 8AM, 12PM and 5PM. Duration peaks in the mornings (AM)

For each month:

January-Most ridership: 3AM  and least ridership: 5PM. Most duration: 2AM and least duration: 8AM.

February-Most ridership: 5PM and least ridership: 3AM. Most duration: 2AM and least duration: 12AM.

March-Most ridership: 5PM and least ridership: 3AM. Most duration: 4AM and least duration: 6AM.

April-Most ridership: 5PM and least ridership: 3AM. Most duration: 2AM and least duration: 5AM.

May-Most ridership: 5PM and least ridership: 4AM. Most duration: 10AM and least duration: 6AM.

June-Most ridership: 5PM and least ridership: 4AM. Most duration: 4AM and least duration: 6AM.

July-Most ridership: 5PM and least ridership: 4AM. Most duration: 11AM and least duration: 6AM.

August-Most ridership: 5PM and least ridership: 4Am. Most duration: 3AM and least duration: 6AM.

September-Most ridership: 5PM and least ridership: 4AM. Most duration: 10AM and least duration: 5AM.

October-Most ridership: 5PM and least ridership: 4AM. Most duration: 11AM and least duration: 5AM.

November-Most ridership: 5PM and least ridership: 4AM. Most duration: 3AM and least duration: 5AM.

December-Most ridership: 4PM and least ridership: 3AM. Most duration: 2AM and least duration: 6AM.

Based on Chicago seasons

Winter (December - February):

Cold Weather and Snow:

Chicago winters are harsh, with average temperatures often below freezing and frequent snowstorms. These conditions explain the significant drop in ride counts during these months.

January consistently has the lowest ridership, with early morning hours (12-5 AM) seeing near negligible activity.

Shorter Days: Limited daylight reduces the number of evening rides. Peak commute periods (morning and evening) still exist but are much smaller than during warmer months.

Longer Ride Durations:

The few users braving the cold may have limited alternatives or are engaging in necessary commutes. This is reflected in the slightly longer ride durations for early hours and off-peak times.

Spring (March - May):

Rising Temperatures and Daylight:

With temperatures climbing to more comfortable levels, ride counts steadily increase.

May sees a sharp surge in usage, reflecting customers enthusiasm for outdoor activities as the city warms.

Morning Commutes Grow: Ride counts during 7-9 AM hours spike as commuting returns to full swing.

Longer Durations in Early Spring:

March and early April have ride durations ~20+ minutes during midday, likely due to slower pace as people reacquaint themselves with outdoor cycling.

Summer (June - August):

Ideal Biking Weather:

Chicago summers are warm and humid, with long daylight hours, making this the busiest months for biking.

July and August consistently register the highest ride volumes.

Recreational and Social Usage:

Shorter Durations in Daytime reflecting frequent, short commutes or errands.

Midday and evening hours show high usage, indicating significant leisure activity along maybe scenic trails.

Durations remain steady (~15-18 minutes) for commute hours but increase (~20-25 minutes) in early mornings (3-4 AM), pointing to recreational trips.

Evening Commutes Peak:
Longer days allow for high evening commute volumes until 8PM.

Autumn/Fall (September - November):

Cooling Down:

September maintains high ride counts, but as temperatures drop in October and November, usage declines.

October evenings (5-6 PM) still see significant activity, with high ride counts.

Shorter Durations in November:

Average ride duration drops, reflecting more focused, weather-conscious commuting.

Based on seasonal hourly pattern

Early Morning (12-5 AM):

Minimal activity shown by sparse rides during these hours.A slight spikes in summer suggest recreational or event-related trips.

Morning Commutes (6-10 AM):

Prominent in all seasons. Morning rides are relatively robust year-round, even during winter months, as many rely on biking for short commutes. Peak hours (7-9 AM) align with work start times. Winter months show low durations, while summer high durations consistent with commute-focused usage.

Midday (10 AM - 3PM):

Midday rides dominate during summer months, reflecting recreational/leisure activities. Winter midday activity is significantly lower, maybe due to cold temperatures and fewer recreational rides.


Evening Commutes (4-8 PM):

Evening commute spikes reflecting vibrant downtown workforce returning home.Summer peaks extend until 8 PM due to longer daylight, while winter evenings taper off earlier as darkness and cold deter usage.

Late Night (9-11 PM):
Summer late-night rides remain steady, possibly from events or outings. In winter, late-night activity drops sharply.

(ii) customer type

```{r monthly_time_stats_cus, echo=FALSE, message=FALSE, warning=FALSE}

monthly_time_stats_cus
```

Table shows that;

In general, the largest ridership for months is September by Member customers at 5PM and least ridership January by Casual customers at 4AM with the longest trip duration in March by Casual customers at 4AM and least duration in March by Member customers at 6AM.

Data shows that, Most ridership occur from late spring (May), summer (June, July and August) and autumn (September and October) during the evenings by (4PM to 6PM) dominated by the Member customers. Least ridership occurs in the winter (December, January and February), spring (March and April) and late autumn (November) during the early mornings (2 to 4 AM) dominated by the Casual customers.

Highest durations in general is by the Casual customers and least durations is dominated by the Member customers.

For each month:

January-Most ridership: Member at 5PM  and least ridership: Casual by 4AM. Most duration: Casual by 2AM and least duration: Casual by 5AM.

February-Most ridership: Member at 5PM and least ridership: Casual at 4AM. Most duration: Casual at 1AM and least duration: Member at 3AM.

March-Most ridership: Member at 5PM and least ridership: Casual at 4AM. Most duration: Casual at 4AM and least duration: Member at 6AM.

April-Most ridership: Member at 5PM and least ridership: Casual at 4AM. Most duration: Casual at 10AM and least duration: Member at 5AM.

May-Most ridership: Member at 5PM and least ridership: Casual at 4AM. Most duration: Casual at 10AM and least duration: Member at 6AM.

June-Most ridership: Member at 5PM and least ridership: Member at 3AM. Most duration: Casual 3AM and least duration: Member at 6AM.

July-Most ridership: Member at 5PM and least ridership: Member at 3AM. Most duration: Casual at 10AM and least duration: July at 8AM.

August-Most ridership: Member at 5PM and least ridership: Member at 3AM. Most duration: Casual at 11AM and least duration: Member at 6AM.

September-Most ridership: Member at 5PM and least ridership: Member at 3AM. Most duration: Casual at 10AM and least duration: Member at 6AM.

October-Most ridership: Member at 5PM and least ridership: Casual at 4AM. Most duration: Casual at 11AM and least duration: Member at 6AM.

November-Most ridership: Member at 5PM and least ridership: Casual at 4AM. Most duration: Casual at 3AM and least duration: Member at 5AM.

December-Most ridership: Member at 5PM and least ridership: Casual at 4AM. Most duration: Casual at 2AM and least duration: Casual at 5AM.

The ridership patterns in Chicago are heavily influenced by its distinct four-season climate, with notable effects on both ride frequency and ride duration across customer types.

Casual customers: Maintains same hourly usage pattern year-round. It shows seasonal variability seen by a gradual increase in ridership as months gets warmer and weather improves indicated by low ridership in cold months which spikes in warmer months. Their trip durations, dominates over the Member customers, showing long trips. This indicates a use of bike services mainly for outdoor/recreational activities.

Member customers: Maintain a consistent hourly usage pattern throughout the year. Dominates ridership year-round with a spike in ridership as weather gets warmer. They have short ride duration compared to casual customers. This indicates a stable commuter base. Although usage declines slightly, a core commuting base persists unaffected by the whether.

Member customers dominates ridership at all hours year-round, except in Summer months (June, July and August) from 10AM to 3PM where Casual ridership dominates. 

Casual customers dominates trip durations at all hours year-round, except in Winter months of December at 5Am, January at 4-6AM and February at 4AM, where Member customers dominates. Casual riders account for most late-night trips, possibly linked to nightlife or extended recreational activities.

Visualization on the Analysis of Cyclistic user's average monthly rides and ride duration based on hourly usage pattern 1

[![Tableau Visualization](https://github.com/Dr-Irtwange-NB/Cyclistic-bike-share-analysis/blob/main/images/Analysis%20of%20%20Cyclistic%20user's%20monthly%20rides%20by%20time.png?raw=true)](https://public.tableau.com/views/AnalysisofCyclisticusersaveragemonthlyridesandridedurationbasedonhourlyusuagepattern/AnalysisofCyclisticusersmonthlyridesbytime?:language=en-US&:embed=y&:sid=&:redirect=auth&:embed_code_version=3&:loadOrderID=17&:display_count=y&:origin=viz_share_link)

This line chart shows the monthly-hourly usage pattern for ridership and comparison with customer types for a whole year.

Visualization on the Analysis of Cyclistic user's average monthly rides and ride duration based on hourly usage pattern 2

[![Tableau Visualization](https://github.com/Dr-Irtwange-NB/Cyclistic-bike-share-analysis/blob/main/images/Analysis%20of%20%20Cyclistic%20user's%20average%20monthly%20ride%20duration%20by%20time%20.png?raw=true)](https://public.tableau.com/views/AnalysisofCyclisticusersaveragemonthlyridesandridedurationbasedonhourlyusuagepattern/AnalysisofCyclisticusersaveragemonthlyridedurationbytime?:language=en-US&:embed=y&:sid=&:redirect=auth&:embed_code_version=3&:loadOrderID=18&:display_count=y&:origin=viz_share_link)

This line chart shows monthly-hourly usage pattern for ride duration and comparison with customer types for a whole year.

(d) General Rides and average ride duration by time based on the week_of_month.

(i) all

```{r Rides and average ride duration : week_of_month, echo=FALSE, message=FALSE, warning=FALSE}
# week_of_month data frame: Rides and average ride duration grouped by week_of_month and time of day

week_of_month_time_stats <- clean_Cyclistic_trip_v1 %>%
  mutate(hour = format(started_at, "%H")) %>%  # Extract hour as a grouping variable
  group_by(hour,month,week_day) %>%
  summarise(
    total_rides = n(),  # Count the number of rides
    avg_ride_duration = round(mean(ride_length, na.rm = TRUE), 2)  # Average ride length, rounded to 2 decimal places
  ) %>%
  arrange(hour, month,week_day)  # Arrange the results by hour

# based on customer types

week_of_month_time_stats_cus <- clean_Cyclistic_trip_v1 %>%
  mutate(hour = format(started_at, "%H")) %>%  # Extract hour as a grouping variable
  group_by(hour,month,customer_type,week_day) %>%
  summarise(
    total_rides = n(),  # Count the number of rides
    avg_ride_duration = round(mean(ride_length, na.rm = TRUE), 2)  # Average ride length, rounded to 2 decimal places
  ) %>%
  arrange(hour, month,customer_type,week_day)  # Arrange the results by hour

week_of_month_time_stats
```

Table shows that;

In general, the largest ridership for months is July, Wednesday at 5PM and least ridership April, Thursday at 3AM with the longest trip duration is December, Tuesday at 3AM and least duration January, Sunday at 6AM.

Data shows that most rides occur from late spring (May), summer (June, July and August) and autumn (September and October) during the evenings by (4PM to 6PM) on Weekdays. Least ridership occurs in the winter (December, January and February), spring (March and April) and late autumn (November), dominated by weekdays during the early mornings (1 to 4 AM).

For each month:

January-Most ridership: Wednesday at 5PM  and least ridership: Tuesday at 3AM. Most duration: Saturday at 1AM and least duration: Sunday at 6AM.

February-Most ridership: Thursday at 5PM and least ridership: Tuesday at 3AM. Most duration: Monday at 4AM and least duration: Saturday at 3AM.

March-Most ridership: Monday at 5PM and least ridership: Tuesday at 3AM. Most duration: Tuesday at 4AM and least duration: Wednesday at 6AM.

April-Most ridership: Tuesday at 5PM and least ridership: Thursday at 3AM. Most duration: Wednesday at 2AM and least duration: Friday at 2AM.

May-Most ridership: Wednesday at 5PM and least ridership: Tuesday at 3AM. Most duration: Monday at 2AM and least duration: Thursday at 5AM.

June-Most ridership: Wednesday at 5PM and least ridership: Tuesday at 3AM. Most duration: Tuesday at 3AM and least duration: Monday at 6AM.

July-Most ridership: Wednesday at 5PM and least ridership: Tuesday at 3AM. Most duration: Friday at 3AM and least duration: Monday at 6AM.

August-Most ridership: Thursday at 5PM and least ridership: Tuesday at 3AM. Most duration: Wednesday at 3AM and least duration: Monday at 6AM.

September-Most ridership: Wednesday at 5PM and least ridership: Wednesday at 3AM. Most duration: Tuesday at 3AM and least duration: Wednesday at 5AM.

October-Most ridership: Wednesday at 5PM and least ridership: Monday at 4AM. Most duration: Friday at 1AM and least duration: Monday at 4AM.

November-Most ridership: Thursday at 5PM and least ridership: Tuesday at 3AM. Most duration: Friday at 1AM and least duration: Thursday at 6AM.

December-Most ridership: Wednesday at 5PM and least ridership: Tuesday at 3AM. Most duration: Tuesday at 3AM and least duration: Thursday at 5AM.

Seasonality.

Winter (December, January and February):

Increased ridership on weekdays to weekends indicating reduced weekend leisure rides, with activity largely limited to necessity-driven commuting. Morning commutes (7–9 AM) and evening rides (4–6 PM) dominate weekdays, with weekends showing an even spread across the day. Ridership peaks on weekdays which gradually decline during the weekends. Ride durations are low but peaks in the early mornings at 1-4AM.

It shows the lowest ridership which declines sharply during these months, reflecting Chicago’s harsh winter conditions. January often sees the least activity, with short-duration rides dominating, likely due to the residual effects of holidays (e.g., New Year) or people easing into routines at the start of a new month.

Spring (March, April and May):

Increased ridership on weekdays to weekends, with May seeing a gradual increase in ridership as months get warmer and routine activities (work, school, errands) stabilize. March sees moderate activity, which escalates in April and peaks in May. Duration peaks at 1-4AM on weekdays and on weekends from 10AM-6PM. Significant increases during midday and evening hours reflects more leisure-oriented activities, especially in warmer months with slightly longer rides, especially on weekends.

Summer(June, July and August):

Increased ridership on weekdays to weekends. especially when compared to Sundays. It shows peak season with the highest ridership volumes across all weeks. This may be due to warmer and longer days in these months which encourage both commuting and recreational trips. durations peaks from 1-4AM on weekdays and 2-4PM on both weekdays and weekends. 

Autumn(September, October and November):

Increased ridership on weekdays to weekends.  Weekday patterns remain consistent, but recreational weekend rides decrease sharply after September. Especially compared to Sundays. Durations peaks from 1-4AM on weekdays and 2-4PM on weekends. Ridership remains robust in September but gradually tapers off in October and November as temperatures drop.

Key Observations.

Weather Sensitivity: Ridership and ride duration are highly sensitive to seasonal weather, peaking in warmer months (May–September) and dropping significantly in winter (December–February). These patterns align well with Chicago’s urban commuting behaviors, seasonal tourism, and recreational usage, offering a comprehensive view of how the timing and volume of rides evolve throughout the year.

Weekday vs. Weekend: Weekdays show strong morning and evening commuter peaks, while weekends exhibit more balanced activity throughout the day, with longer ride durations.

Hourly patterns within weeks remain consistent, with peaks in the late morning and evening.

Weekday patterns (Monday to Friday) show distinct morning and evening peaks, likely corresponding to commuting hours (7–9 AM and 5–7 PM). Weekdays show shorter ride durations during commuting hours, possibly due to short, purposeful trips.

Weekend days (Saturday and Sunday) exhibit more consistent activity throughout the day, peaking later (around midday to early evening). Weekends exhibit longer ride durations, particularly in the afternoons, suggesting recreational use, particularly in warmer months..

Inverse relationship or proportional trends between Ridership and Duration.

High Ridership → Shorter Durations:

During weekdays, high ridership aligns with shorter ride durations, particularly during morning (7–9 AM) and evening (5–7 PM) commute times. These rides are purpose-driven and often limited to essential trips. High ridership correlates with shorter durations during commuter-heavy periods.

Lower Ridership → Longer Durations:

During late-night hours (9 PM–3 AM) or Weekends, lower total ridership correlates with longer average ride durations. This trend is especially evident in warmer months, reflecting leisure trips, exploratory rides or group activities. Lower ridership aligns with longer durations during non-commuter times and recreational periods.

(ii) Customer type

```{r week_of_month_time_stats_cus, echo=FALSE, message=FALSE, warning=FALSE}

week_of_month_time_stats_cus
```

Table shows that;

In general, the largest ridership for months is by Member customers on Wednesday of July at 5PM and least ridership by Casual customers on Thursday of January at 4AM with the longest trip duration by Casual customers on Monday of March at 4AM and least duration by Casual customers on Tuesday of November at 3AM.

Data shows that most ridership occur on weekdays by the Member customers from late spring (May), summer (June, July and August) and autumn (September and October), during the evenings by (4PM to 6PM). Least ridership occur by the Casual customers in winter (December, January and February), spring (March and April) and late autumn (November), during the early mornings (2 to 4 AM) mostly on weekdays.

For each month:

January-Most ridership: by Members on Wednesday at 5PM  and least ridership: by Casuals on Thursday at 4AM. Most duration: by Members on Saturday at 1AM and least duration: by Casual on Thursday at 2AM.

February-Most ridership: by Members on Thursday at 5PM and least ridership: by Casuals on Monday at 4AM. Most duration: by Casuals on Monday at 1AM and least duration: by Members on Wednesday at 3AM.

March-Most ridership: by Members on Tuesday at 5PM and least ridership: by Casuals on Thursday at 3AM. Most duration: by Casuals on Monday at 4AM and least duration: by Members on Monday at 2AM.

April-Most ridership: by Members on Tuesday at 5PM and least ridership: by Members on Thursday at 3AM. Most duration: by Casuals on Wednesday at 12AM and least duration: by Members on Friday at 2AM.

May-Most ridership: by Members on Wednesday at 5PM and least ridership: by Casuals on Tuesday at 4AM. Most duration: by Casuals on Sunday at 6AM and least duration: by Members on Friday at 3AM.

June-Most ridership: by Members on Tuesday at 5PM and least ridership: by Members on Tuesday at 3AM. Most duration: by Casuals on Tuesday at 3AM and least duration: by Members on Tuesday at 3AM.

July-Most ridership: by Members on Wednesday at 5PM and least ridership: by Members on Tuesday at 3AM. Most duration: by Casuals on Friday at 3AM and least duration: by Casuals on Monday at 4AM.

August-Most ridership: by Members on Thursday at 5PM and least ridership: by Members on Wednesday at 2AM. Most duration: by Casuals on Wednesday at 3AM and least duration: by Members on Monday at 6AM.

September-Most ridership: by Members on Wednesday at 5PM and least ridership: by Members on Wednesday at 3AM. Most duration: by Members on Wednesday at 2AM and least duration: by Casuals on Wednesday at 4AM.

October-Most ridership: by Members on Wednesday at 5PM and least ridership: by Casuals on Monday at 4AM. Most duration: by Casuals on Friday at 7AM and least duration: by Casuals on Tuesday at 5AM.

November-Most ridership: by Members on Thursday at 5PM and least ridership: by Casuals on Tuesday at 3AM. Most duration: by Casuals on Friday at 3AM and least duration: by Casuals on Tuesday at 3AM.

December-Most ridership: by Members on Wednesday at 5PM and least ridership: by Casuals on Tuesday at 3AM. Most duration: by Members on Tuesday at 3AM and least duration: by Casuals on Thursday at 5AM.

Ridership: Members dominate ridership at each hourly pattern and peak (rush hours), except on April-Saturday and weekends of May, June, July, August, September, October where ridership is dominated by Casuals. It's shows that Members use bikes for goal oriented or commute purposes seen by they high ridership predominantly on weekdays and Members use bikes for mostly leisure or recreational purposes indicated by high ridership predominantly on weekends of warmer months (late Spring, Summer and Autumn) and less ridership on colder months in Chicago.

Ridership is affected by seasonal changes, more so in Casuals as they show a steep decline during colder months and a sharp rise during warmer months. 

Durations: Casuals have longer ride duration year-round than Members which peaks from 1-4 AM. Highest duration peaks can be seen on weekdays in June and March at 3-4AM. 

Casual Vs Member customers.

Member Customers:

Hourly Usage:

Peak Hours: Member customers are more likely to use bikes during morning (7-9AM) and evening (4-6PM) commute times, aligning with typical work/school commutes. These peaks indicate that members view bike services as an essential part of their daily transportation routine.

Low Hours: The very early mornings (1–6AM) and late evenings (9PM–12AM) experience a noticeable dip in ridership, especially during colder months when weather deters biking.

Weekly Usage:

Weekdays: All workweek except Friday exhibit the most consistent and highest ridership during the workweek, particularly in the morning and evening commute hours. These days are more utility-focused, with members using bikes primarily for commuting. Friday is slightly less active, perhaps reflecting lower demand or occasional shifts in work schedules.

Weekends: It show's moderate activity, with ridership that peaks in the early afternoon, indicating members using bikes for errands or recreational purposes. Demand is not as high on Sundays as it's for Saturday. Longer ride durations occurs more on Sundays than Saturday.

Seasonality:

Winter (December–February): Bike ridership drops significantly in winter. However, weekends still see relatively higher usage, likely due to the flexibility in weekend schedules and holiday activities. Even during the cold months, recreational biking on Sundays remains popular. Weekday commute pattern remains almost constant.

Spring and Summer: Spring and summer see an increase in ridership, as there're warmer months. For Weekends in Spring and Summer, Saturday sees higher ridership except in June were high ridership occur on Sunday. This show higher recreational use, with peak usage occurring during afternoon hours. The warmer weather encourages more biking throughout the day.Weekday commutes remains high compared to Weekends.

Fall (September–November): As temperatures cool in fall, ridership begins to decrease with Weekday dominating ridership. However, Weekends (especially Sunday) have the longest durations, showing that recreational use continues even as the weather becomes less favorable.

Casual Customers:

Hourly Usage:

Peak Hours: Casual riders follow similar peak times as member customers, with usage focused around the morning (7-9 AM) and evening (4-6 PM) hours. This suggests that casual customers, while less consistent, still use bike services during typical commute periods.

Low Hours: Like member customers, casual riders avoid very early mornings and late nights, likely due to discomfort in colder temperatures during the winter months.

Weekly Usage:

Weekdays: Casual customers shows peak usage during commute times. However, their usage seems less predictable than that of member customers, indicating they may only use bikes sporadically or for specific needs. Longer rides occur on weekdays compared to weekends

Weekends: Ridership is higher than on weekdays. Usage peaks in the midday and early afternoon. This could indicate casual customers prefer biking as part of their weekend leisure activities.

Seasonality:

Winter: In the winter, cold temperatures result in lower ridership during early mornings and late evenings.

Comparison Between Member and Casual Customers:

Usage Consistency: Member customers exhibit more consistent and predictable usage, with strong commuter activity during weekdays and recreational use on weekends except in warmer months year-round. Casual customers, on the other hand, show more variations based on seasonality, with commuter activity during weekdays and higher recreational use on weekends except in colder months year-round.

Recreational vs. Commuting Use: Member customers use bikes more for commuting during the week and for recreation on weekends. Casual customers use bikes primarily for leisure and recreational activities.

Seasonal Impact: Both groups are affected by Chicago's weather. Casual customers show a more pronounced decrease in ridership during winter months but with longer durations than the Member customers. This reflects Casuals use of bikes for Leisure while Members use bikes for time-sensitive purposes. Casual customers, while less frequent users overall, still bike on weekends during the colder months, likely for holiday or social activities.

Conclusion:
In general, Member customers are more routine-oriented, using bikes for both commuting and recreation with higher predictability throughout the week, while casual customers tend to bike more on weekends for leisure purposes. Both groups are heavily influenced by weather, with colder temperatures reducing usage, particularly during early mornings and evenings.

Visualization on the Analysis of Cyclistic user's week_of_month rides and ride duration based on hourly usage pattern 1 

[![Tableau Visualization](https://github.com/Dr-Irtwange-NB/Cyclistic-bike-share-analysis/blob/main/images/Analysis%20of%20%20Cyclistic%20user's%20week_of_month%20rides%20by%20time.png?raw=true)](https://public.tableau.com/views/AnalysisofCyclisticusersweek_of_monthridesandridedurationsbasedonhourlyusuagepattern/AnalysisofCyclisticusersweek_of_monthridesbytime?:language=en-US&:embed=y&:sid=&:redirect=auth&:embed_code_version=3&:loadOrderID=19&:display_count=y&:origin=viz_share_link)

This line chart shows week of each month-hourly usage pattern for ridership and comparison with customer types for a whole year.

Visualization on the Analysis of Cyclistic user's week_of_month rides and ride duration based on hourly usage pattern 2

[![Tableau Visualization](https://github.com/Dr-Irtwange-NB/Cyclistic-bike-share-analysis/blob/main/images/Analysis%20of%20%20Cyclistic%20user's%20week_of_month%20Average%20ride%20durations%20by%20time.png?raw=true)](https://public.tableau.com/views/AnalysisofCyclisticusersweek_of_monthridesandridedurationsbasedonhourlyusuagepattern/AnalysisofCyclisticusersweek_of_monthAverageridedurationsbytime?:language=en-US&:embed=y&:sid=&:redirect=auth&:embed_code_version=3&:loadOrderID=20&:display_count=y&:origin=viz_share_link)

This line chart shows week of each month-hourly usage pattern for ride duration and comparison with customer types for a whole year.

##### Determine the most popular stations

(a) Total number of stations
```{r Total number of stations, echo=FALSE, message=FALSE, warning=FALSE}
Number_of_stations <- clean_Cyclistic_trip_v1 %>%
  summarise(number_of_stations = n_distinct(start_station_name))

Number_of_stations

```

Filter out stations based on some criteria to reduce time spent on analysis and still maintain data validity.

Different types of customers use the bike share system and the large number of stations (1598 stations) presents a challenge. To determine which stations to focus on, I considered stations that have significant activity (i.e., high ride counts), represent diverse patterns of usage by customer type (casual vs. member) and Stations with significant differences in usage patterns between casual and member customers 

```{r Most popular stations 1, message=FALSE, warning=FALSE, include=FALSE}

# Step 1: Calculate the total ride counts for each station

station_rides <- clean_Cyclistic_trip_v1 %>%
  group_by(start_station_name, customer_type) %>%
  summarise(total_rides = n(), .groups = "drop") %>%
  rename(station_name = start_station_name)

# Step 2: Filter stations with significant activity (e.g., stations with >100 rides)

stations_with_significant_rides <- station_rides %>%
  group_by(station_name) %>%
  summarise(total_station_rides = sum(total_rides)) %>%
  filter(total_station_rides > 100)  # Example threshold of 100 rides

# Step 3: Look at stations with large differences in customer type usage

station_customer_diff <- station_rides %>%
  spread(key = customer_type, value = total_rides, fill = 0) %>%
  mutate(difference_in_usage = abs(casual - member)) %>%
  filter(difference_in_usage > 50)  # Threshold for significant difference

# Step 4: Top stations with the most significant activity

top_stations <- station_rides %>%
  group_by(station_name) %>%
  summarise(total_station_rides = sum(total_rides)) %>%
  arrange(desc(total_station_rides))

# Step 5: Merge the data sets to focus on stations with significant activity and high customer type variation
focus_stations <- full_join(stations_with_significant_rides, station_customer_diff, by = "station_name")

```

The number of stations are shown based on the following reasons : 

(1) High Ride Volume: These stations are active and contribute a significant amount of data.
(2) Customer Type Differences: Stations where the difference between Casual and Member customers is substantial offer a richer understanding of how different customer segments use the system.
(3) Representative Data: Focusing on stations with significant activity ensures that the analysis represents the typical usage patterns in the system.

This analysis aid to prioritize stations that will provide meaningful insights into customer behavior and allow for targeted recommendations, whether for marketing, resource allocation, or operational strategies.

The amount of stations decreased from 1598 to 750 after filtering out stations with little to no activity, missing data, or stations that are rarely used. In order to reduce the number of stations while still maintaining the integrity and validity of the data, I applied some reasonable criteria such as:

Stations with Minimum Ride Count

Stations with Active Customer Type Usage

Stations with Consistent Customer Segmentation

```{r Most popular stations 2, echo=FALSE, message=FALSE, warning=FALSE}
cleaned_focus_stations <- focus_stations %>%
  filter(
    total_station_rides >= 10,                   # Minimum number of total rides
    casual > 0 & member > 0,                     # Activity from both customer types
    difference_in_usage > 50                     # Significant difference in usage
  ) %>%
  mutate(
     consistent_segmentation = (casual / total_station_rides > 0.1) & 
                              (member / total_station_rides > 0.1) # Ensure both types are >10% of total
  ) %>%
  filter(
        consistent_segmentation                       # Consistent customer segmentation
  )

cleaned_focus_stations
```

(b) Top 20 most popular stations: number of rides, customer type and member type


(i) Top 20 for casual customers
```{r Top 20 most Popular ride stations, echo=FALSE, message=FALSE, warning=FALSE}

# Top 20 stations popular among casual riders

top20_casual <- cleaned_focus_stations %>%
  arrange(desc(casual)) %>%
  slice_head(n = 20)

# Top 20 stations popular among member riders

top20_member <- cleaned_focus_stations %>%
  arrange(desc(member)) %>%
  slice_head(n = 20)

# Top 20 stations with the highest amount of rides (overall popular train stations)
top20_overall <- cleaned_focus_stations %>%
  arrange(desc(total_station_rides)) %>%
  slice_head(n = 20)

top20_casual

```

(ii) Top 20 for member customers
```{r top20_member, echo=FALSE, message=FALSE, warning=FALSE}
top20_member
```

(iii) Top 20 Overall
```{r top20_overall, echo=FALSE, message=FALSE, warning=FALSE}
top20_overall
```

(c) Bike types at stations
```{r bike types at stations, echo=FALSE, message=FALSE, warning=FALSE}
# Step 1: Filter clean_Cyclistic_trip_v1 to include only focused stations
# Ensure station names match between the two data frames

focus_station_names <- cleaned_focus_stations$station_name

filtered_data <- clean_Cyclistic_trip_v1 %>% 
  filter(start_station_name %in% focus_station_names)

# Step 2: Summarize the number of bike types for each station
bike_types_per_station <- filtered_data %>%
  group_by(start_station_name,bike_type) %>%
  summarise(
    total_bike_types = n(),  
     ) %>%
  arrange(start_station_name)

bike_types_per_station
```

(d) Number of bike types used by different customer types

```{r number of bike types used by different customer types, echo=FALSE, message=FALSE, warning=FALSE}
# Step 3: Summarize the number of bike types used by different customer types for each station

bike_types_by_customer <- filtered_data %>%
  group_by(start_station_name, customer_type,bike_type) %>%
  summarise(
    total_bike_types = n(),  
     ) %>%
  arrange(start_station_name, customer_type)

bike_types_by_customer
```

This table shows that every station offers an electric bike type which is the most unpopular/undesirable bike option for each customer type. The classic bike is the most popular choice followed by moderate usage of the electric scooter from each customer type.

Visualization on the Analysis of Cyclistic bike-share stations 1

[![Tableau Visualization](https://github.com/Dr-Irtwange-NB/Cyclistic-bike-share-analysis/blob/main/images/Analysis%20of%20Cyclistic%20popular%20stations%20total%20rides,%20member%20type%20rides%20and%20difference%20in%20member%20type%20usuage%20%20%20.png?raw=true)](https://public.tableau.com/views/AnalysisofCyclisticbike-sharestations/AnalysisofCyclisticpopularstationstotalridesmembertyperidesanddifferenceinmembertypeusuage?:language=en-US&:embed=y&:sid=&:redirect=auth&:embed_code_version=3&:loadOrderID=21&:display_count=y&:origin=viz_share_link)

Interactive table to find the Stations with the Minimum and Maximum number of total rides, Member and Casual ridership, difference in usage in ridership between the two customer group and their inter-relationship with each other.

Visualization on the Analysis of Cyclistic bike-share stations 2

[![Tableau Visualization](https://github.com/Dr-Irtwange-NB/Cyclistic-bike-share-analysis/blob/main/images/Analysis%20of%20Cyclistic%20popular%20stations%20by%20bike%20type%20and%20customer%20type%20.png?raw=true)](https://public.tableau.com/views/AnalysisofCyclisticbike-sharestations/AnalysisofCyclisticpopularstationsbybiketypeandcustomertype?:language=en-US&:embed=y&:sid=&:redirect=auth&:embed_code_version=3&:loadOrderID=22&:display_count=y&:origin=viz_share_link)

A heat map showing each station's number of bike types, customer type and relationship with each other. From this heat map, I was able to identify:
Stations were each customers type dominate over each other. It is relevant to know that at each station, the distribution of each customer type are not equal, but one type dominate over the other. 
Stations were customers use only 1,2 or 3 bike type. This may be due to preference or available choices or no choices at these stations.
These heat map can be manipulated to determine stations were promotions can be held to persuade and convince Casual customers to become Member types.

#### **Observations and insights**

Based on the analysis of Cyclistic bike-share data, the following observations and insights were made:

It can be seen that the Member customers make up 61% of cyclist and Casual members 39%. 

The classic bike (66.7%) is the most popular bike type among all customer type followed by the electric bike (32.3%) and least the electric scooter (1.0%).

It's shown that the classic bike are used for longer trips (leisure or recreational use), electric_bike for Moderate trips (practical / time-sensitive usage e.g commuting) and electric_scooter for short quick trips : short errands.

Based on ridership:

the Member customers are the main customer base, indicated on their consistent ridership number year-round with little seasonal variation while the Casual members show a large seasonal variation.

Based on durations: 

In general, the average duration of both customer type 19.73 mins, indicates that that most trips are relatively short. Member customers maintain an almost constant short trip duration year-round showing their use of service for piratical or time sensitive trips such as commutting purposes while Casual users use services for longer trips mostly during warmer months showing their use of services for leisure and recreational purposes. Although Casual customer makes up 39% of Cyclistic users, more time is spent on rides by these group compared to the Member type at 61%, This cements the fact that Casual users use services for leisure trips and less time sensitive tasks.

Week pattern: 

In general when all dataset is viewed in weeks, it's shown that most rides occur on Saturday (peak weekend), Wednesday and Thursday (peak weekdays) with Lower rides early weekdays (Tuesday and Monday), which may be attributed to a quite start of the week. Member customers dominate the weekdays (Monday to Friday) while Casual customers dominate the weekends (Sunday and Saturday). The weekends show longer trip durations compared to weekdays with casual customers dominating all week and the highest duration of member users also on weekends.

Monthly pattern: 

In general when all dataset is viewed by months, It shows seasonal variations affected by Chicago's monthly pattern (i.e) more rides and longer trip durations in warmer months and less in colder months. From this grouping, we observed that Member type customers are the main customer base as they show an almost constant ridership and duration year round when compared to the Casual users which show seasonal fluctuation in both ridership and trip duration year round. This indicates that, Casual users use bike for recreational purposes which spike's in warmer months and drops in colder months while Member users use bike for commuting purposes indicated by their consistency. From this grouping, it's observed that the most favored bike are the Classic bike followed by the electric scooter year-round, The Classic bike shows spike in usage in warmer months and drop in colder month showing user's preference for recreational activities while the electric bike maintains a moderate increase during warmer month showing preference for commuting purposes. The electric scooter is the least favored and this is due to the limited data set for bike usage  seen only in 2 months of August and September which may indicate limited availability, disinterest, small niche or seasonality.

Week of each Month pattern: 

In general when all dataset is viewed by grouping the week in each months, It shows that rides affected by Chicago’s seasonal pattern.

Ridership - Saturdays dominate the highest ridership in many months, especially during spring and summer, Weekday Commutes Stay Consistent Year-Round, Casual Riders dominate Weekends showing higher participation on weekends in warmer months with a sharp/steep decline in colder months, Members are the Primary Riders maintaining a relatively stable ridership across all weekdays showing slight peaks on Wednesday and Thursday and a small drop in cold months year-round. When grouped by bike type, Weekend (Saturday and Sunday) are the busiest days for classic bikes with a higher ride count compared to the electric bikes especially in the summer months. Generally, the Classic bike shows higher ride count compared to the electric bikes which suggest that Classic bikes are popular for leisure rides, exercise/recreational activities or commutes purpose. Electric bikes gain more traction in the warmer months (June to October) possibly due to the warm weather and commuter trends. Electric scooters are relatively less popular in comparison showing low numbers across all months (August and September).When grouped by customer bike preference, Casual Riders tend to go for Classic bikes, with their highest ridership on weekends (Saturday and Sunday) especially in summer months, suggesting leisure and recreational usage. Members Riders use classic bikes predominantly, especially during weekdays. This suggests commuting use.

Durations - Longest ride occurs during spring and summer especially on weekends indicating recreational usage while shorter rides occur in the colder months which indicates trip duration for piratical usage e.g, commuting. Casual users show a strong seasonal pattern with longer rides in warmer months (spring/summer) and shorter rides in colder months indicating rides for primarily recreational purposes while Members ride duration are relatively stable, with minor seasonal dips indicating rides for piratical purposes. When grouped by bike type,For Classic bikes, there’s longer weekend trips and stable weekday trip duration, indicating strong usage for recreational purposes during the weekends and commute during weekdays. Electric bike shows a moderate and consistent duration year-round, throughout the weeks indicating steady year-round use, suggesting a practical, utility-driven preference for shorter distances while the Electric scooter has limited data (month worth) which may be due to a promotional/survey event, seasonality, small niche or disinterest.When grouped by customer bike preference, it shows that both customer types have longer trip duration occur on the Classic bikes, moderate duration on the electric bike and the least duration on the electric scooter except in August for the member customers where the electric scooter has a longer duration when compared to the electric bike.

Ride and ride durations pattern based on a 24hrs: 

When whole year is summarized into a 24 hour bin, 2 peaks (Rush hours) are identified, 8AM (increase ridership to commute) and 5PM ( return from commute and rides for leisure). Data shows an inverse relationship between total rides and average ride duration, When total rides are high, average duration are relatively short indicating a focused, piratical or time sensitive use. Ride duration slightly increases during off peak hours despite fewer rides being recorded indicating recreational or leisure rides and trips. Member users have the highest ridership and shortest duration indicating a focused or piratical use of bikes while Casual users have lower ridership and longer trip duration indicating recreational use.

When whole year is grouped weekly into a 24 hour bin, The highest ridership occurs on weekdays with asymmetric peaks during the rush hours and lowest ridership on weekends which shows a plateau from 12PM to 4PM.Member ridership dominates the weekdays at all hours of the day, while Casual ridership exceeds that of Member users on weekends. Ride durations tend to be longer than average on weekends, while weekday durations fluctuate, with shorter rides observed during peak hours. Casual users dominate trip durations throughout the week, showing longer and more variable trip lengths compared to members, whose trip durations remain almost constant within a 24-hour period.

When whole year is grouped monthly into a 24 hour bin, Ridership maintains 2 asymmetrical peaks during rush hours (8PM and 5PM) for each month within 24hr bin, with higher ridership during the warmer months and a steep decrease in ridership during the colder months. Member users dominates dataset showing 2 asymmetrical peaks during the rush hours while Casual users show three distinct peaks (8AM, 12PM and 5PM), each progressively taller than the previous one, indicating a trend of increasing ridership over time.Casual customers maintains same hourly usage pattern year-round showing seasonal variability with the trip durations, dominating over the Member customers indicating a use of bike services mainly for outdoor/recreational activities while Member customers maintains a consistent hourly usage pattern throughout the year dominating ridership year-round with a spike in ridership as weather gets warmer, they have short ride durations compared to casual customers, this indicates a stable (core) commuter base unaffected by Chicago's whether. Member customers dominates ridership at all hours year-round, except in Summer months (June, July and August) from 10AM to 3PM where Casual ridership dominates. Casual customers dominates trip durations at all hours year-round, except in Winter months of December at 5Am, January at 4-6AM and February at 4AM, where Member customers dominates. Casual riders account for most late-night trips, possibly linked to nightlife or extended recreational activities

When whole year is grouped by week of each month in a 24 hour bin, Ridership pattern remains consistent year round but with increased ridership in warmer months. Member users dominates weekdays and winter weekends while Casual users exceeds Member users on weekends while maintaining same hourly pattern. Durations are shorter on average with longer durations during warmer months

#### **In conclusion**

Chicago’s bike-share usage exhibits distinct, multifaceted patterns driven by season, time-of-day, customer type, and bike preference.

Seasonality & Weather:

Peak Usage: Warm months (late spring through early autumn) see the highest ridership, particularly during evenings (4–6 PM) and mid-mornings.

Low Usage: Harsh winter conditions sharply reduce ridership, especially in the early mornings (1–4 AM).

Customer Segmentation:

Member Customers (~61%):

They form a consistent, year-round base predominantly using bikes for commuting during weekdays (with morning and evening peaks). Their rides are shorter and more purpose-driven, indicating efficiency and routine travel.

Casual Customers (~39%):

Their usage is more seasonal and leisure-oriented, with rides spiking during warmer months and on weekends. They tend to take longer trips, suggesting a focus on recreation, sightseeing, or extended outings.

Bike Type Preferences:

The classic bike is the most popular option across the board, followed by the electric bike. Electric scooters have limited adoption, likely due to niche appeal, availability issues, or promotional factors.

Time-of-Day & Weekly Trends:

Weekdays: Show pronounced commuter peaks—morning (around 8AM) and evening (around 5PM) with short ride durations.

Weekends: Exhibit more leisurely riding patterns with higher mid-day activity and longer trip durations.

Inverse Relationship:

Periods of high ridership (rush hours) are associated with shorter ride durations, while off-peak hours (late night/early morning) have fewer rides but longer durations.

Overall, the data paints a picture of a dual-use system: a robust, routine-driven commuter base (Members) and a weather-sensitive, leisure-focused group (Casuals).



### SHARE PHASE


Presentation [here](https://rpubs.com/nguwasenirtwange/1279089).
