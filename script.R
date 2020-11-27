install.packages("tidyverse") 
install.packages("lubridate")
install.packages("dplyr")
install.packages("scales")
install.packages("ggthemes")
install.packages("patchwork")
library(tidyverse)
library(lubridate)
library(dplyr)
library(scales)
library(ggthemes)
library(patchwork)

rm(list = ls())
#loading in data sets
footfall_raw2020 <-
  read_csv(
    "https://data.smartdublin.ie/dataset/cc421859-1f4f-43f6-b349-f4ca0e1c60fa/resource/3048794e-16bd-4edb-9ba9-8018a6aadcdb/download/jan-oct-2020-ped-data.csv"
  )
footfall_raw2019 <-
  read_csv(
    "https://data.smartdublin.ie/dataset/cc421859-1f4f-43f6-b349-f4ca0e1c60fa/resource/0e1ac985-3a45-4134-a696-32909a0310aa/download/dcc-2019-pedestrian-footfall-count-jan-dec_14082020.csv"
  )
covid_raw <-
  read_csv(
    "https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
  )
#--------------------------------------------cleaning up covid--------------------------------------------------------------------------------------
# Title: How to plot a time series in R (new COVID-19 cases example)
# Author: Data Science with Tom
# Date: 21/11/2020
# Availability: https://www.youtube.com/watch?v=AeIRvIB1Ogs  
# Example followed until line 46, from there I manipulated the dataset on my own

#removing unnecessary columns (Provience/state, Latitude and Longitude)
covid_columns_removed <- covid_raw[, -c(1, 3, 4)]
#pivoting the data, renaming columns, formatting date, getting total covid cases per day per country
covid_cleaned <- covid_columns_removed %>%
  pivot_longer(-c(`Country/Region`),
               names_to = "date",
               values_to = "confirmed") %>%
  rename(Country = `Country/Region`) %>%
  mutate(date = mdy(date)) %>%
  group_by(Country, date) %>%
  summarise(confirmed = sum(confirmed)) %>%
  ungroup()
#getting new cases per day
covid_sorted <- covid_cleaned %>%
  arrange(date) %>%
  group_by(Country) %>%
  mutate(new_cases = confirmed - lag(confirmed, default = 0)) %>%
  ungroup()
#only keeping the records for Ireland
covid_just_Ireland <- covid_sorted %>%
  filter(Country == "Ireland")
#removing dates past my cut off point
covid_dates_removed <- covid_just_Ireland %>%
  filter(covid_just_Ireland$date <= "2020-10-09")
#removing unnecessary columns (country and total confirmed cases)
covid_dates_removed <- covid_dates_removed[, -c(1, 3)]
#removing the extra day in the year records for matching data (leap year)
covid_dates_removed <- covid_dates_removed[-c(39),]
#assigning the dataset to another dataset for merging
df_covid <- covid_dates_removed
#------------------cleaning up 2019--------------------------------------------------------------------------------------------------------------------------
#removing streets that have large numbers of empty records and that don't link up with the streets I've selected to keep for both datasets
footfall_columns_removed_2019_other <-
  footfall_raw2019[, -c(6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 18, 22, 23, 24)]
#removing null records
footfall_columns_removed_2019_other <-
  na.omit(footfall_columns_removed_2019_other)
#converting date format
footfall_date_converted_2019_other <-
  footfall_columns_removed_2019_other %>%
  mutate(Time = dmy_hms(Time))
#converting date format again to get rid of hms
footfall_date_converted_2019_other$Time <-
  as.Date(footfall_date_converted_2019_other$Time)
#pivoting
footfall_pivotted_2019_other <-
  footfall_date_converted_2019_other %>%
  pivot_longer(-c(Time),
               names_to = "street",
               values_to = "people_2019")
#combining all footfall per day
footfall_combined_people_2019_other <-
  footfall_pivotted_2019_other %>%
  group_by(Time) %>%
  summarise(people_2019 = sum(people_2019))
#removing records after the given date to line up with my 2020 dataset
footfall_dates_removed <- footfall_combined_people_2019_other %>%
  filter(footfall_combined_people_2019_other$Time <= "2019-10-09")
#removing records again this time to line up with my covid dataset
footfall_dates_removed <- footfall_dates_removed %>%
  filter(footfall_dates_removed$Time >= "2019-01-22")
#removing unnecessary columns (Dates)
footfall_dates_removed <- footfall_dates_removed [, -c(1)]
#assigning the dataset to another dataset for merging
df_2019 <- footfall_dates_removed
#---------------------------------------cleaning up 2020-----------------------------------------------------------------------------------------------------
#removing streets that have large numbers of empty records and that don't link up with the streets I've selected to keep for both datasets
footfall_columns_removed_other <-
  footfall_raw2020[, -c(4, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 18, 21, 22, 23, 24)]
#removing null records
footfall_columns_removed_other <-
  na.omit(footfall_columns_removed_other)
#converting date format
footfall_date_converted_other <- footfall_columns_removed_other %>%
  mutate(`Date & Time` = dmy_hms(`Date & Time`))
#converting date format again to get rid of hms
footfall_date_converted_other$`Date & Time` <-
  as.Date(footfall_date_converted_other$`Date & Time`)
#pivoting
footfall_pivotted_other <- footfall_date_converted_other %>%
  pivot_longer(-c(`Date & Time`),
               names_to = "street",
               values_to = "people_2020")
#combining all footfall in dublin per day
footfall_combined_people_other <- footfall_pivotted_other %>%
  group_by(`Date & Time`) %>%
  summarise(people_2020 = sum(people_2020))
footfall_combined_people_other <- footfall_combined_people_other %>%
  filter(footfall_combined_people_other$`Date & Time` <= "2020-10-09") %>%
  filter(footfall_combined_people_other$`Date & Time` >= "2020-01-22")
#removing the extra day in the year records for matching data (leap year)
footfall_combined_people_other <-
  footfall_combined_people_other[-c(39),]
#assigning the dataset to another dataset for merging
df_2020 <- footfall_combined_people_other
#---------------merging----------------
df_2019 <- tibble::rowid_to_column(df_2019, "ID")
df_2020 <- tibble::rowid_to_column(df_2020, "ID")
df_covid <- tibble::rowid_to_column(df_covid, "ID")
#merging data sets for summary graph
merged.df = merge(df_2019, df_2020)
final_merge = merge(merged.df, df_covid)
#removing unnecessary column
final_merge <- final_merge[, -c(1, 3)]


#----------------Summary Plot--------------------------------------------------------------------------
#allows me to create the legend, let the variable equal a color
colors <-
  c("2019" = "blue",
    "2020" = "red",
    "Daily covid cases" = "black")

# Title: Make Beautiful Graphs in R: 5 Quick Ways to Improve ggplot2 Graphs
# Author: Dataslice
# Date: 21/11/2020
# Availability: https://www.youtube.com/watch?v=qnw1xDnt_Ec&t=483s 
# Used some elements of the guide to improve quality of my graph such as the theme fivethirthyeight

# Title: Dual Y axis with R and ggplot2
# Author: The R Graph Gallery
# Date: 21/11/2020
# Availability: https://www.r-graph-gallery.com/line-chart-dual-Y-axis-ggplot2.html
# Used as a guide to plot two Y graphs (Used in several graphs)

final_merge %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = people_2019, color = "2019")) +
  geom_line(aes(y = people_2020, color = "2020")) +
  geom_line(aes(y = new_cases * 100, color = "Daily covid cases")) +
  scale_x_date(date_breaks = "1 weeks", date_labels = "%d-%b") +
  scale_y_continuous(
    name = "Footfall in Dublin",
    labels = comma,
    sec.axis = sec_axis( ~ . / 100,
                         name = "Covid-19 Cases")
  ) +
  labs(
    title = "Covid cases in Ireland VS footfall in Dublin City",
    subtitle = "Summary graph of my datasets",
    x = "Dates",
    y = "Covid-19 Cases",
    color = "People"
  ) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),
        axis.text.x = element_text(angle = 90))
#-------------------------------Insight One (Was footfall in Dublin City impacted by COVID-19)--------------------------------------------
insight_1 <- final_merge %>%
  filter(final_merge$date >= "2020-08-01")

insight_1 %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = people_2020, color = "2020")) +
  geom_line(aes(y = new_cases * 100, color = "Daily covid cases")) +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b") +
  scale_y_continuous(
    name = "Footfall in Dublin",
    labels = comma,
    sec.axis = sec_axis( ~ . / 100,
                         name = "Covid-19 Cases")
  ) +
  labs(
    title = "Was footfall in Dublin City impacted by COVID-19?",
    x = "Dates",
    y = "Covid-19 Cases",
    color = "People"
  ) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),
        axis.text.x = element_text(angle = 90))
#-----------------------Insight Two (Is footfall corrolated to the amount of COVID-19 cases)---------------------------------------------------
insight_2 <- final_merge %>%
  filter(final_merge$date >= "2020-06-06")

insight_2 <- insight_2 %>%
  filter(insight_2$date <= "2020-07-29")

insight_2 %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = people_2020, color = "2020")) +
  geom_line(aes(y = new_cases * 100, color = "Daily covid cases")) +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b") +
  scale_y_continuous(
    name = "Footfall in Dublin",
    labels = comma,
    sec.axis = sec_axis( ~ . / 100,
                         name = "Covid-19 Cases")
  ) +
  labs(
    title = "Is footfall corrolated to the amount of COVID-19 cases?",
    x = "Dates",
    y = "Covid-19 Cases",
    color = "People"
  ) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),
        axis.text.x = element_text(angle = 90))
#----------------------------Insight Three (Was the first lockdown in Ireland proactive or reactive)-----------------------------------------------------
insight_3 <- final_merge %>%
  filter(final_merge$date >= "2020-02-17")
insight_3 <- insight_3 %>%
  filter(insight_3$date <= "2020-03-31")
insight_3 %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = people_2020, color = "2020")) +
  geom_line(aes(y = new_cases * 100, color = "Daily covid cases")) +
  scale_x_date(date_breaks = "5 days", date_labels = "%d-%b") +
  scale_y_continuous(
    name = "Footfall in Dublin",
    labels = comma,
    sec.axis = sec_axis( ~ . / 100,
                         name = "Covid-19 Cases")
  ) +
  labs(
    title = "Was the first lockdown in Ireland proactive or reactive?",
    x = "Dates",
    y = "Covid-19 Cases",
    color = "People"
  ) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),
        axis.text.x = element_text(angle = 90))
#-----------------------------------insight four (did footfall return to normal?)----------------------------------------------------
insight_4 <- final_merge %>%
  filter(final_merge$date >= "2020-06-08")
insight_4 <- insight_4 %>%
  filter(insight_4$date <= "2020-08-24")
insight_4 %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = people_2020, color = "2020")) +
  geom_line(aes(y = people_2019, color = "2019")) +
  scale_x_date(date_breaks = "5 days", date_labels = "%d-%b") +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Did footfall return to normal in Dublin City?",
    x = "Dates",
    y = "People in Dublin",
    color = "People"
  ) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),
        axis.text.x = element_text(angle = 90))

#--------------------------------------Calculations done for workings in my report---------------------------------------------------
insight_3_a <- covid_just_Ireland %>%
  filter(date >= "2020-02-17")
insight_3_a <- insight_3_a %>%
  filter(date <= "2020-03-27")
sum(insight_3_a$new_cases)

insight_4_a <-footfall_combined_people_other %>%
  filter(`Date & Time` >= "2020-06-08")
insight_4_a <- insight_4_a %>%
  filter(`Date & Time` <= "2020-08-24")
mean(insight_4_a$people_2020)

insight_4_b <- footfall_combined_people_2019_other %>%
  filter(Time >= "2019-06-08")
insight_4_b <- insight_4_b %>%
  filter(Time <= "2019-08-24")
mean(insight_4_b$people_2019)

