
# Load standard libraries
library("tidyverse")
library("dplyr")
library("ggplot2")



data <- read.csv("/Users/chanchingping/Downloads/GSAF5.csv.bz2", sep='\t')

#Count how many countries we have in the data
length(unique(data$Country))

#Count how many cases per country
country_count <- table(data$Country)
sort_country_count <- sort(country_count, decreasing = FALSE)
head(sort_country_count, 80)

#Variable 'Year' data type
class(data$Year)

#Count how many missing values in 'Year'
sum(is.na(data$Year))



#Calculate min, median and range of 'Year' variable
min_year<-min(data$Year, na.rm = TRUE)
min_year
median_year<-median(data$Year, na.rm = TRUE)
median_year
range_year<-range(data$Year, na.rm = TRUE)
range_year


#Find what 'Date' values looks like with 'Year' equals to 0
year_zero <- data %>%
  select(Date, Year) %>% 
  filter(Year == 0) 
head(year_zero,6)
count(year_zero)


data %>% 
  filter(Date == "Ca. 725 B.C.")


#Rename 'Fatal..Y.N.' to 'Fatal'
rename_data <- data %>% 
  select(Year, Fatal..Y.N., Country) %>%
  filter(Year>0) %>% 
  rename(Fatal = Fatal..Y.N.)



#Check how many cases have been recorded and organize them by year in descending order.
total_cases <- rename_data %>% 
  select(Year, Fatal) %>% 
  filter(Year>0) %>% 
  group_by(Year) %>% 
  summarise(yr_cases = n()) 

total_cases %>%  
  arrange(desc(yr_cases))


#After Y2012(include), the percentage of data with fatal information 
after_2012 <- nrow(filter(rename_data, !Fatal== "" & Year >= 2012))
after_2012_all <- nrow(filter(data, Year >= 2012))
after_2012_pct <- after_2012/after_2012_all
after_2012_pct
after_2012_all


#See how many different values in 'Fatal' variable
rename_data %>% 
  group_by(Fatal) %>% 
  filter(Year >= 2012) %>% 
  summarise(unique_value = n())

#Assign new values 'False' or 'True' to 'Fatal' variable
rename_data <- rename_data %>% 
  mutate(Fatal = ifelse(Fatal=="N", "FALSE", ifelse(Fatal=="Y", "TRUE","")))
head(rename_data, 10)

#Find the total shark attack cases and the fatality rate for Australia and South Africa between 2012 and 2021.
aus_sa <- rename_data %>% 
  select(Year, Country, Fatal) %>% 
  filter(Year >= 2012 & Country=='AUSTRALIA'|Country=="SOUTH AFRICA") %>% 
  group_by(Country) %>% 
  summarise(total_casecount=n(), percentage = sum(Fatal== TRUE)/total_casecount)
aus_sa
