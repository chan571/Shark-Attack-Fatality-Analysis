As part of this practice, we are tackling fundamental programming tasks in R, focusing on data cleaning and exploratory analysis. 
We are working on Global Shark Attack file, a compilation of all reported shark attacks on humans. 
The question we aim to address is: Which country, Australia or South Africa, poses a greater risk in terms of shark attacks on people?

Let's get started!


Load standard library
```
library("tidyverse")
library("dplyr")
library("ggplot2")
```

There are 24 variables and 25827 cases in this data
```
data <- read.csv("/Users/chanchingping/Downloads/GSAF5.csv.bz2", sep='\t')
summary(data)
```
<img width="1001" alt="Screen Shot 2024-01-30 at 6 27 39 PM" src="https://github.com/cp571/Data-Cleaning/assets/157858508/274ba681-cba8-4c48-9f76-a3f433d400c9">

The key variables we need to address the question are *Year*,*Country*,*Fatal..Y.N.*. All of these variables have some missing values. 
The column name 'Fatal..Y.N.' is not very intuitive, so let's change it to simply 'Fatal'.
```
rename_data <- data %>% 
  select(Year, Fatal..Y.N., Country) %>%
  filter(Year>0) %>% 
  rename(Fatal = Fatal..Y.N.)
```

We are going to focus on recent time span. 
The range of *Year* is 0 - 2021.
```
range_year<-range(data$Year, na.rm = TRUE)
range_year
```
Find out why *Year* has minimum value of '0' and how many of them.
```
year_zero <- data %>%
  select(Date, Year) %>% 
  filter(Year == 0) 
head(year_zero,6)
count(year_zero)
```
<img width="204" alt="Screenshot 2024-02-13 at 6 50 03 PM" src="https://github.com/cp571/Data-Cleaning/assets/157858508/a91c2947-4938-4bc1-aecc-4a1101033c27">

There are 129 cases with Year value equal to ‘0’. 
Some of the data dates back a long time ago, mostly from late 1800 and onwards. 
However, there are also a number of years before Christ (B.C.), due to various sources contributing to this data, some of which are hard to validate for accuracy.
For instance, the case with date ‘Ca. 725 B.C.’, the data could come from a work of fiction, given that it originates from a book published in 1958.
```
data %>% 
  filter(Date == "Ca. 725 B.C.")
```
<img width="648" alt="Screenshot 2024-02-13 at 6 54 06 PM" src="https://github.com/cp571/Data-Cleaning/assets/157858508/c2c88379-22d6-4729-a8d5-14e91fd5b695">

The analysis will concentrate on data from 2012 to 2021, as there's an increasing trend in shark-related incidents reported after 2000. 
This range provides recent and focused data, with fewer missing values for the variable 'fatal incidents', resulting in 1204 cases.

```
#Check how many cases have been recorded and organize them by year in descending order.
total_cases <- rename_data %>% 
  select(Year, Fatal) %>% 
  filter(Year>0) %>% 
  group_by(Year) %>% 
  summarise(yr_cases = n()) 

total_cases %>%  
  arrange(desc(yr_cases))
```
<img width="201" alt="Screenshot 2024-02-13 at 7 09 43 PM" src="https://github.com/cp571/Data-Cleaning/assets/157858508/50cf85bb-81b3-4459-b1ca-474352130ea0">

```
#After Y2012(include), the percentage of data with fatal information 
after_2012 <- nrow(filter(rename_data, !Fatal== "" & Year >= 2012))
after_2012_all <- nrow(filter(data, Year >= 2012))
after_2012_pct <- after_2012/after_2012_all
after_2012_pct
```
<img width="144" alt="Screenshot 2024-02-13 at 7 10 13 PM" src="https://github.com/cp571/Data-Cleaning/assets/157858508/a1dc5ca0-b498-4d4d-b5f5-9392b9172159">

```
after_2012_all
```
<img width="109" alt="Screenshot 2024-02-13 at 7 10 46 PM" src="https://github.com/cp571/Data-Cleaning/assets/157858508/a61d5bae-e932-42a0-803f-b5f55fd93569">


Now it is time to analyze if the attack was fatal. 

There are 8 different values in the Fatal variable, with most of them likely resulting from input errors, as they each have only 1-2 observations. 
It’s possible that missing is also ‘UNKNOWN’, they have the same meaning.
I think we can’t make any assumptions on the values that are other than “Y” or “N”, therefore if value that is other than these two characters then we should categorize them as ‘UNKNOWN’. 
However in here unknown values are represent as blank.
```
#See how many different values in 'Fatal' variable
rename_data %>% 
  group_by(Fatal) %>% 
  filter(Year >= 2012) %>% 
  summarise(unique_value = n())
```
<img width="250" alt="Screenshot 2024-02-13 at 7 18 53 PM" src="https://github.com/cp571/Data-Cleaning/assets/157858508/379aefc9-484e-4442-8d23-3eade2541147">

```
#Assign new values 'False' or 'True' to 'Fatal' variable
rename_data <- rename_data %>% 
mutate(Fatal = ifelse(Fatal=="N", "FALSE", ifelse(Fatal=="Y", "TRUE","")))
head(rename_data, 10)
```
<img width="363" alt="Screenshot 2024-02-13 at 7 20 36 PM" src="https://github.com/cp571/Data-Cleaning/assets/157858508/eed6960c-6ada-40f5-84d9-6efd1f46019b">

Finally, we can answer the question about which country is more dangerous, Australia or South Africa?
The result shows South Africa not only has more than double the number of reported cases compared to Australia but also a higher fatality rate. 
Therefore, based solely on these two numbers, I would conclude that in the recent decade, South Africa is more dangerous in terms of shark attacks on people.

```
#Find the total shark attack cases and the fatality rate for Australia and South Africa between 2012 and 2021.
aus_sa <- rename_data %>% 
  select(Year, Country, Fatal) %>% 
  filter(Year >= 2012 & Country=='AUSTRALIA'|Country=="SOUTH AFRICA") %>% 
  group_by(Country) %>% 
  summarise(total_casecount=n(), percentage = sum(Fatal== TRUE)/total_casecount)
aus_sa
```
<img width="358" alt="Screenshot 2024-02-13 at 7 23 09 PM" src="https://github.com/cp571/Data-Cleaning/assets/157858508/c1e0a9bf-074e-47a6-af50-88a4899c3ca5">


Note:
There are a few ethical issues with this data. The sources from which this data is extracted vary, with some having questionable validity.For example, some cases date back to B.C. The data collection method should be examined. Moreover, a significant amount of data is missing, as we discovered earlier, with more than half of the cases missing ‘Year’ values. This will impact the accuracy of the analysis results, considering that ‘Year’ is a crucial variable in our analysis.

Furthermore, relying solely on the number of incidents and fatality rates may not provide enough information to support the argument that South Africa is more dangerous in terms of shark attacks. Other variables, such as related policies and medical resources, may also influence the likelihood of people getting injured by sharks and the fatality rate.

It is possible that people think it is the ‘fact’ that South Africa is more dangerous than Australia without delving deeper into other factors that may also influence the final analysis result. If the information is misused, it will definitely hurt South Africa’s tourism industry.




