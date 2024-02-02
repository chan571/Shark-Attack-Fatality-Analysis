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


At first glance, most of the variables are self-explanatory. However, I am uncertain about the variables *href.formula*, *href*, *Case.Number*, *X*, *X.1* and *original.order*. In particular, *Case.Number*, there are three of them and they appear to have identical values, creating redundancy.  This redundancy is also observed in *Date* and *Year* columns.

The key variables we need to address the question are *Date* or *Year*,*Type*,*Country*,*Fatal..Y.N.*. All of these variables have some missing values. Additionally the observations for *Fata..Y.N.* are not consistently formatted, there is one row showing "Yx2".

Next, we explore the data a bit more closely. 

A total of 218 different countries are listed in the dataset. Meanwhile, let's see how many cases per country
```
length(unique(data$Country))

country_count <- table(data$Country)
sort_country_count <- sort(country_count, decreasing = TRUE)
head(sort_country_count, 20)
```
<img width="1168" alt="Screen Shot 2024-01-30 at 6 38 47 PM" src="https://github.com/cp571/Data-Cleaning/assets/157858508/26b45841-cd62-46b2-9e33-bd4b87413f08">


                                                                                                                                        
