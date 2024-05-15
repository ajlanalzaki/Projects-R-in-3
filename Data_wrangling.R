#Load package tidyverse 

library(tidyverse)
library(janitor)

#Import data 
#Assign object cereal_data to cereal 

cereal_data <- read_csv("https://raw.githubusercontent.com/rfortherestofus/rin3-datasets/main/getting-started-datasets/cereal/cereal.csv")|>
  clean_names()

#or 
cereal_data <- read_csv("cereal.csv")
  
#View data 
  cereal_data_clean <- cereal_data |>
    drop_na(sugars_g)|>
  glimpse()
  
  
#Look for specific cereal 
  cereal_data_clean |>
    select(contains ("NAME"))|>
  view()
#Select calories to fat_g columns 
  cereal_data_clean |>
    select(calories:fat_g)|>
    view()
  
  
#Select calories to fat_g columns and include name 
  #Not sure why I cant get this to work 
  cereal_data_clean |>
    select((contains ("NAME")) & select(calories:fat_g))|>
    view()
  
#Look for those cereals that are cold 
  cereal_data_clean|>
    filter(type == "cold")|>
    view()
  
#Look for those cereals that are hot 
  cereal_data_clean|>
    filter(type == "hot")|>
    view()

  
#Grouping data together for cereal data that is grouped by manufacturer and then summarize the average calories by manufacture. Here we present the number of cereal by each manufacturer and then include those with more than 1 type of cereal and arrange average calories by ascending order
  cereal_data_clean|>
    group_by(mfr)|>
    summarize(average_calories_by_mfr=mean(calories), n=n())|>
    filter(n>1)|>
    arrange(average_calories_by_mfr)|>
    view()
  
#Grouping data together for cereal data that is grouped by manufacturer and then summarize the average calories by manufacture. Here we present the number of cereal by each manufacturer and then include those with more than 1 type of cereal and arrange average calories by descending order
  cereal_data |>
    group_by(mfr)|>
    summarize(average_calories_by_mfr=mean(calories), n=n())|>
    filter(n>1)|>
    arrange(desc(average_calories_by_mfr))|>
    view()
  
  
#Grouping data together for cereal data that is grouped by manufacturer and then summarize the average calories by manufacture. Here we present the number of cereal by each manufacturer and then include those with more than 1 type of cereal and arrange the number of different cereals in descending order 
  cereal_data |>
    group_by(mfr)|>
    summarize(average_calories_by_mfr=mean(calories), n=n())|>
    filter(n>1)|>
    arrange(desc(n))|>
    view()

  #Counting the number of cereals for each brand 
  cereal_data|>
    count(mfr)|>
    view()
  