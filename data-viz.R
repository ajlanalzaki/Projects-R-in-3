# Load package tidyverse 

library(tidyverse)
library(janitor)
library(skimr)
library(ggthemes)


# Import data 
# Assign object cereal_data to cereal 

cereal_data <- read_csv("https://raw.githubusercontent.com/rfortherestofus/rin3-datasets/main/getting-started-datasets/cereal/cereal.csv")

# or 
# cereal_data <- read_csv("cereal.csv")

# View data
skimr::skim(cereal_data)
cereal_data_clean <- cereal_data |>
  drop_na(sugars_g, potass_mg) |>
  glimpse()


# Look for specific cereal 
cereal_data_clean |>
  select(contains ("NAME")) |>
  view()
# Select calories to fat_g columns 
cereal_data_clean |>
  select(calories:fat_g) |>
  view()

# From Libby re: selecting with "and" -------------------------------------

# You have two select() functions, one inside the other!
# You can use an "or" pipe here, like this:
cereal_data_clean |>
  select(contains("NAME") | calories:fat_g)

# Or, you can use commas (see ?select for more details!)
cereal_data_clean |>
  select(contains("NAME"), calories:fat_g)

# Look for those cereals that are cold 
cereal_data_clean |>
  filter(type == "cold") |>
  view()

# Look for those cereals that are hot 
cereal_data_clean |>
  filter(type == "hot") |>
  view()


# Grouping data together for cereal data that is grouped by manufacturer and then 
# summarize the average calories by manufacture. Here we present the number of 
# cereal by each manufacturer and then include those with more than 1 type of 
# cereal and arrange average calories by ascending order
cereal_data_clean |>
  group_by(mfr) |>
  summarize(average_calories_by_mfr = mean(calories), n = n()) |>
  filter(n > 1) |>
  arrange(average_calories_by_mfr) |>
  view()

# Grouping data together for cereal data that is grouped by manufacturer and then 
# summarize the average calories by manufacture. Here we present the number of 
# cereal by each manufacturer and then include those with more than 1 type of 
# cereal and arrange average calories by descending order
cereal_data |>
  group_by(mfr) |>
  summarize(average_calories_by_mfr = mean(calories), n = n()) |>
  filter(n > 1) |>
  arrange(desc(average_calories_by_mfr)) |>
  view()


# Grouping data together for cereal data that is grouped by manufacturer and then 
# summarize the average calories by manufacture. Here we present the number of 
# cereal by each manufacturer and then include those with more than 1 type of 
# cereal and arrange the number of different cereals in descending order 
cereal_data |>
  group_by(mfr) |>
  summarize(average_calories_by_mfr = mean(calories), n = n()) |>
  filter(n > 1) |>
  arrange(desc(n)) |>
  view()

# Counting the number of cereals for each brand 
cereal_data |>
  count(mfr) |>
  view()


# Data Visualization --------------------------------------------------------

# Prepare the data for plotting. 
# Only want to include those with calories < 110

cereal_plot <- cereal_data |>
  filter(calories  < 110) |>
  view()

# So here, I want to plot a graph of all the names of cereals with calories < 110 
# and and to be color coded by manufacturer. I have inlcuded the calories numerically 
# as text and will also seperate our the graph using facet grids by manufacturer

ggplot(data = cereal_plot,
       mapping = aes(x = calories,
                     y = name,
                     fill = mfr,
                     label = calories))+
  geom_col()+
  geom_text(
  color ="black",
  hjust = 1.1)+
  labs(title="Calories per serving",
       subtitle ="Only calories less than 100 included",
       x = "Calories",
       y = NULL)+
  theme_minimal() + 
  facet_grid(cols = vars(mfr))

#Is there a way to change the legend title from mfr to manufacturer

