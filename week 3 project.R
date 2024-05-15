# Load package tidyverse 

library(tidyverse)
library(janitor)
library(skimr)
library(ggthemes)


# Import data 
# Assign object cereal_data to cereal 

cereal_data <- read_csv("https://raw.githubusercontent.com/rfortherestofus/rin3-datasets/main/getting-started-datasets/cereal/cereal.csv")

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

