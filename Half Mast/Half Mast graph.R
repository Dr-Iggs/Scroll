library(tidyverse)
mast <- read_csv('Half Mast 15 pages.csv')

summary <- mast %>%
  mutate(Month = str_extract(Date, '-.*-'),
         Month=str_remove_all(Month,'-'),
         Year = str_extract(Date, '-\\d{1,2}$'),
         Year = str_remove(Year,'-')) %>%
  group_by(State, Month, Year) %>%
  summarise(count = n())
ggplot(filter(summary,State=='Idaho'),aes(x=count)) + geom_histogram()
View(summary)
View(mast)
max()
