library(tidyverse)
library(showtext)
library(readxl)
font_add_google("Open Sans", "opensans")
showtext_auto()
tickets <- read_xlsx('Events.xlsx',sheet = 'Sheet1') %>%
  mutate(Date = mdy(Date))
summary <- tickets %>%
  filter(Building %in% c('Drama Theater','Black Box Snow')) %>%
  group_by(`Event Name`) %>%
  summarise(Number = n(),
            Total = sum(`Total Sold`),
            FirstDate = min(Date)) %>%
  arrange(Total) %>%
  filter(Total>999)

ggplot(summary,aes(x=Total,y=reorder(`Event Name`,Total),label=Total)) +
  geom_col() +
  geom_text(nudge_x=ifelse(summary$Total>999,50,36)) +
  #scale_fill_manual(values=c('Yes'='#A13935','No'='grey')) +
  labs(y=element_blank(),x='Number of Votes Received',
       title='Total Tickets Sold') +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position='none')


ic <- tickets %>%
  filter(Building == 'BYU I-Center' & !str_detect(`Event Name`,'COMMENCEMENT|GRADUATION')) %>%
  head(n=10)

ggplot(ic,aes(x=`Total Sold`,y=reorder(`Event Name`,`Total Sold`),label=`Total Sold`)) +
  geom_col() +
  geom_text(nudge_x=ifelse(ic$`Total Sold`>999,50,36)) +
  #scale_fill_manual(values=c('Yes'='#A13935','No'='grey')) +
  labs(y=element_blank(),x='Number of Tickets',
       title='Total Tickets Sold for I-Center Events') +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position='none')
  
  
tickets %>%
  filter(str_detect(`Event Name`,'COMMENCEMENT'))
  
  'CLOSED\\s?-'
