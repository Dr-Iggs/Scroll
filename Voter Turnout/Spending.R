library(tidyverse)
library(showtext)
library(readxl)
library(readr)
font_add_google("Open Sans", "opensans")
showtext_auto()
url <- 'https://sunshine.sos.idaho.gov/?offices[0][value]=46&offices[1][value]=48&districts[0][value]=46:%20REXBURG%20HILL&districts[1][value]=48:%20REXBURG%20HILL&_gl=1*b3eiq8*_ga*MTgyMjA0ODg0MC4xNjk1MjYyODky*_ga_SN820FRG5P*MTY5OTkxNjE3Ny4yLjEuMTY5OTkxNzMyNy4wLjAuMA..&_ga=2.62562796.1363214646.1699916178-1822048840.1695262892'

campaign <- read_xlsx('Spending.xlsx',sheet = 'Sheet1')

summary <- campaign %>%
  #filter(For=='Signs') %>%
  group_by(Candidate) %>%
  summarise(total = sum(Amount)) %>%
  mutate(Won = ifelse(Candidate %in% c('David Reeser','Jerry Merrill','Eric Erickson'),'Yes','No'),
         Votes = c(406,1239,1235,1298,1323,213,338,178,755),
         Spend_Vote = total/Votes,
         distance = case_when(
           length(as.character(total)) ==3~30,
           length(as.character(total)) ==6~80,
           length(as.character(total))>5~120
         )) 

ggplot(summary,aes(x=total,y=reorder(Candidate,total),fill=Won,label=paste0('$',total))) +
  geom_col() +
  theme_minimal() +
  geom_text(nudge_x=summary$distance) +
  scale_fill_manual(values=c('Yes'='#A13935','No'='grey')) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position='none') +
  labs(x='Total Campaign Spending',y=element_blank(),
       title='David Reeser outspend opponents')
