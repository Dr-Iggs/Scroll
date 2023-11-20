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
         Votes = c(406,1235,1298,1323,178,755),
         Spend_Vote = total/Votes,
         distance = case_when(
           str_length(as.character(total)) ==3~40,
           str_length(as.character(total)) ==6~70,
           str_length(as.character(total))>6~80
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
       title='Campaign spending for each Rexburg candidate')


View(campaign %>%
  group_by(For) %>%
  summarise(totals = sum(Amount))
)

campaign %>%
  group_by(Candidate,For) %>%
  summarise(amount=sum(Amount))%>%
  write_csv('byboth.csv')

compare <- tibble(Year = c(2021,2023),
       Donations = c(8297,4892),
       Won = c('No','Yes'))

ggplot(compare,aes(y=Donations,x=Year,fill=Won,label=paste0('$',Donations))) +
  geom_col() +
  theme_minimal() +
  geom_text(nudge_y=150) +
  scale_fill_manual(values=c('Yes'='#A13935','No'='grey')) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position='none') +
  labs(y='Total Funds raised',x=element_blank(),
       subtitle='Only includes reported campaigns (over $500)',
       title='Campaign contributions in 2021 compared to 2023') +
  scale_x_continuous(breaks=c(2021,2023),labels=c('2021','2023'))

sum(summary$total)
