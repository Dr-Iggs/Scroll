library(tidyverse)
library(readxl)
house = read_xlsx('Rent.xlsx',sheet='Rent') %>%
  mutate(asPercent = Capacity / sum(Capacity) * 100,
         Owned = str_replace(Owned,'Self|Other','All Others'),
         Owned =replace_na(Owned, 'All Others'))
  
owners = read_xlsx('Rent.xlsx',sheet='Sheet3') %>%
  filter(!(`Row Labels` %in% c('(blank)','Grand Total','Self'))) %>%
  arrange(`Sum of Capacity`)


#SCATTERPLOT------------------
library(ggrepel)

scatter_own = owners

scatter_own = filter(scatter_own,`Count of Owned`>2)
scatter_own$`Sum of Capacity`[9] <- 4134+1435
scatter_own = filter(scatter_own,`Row Labels`!='Rexburg Housing')
                                                                    
ggplot(scatter_own,aes(x=`Sum of Capacity`,y=`Count of Owned`)) + 
  geom_point(color='#A13935',size=5) +
  theme_minimal() +
  labs(x='Total Number of Beds',y='Total Number of Complexes',
       title='Number of Buildings & Beds',
       caption='Limited to Companies with 3+ Buildings') +
  geom_text_repel(
    label=scatter_own$`Row Labels`, 
    nudge_x = 0.25, nudge_y = 0.25,size=3
  ) #+
  scale_y_continuous(breaks=c)

ggplot(owners, aes(x=`Sum of Capacity`,y=reorder(`Row Labels`,`Sum of Capacity`,sum))) + geom_col(fill='#A13935')
  
ggplot(house, aes(x=Capacity,y=reorder(Owned,Capacity,sum))) + geom_col(fill='#A13935')


house2 <- house %>%
  filter(!(Owned %in% c(NA,'Grand Total','Self')))
  


#PERCENT OF HOUSING------------------
ggplot(house, aes(x = reorder(Owned, asPercent,sum), y = asPercent)) +
  geom_bar(stat = "identity",fill='#A13935') +
  theme_minimal() +
  labs(x = "Management Company", y = "",
       #caption='Limited to companies with >300 capacity',
       title='Ownership as % of all Student Housing') +
  coord_flip() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank()
  ) #+
  #scale_y_continuous(breaks=c(0,5,10,20,30),labels=c(0,5,10,20,30))

#TOTAL OCCUPANCY------------------  
ggplot(house2, aes(x = reorder(Owned, Capacity,sum), y = Capacity)) +
  geom_bar(stat = "identity",fill='#A13935') +
  geom_segment(y=4134,yend=4134,x=11.4,xend=10.6) +
  theme_minimal() +
  labs(x = "Management Company", y = "Total Capacity",
       caption='Limited to companies with >300 capacity',
       title='Who Owns the Most Apartments?',
       sub='Redstone + Rexburg Housing Have Huge Market Lead') +
  coord_flip() +
  theme(legend.position = "none",
        panel.grid.major.y = element_blank()) +
  annotate("text",y=4100,x=9.6, size=3,label="Addition of\nRexburg Housing")


