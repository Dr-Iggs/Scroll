library(tidyverse)
library(readxl)
library(showtext)
font_add_google("Open Sans", "opensans")
showtext_auto()

mayors <- read_xlsx('Mayor.xlsx',sheet="Cities")  %>%
  #filter(City != 'Provo, Utah') %>%
  mutate(voteperc = VotesCast/(Pop2020-`Student Pop`-Under18)*100,
         color=ifelse(College=='BYU-Idaho','Yes','No'),
         otherpeople = Pop2020-`Student Pop`-Under18)

# PERCENTAGE OF NON-STUDENT VOTER AGE THAT VOTED
ggplot(mayors,aes(y=reorder(City,voteperc,'desc'),x=voteperc,fill=color,label=paste0(round(voteperc,1), '%'))) + 
  geom_col() +
  geom_text(nudge_x=3) +
  theme_minimal() +
  scale_fill_manual(values=c('Yes'='#A13935','No'='grey'))+
  theme(legend.position='none',
        panel.grid.minor = element_blank(),
        panel.grid.major.y=element_blank()) +
  labs(y='College Town',x='Turnout of non-student voting-age population',
       title='Even after removing university students, Rexburg turnout is low',
       caption='From each city\'s most recent election') +
    scale_x_continuous(breaks=seq(0,60,10),labels=c('0%','10%','20%','30%','40%','50%','60%'))

# PERCENTAGE OF VOTER AGE THAT VOTED
mayors2 <- mayors %>%
  mutate(voteperc = VotesCast/(Pop2020-Under18)*100)

ggplot(mayors2,aes(y=reorder(City,voteperc,'desc'),x=voteperc,fill=color,label=paste0(round(voteperc,1), '%'))) + 
  geom_col() +
  geom_text(nudge_x=1.5) +
  theme_minimal() +
  scale_fill_manual(values=c('Yes'='#A13935','No'='grey'))+
  theme(legend.position='none',
        panel.grid.minor = element_blank(),
        panel.grid.major.y=element_blank()) +
  labs(y='College Town',x='Turnout of voting-age population',
       title='Overall mayoral election turnout by city',
       caption='From each city\'s most recent election') +
  scale_x_continuous(breaks=seq(0,40,10),labels=c('0%','10%','20%','30%','40%'))


# PERCENTAGE OF TOWN THATS STUDENT
ggplot(mayors,aes(y=reorder(City,`Student Pop`/Pop2020),x=`Student Pop`/Pop2020,
                  fill=color,label = paste0(round(`Student Pop`/Pop2020*100,0),'%'))) + 
  geom_col() +
  geom_text(nudge_x=.025) +
  theme_minimal() +
  scale_fill_manual(values=c('Yes'='#A13935','No'='grey'))+
  theme(legend.position='none',
        panel.grid.minor = element_blank(),
        panel.grid.major.y=element_blank()) +
  labs(y='College Town',x='Student population as a percent of total population',
       title='How much of the town is enrolled at the university?',
       caption='Based on election year enrollment and 2020 census') +
  scale_x_continuous(breaks=seq(0,.6,.2),labels=c('0%','20%','40%','60%'))

stacked <- mayors %>%
  select(!c(VotesCast,RegVoterTurnout,`SP Source`,`Election Source`)) %>%
  mutate(stuperc = `Student Pop`/Pop2020,
         youthperc = Under18/Pop2020,
         adultperc = otherpeople/Pop2020,
         voteperc = (Pop2020-`Student Pop`-Under18)/Pop2020) %>%
  pivot_longer(cols=c(stuperc,youthperc,voteperc,adultperc))
ggplot(stacked,aes(x=value,y=City,fill=name)) +
  geom_col(position='stack')


#STACKED VOTER VS NON
stacked2 <- mayors %>%
  select(!c(RegVoterTurnout,`SP Source`,`Election Source`)) %>%
  mutate(voteperc = (VotesCast)/Pop2020,
         studperc = `Student Pop`/Pop2020,
         otherperc = 1-voteperc-studperc) %>%
  pivot_longer(cols=c(voteperc,otherperc,studperc))
ggplot(stacked2,aes(x=value,y=City,fill=name)) +
  geom_col(position='stack') +
  theme_minimal() +
  annotate('text',x=0,y=9,label='Voters') +
  annotate('text',x=0.4,y=9,label='Students') +
  annotate('text',x=.8,y=9,label='Non-student non-voters')



ggplot(mayors,aes(x=`Student Pop`/Pop2020,y=VotesCast/(Pop2020-`Student Pop`),label=City)) + geom_point() +
  geom_text() +
  labs(x='Percentage of the town that\'s a student',
       y='Percentage of the town that votes')





state <- read_xlsx('Mayor.xlsx',sheet='State')
ggplot(state,aes(x=Year,y=`% of Voting Age Population`)) +geom_line()
