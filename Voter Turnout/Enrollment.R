library(tidyverse)
demo1 <- read_csv('Enrollment.csv')
demo1$order <- c(12,12,12,11,11,11,10,10,10,9,9,9,8,8,8,7,7,7,6,6,6,5,5,5,4,4,4,3,3,3,2,2,2,1,1,1)
demo1$highlight <- c(rep('yes',3),rep('no',33))

label_data <- demo1 %>%
  #arrange(Year, Semester, Position)+
  group_by(Semester, Year) %>%
  mutate(label_y = sum(Count)) %>%
  filter(Position == 'On-Campus')

ggplot(demo1,aes(y=Count,x=order,fill=Position)) + 
  geom_col(position='stack') +
  theme_minimal() +
  labs(x=element_blank(),
       y='Enrollment',
       title='BYU-I Enrollment since 2020') +
  #geom_text(label=demo1$Count,position='stack') +
  annotate("rect",xmin=11.5,xmax=12.5,ymin=0,ymax=18714+4604+19678,alpha=.2)+
  annotate('text',x=1,y=-50,label='2020') +
  annotate('text',x=4,y=-50,label='2021') +
  annotate('text',x=7,y=-50,label='2022') +
  annotate('text',x=10,y=-50,label='2023') +
  geom_text(data = label_data, aes(x = order, y = label_y, label = label_y), 
            vjust = 1, size = 3,nudge_y=2000) +
  scale_x_continuous(breaks=seq(1,12),labels=rep(c('W','S','F'),4))
  #scale_color_discrete(c('yes'='black','no'='white')) +

ggplot(filter(demo1,Semester=='Fall'),aes(y=Count,x=Year,fill=Position)) + 
  geom_col(position='stack') +
  theme_minimal() +
  labs(x=element_blank(),
       y='Enrollment',
       title='Fall enrollments since 2020',
       subtitle='In-person enrollments decline, while online keeps growing') +
  annotate("rect",xmin=2022.5,xmax=2023.5,ymin=0,ymax=18714+4604+19678,alpha=.2) +
  geom_text(data = filter(label_data,Semester=='Fall'), aes(x = Year, y = label_y, label = label_y), 
            vjust = 1, size = 3,nudge_y=2000)

demo2 <- filter(demo1,Position!='Interns' & Semester=='Fall')
ggplot(demo2,aes(y=Count,x=Year,color=Position,label=Count)) + 
  geom_path(lwd=2) +
  theme_minimal() +
  labs(title='BYU-Pathway enrollment surpasses BYU-Idaho in-person',
       y='Enrollment') +
  geom_text(data = demo2, aes(x =Year, y = Count, label = Count), 
            vjust = 1, size = 3,nudge_x = ifelse(demo2$Position=='Online',-.1,.1),nudge_y = 200)

# GENDER
gender <- tibble(Female = c(11991,8683,11002,12495),
                 Male = c(11328,9156,11469,11636))
gender$Female/(gender$Male+gender$Female)

total <- tibble(Year=seq(2018,2023,1),
                Enrollment = c(38005,39152,44488,44304,43024,42997))

ggplot(total,aes(x=Year,y=Enrollment,label=Enrollment)) + geom_line(lwd=2,color='#A13935') +
  scale_y_continuous(breaks=c(35000,40000,45000),
                     labels=c('35,000','40,000','45,000')) +
  ylim(c(35000,45000)) +
  theme_minimal() +
  geom_text(nudge_y=ifelse(total$Enrollment>40000,500,-500)) +
  labs(title='Total fall enrollment since 2018')+
  theme(panel.grid.minor = element_blank())
