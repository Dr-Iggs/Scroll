install.packages(c('tidyverse','readxl','readr'))

library(tidyverse)
library(readxl)
library(readr)

#Remove times where reference to Jesus aren't about him
transcripts <- read_xlsx('Talk and Ties.xlsx',sheet='Sheet1') %>%
  mutate(Text = str_remove(Text, 'Church of Jesus Christ'),
         Text = str_remove(Text, 'Lord\'s Church')) %>%
  filter(!is.na(Text))
names <- read_csv('JesusList.csv')


#Counts the references to 'Jesus Christ', then removes those terms so 
# 'Jesus' and 'Christ' only count individual references
alltalks <- str_c(transcripts$Text,collapse=' ')
JesusChrist <- str_count(alltalks,'Jesus Christ')
alltalks <- str_remove_all(alltalks, 'Jesus Christ')

#Go through all the text, and count each reference to these names.
#Case-sensitive, because speakers usually capitalize the names (Rock
#probably refers to Jesus, but rock probably won't)
appearances <- tibble(name='Jesus Christ', count=JesusChrist)
for(name in names$name) {
  appearances <- appearances %>% add_row(name=name,count=str_count(alltalks,name))
}
ones <- filter(appearances, count==1) #Save single references for later
appearances <- filter(appearances,count>1) %>%
  add_row(name='Rock',count=2) %>%
  add_row(name='Hero',count=2) %>%
  add_row(name='Bridegroom',count=2) %>%
  add_row(name = 'Christ',count = str_count(alltalks,'\\b(in|of|with|the)\\s+Christ\\b'))

#Plot the names with 2+ references
sequence <- seq(0,max(appearances$count,na.rm = T),10)
names_sum_plot <- ggplot(appearances,aes(x=count,y=reorder(str_to_title(name),count,sum))) +
  geom_col(fill='#A13935') +
  geom_text(label=appearances$count,nudge_x=ifelse(appearances$count>9,4,1.8)) +
  scale_x_continuous(breaks=sequence,labels=sequence) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) +
  labs(x='Number of References',y=element_blank(),
       subtitle='Only names with two or more references',
       title='References to Jesus in Conference Talks',
       caption=paste(sum(appearances$count),' total references'))
names_sum_plot

#Sum by speaker
all_names <- paste(names,collapse='|')
transcripts$Jesus_Ref <- str_count(transcripts$Text,paste(names,collapse='|'))

