library(tidyverse)
library(readxl)
library(ggpattern)
library(googlesheets4)

ties <- read_xlsx('Talk and Ties.xlsx',sheet='Sheet1')
read_shee

#Colors -----------------------------------

colors <- c('Yellow'='#fdffb6','Red'='#ffadad','Orange'='#ffd6a5',
            'Green'='#caffbf','Blue'='#a0c4ff','Purple'='#bdb2ff',
            'Pink'='#ffc6ff','Black'='#222222','Grey'='#aaaaaa',
            'White'='#eeeeee','Brown'='#B1907F')
caption <- 'Through Sunday afternoon'

summary <- ties %>%
  filter(Position %in% c('Apostle','First Presidency')) %>%
  group_by(Color) %>%
  summarise(Sum = n()) %>%
  mutate(Graph_Color = colors[Color]) %>%
  filter(!is.na(Color))
  
sequence = seq(1,max(summary$Sum),1)


ggplot(summary,aes(x=Sum,y=reorder(Color,Sum,sum),fill=Color)) + geom_col() +
  theme_minimal() +
  labs(x='Number of Ties or Blazers',
       y=element_blank(),
       title='It\'s a \"Tie!\"',
       subtitle='Blue and red lead the colors',
       caption=caption) +
  scale_x_continuous(breaks=sequence,labels=sequence)+
  scale_fill_manual(values=colors) +
  theme(panel.grid.minor = element_blank()) +
  guides(fill='none')


ggplot(summary,aes(x=Sum,y=reorder(Color,Sum,sum),fill=Color)) + geom_col() +
  theme_minimal() +
  labs(x='Number of Ties',
       y=element_blank(),
       title='First Presidency & Quorum of the Twelve Apostles',
       subtitle='President Nelson leads red to victory!',
       caption=caption) +
  scale_x_continuous(breaks=sequence,labels=sequence)+
  scale_fill_manual(values=colors) +
  theme(panel.grid.minor = element_blank()) +
  guides(fill='none')


#Pattern -------------------------------------

flower_pattern <- pattern_define(
  img_path = "floral.jpeg",  # Replace with your image file path
  aspect_ratio = 1  # Adjust as needed
)
patterns <- c('stripe'='b','polka dot'='d','Plaid'='c')
summary <- ties %>%
  group_by(Pattern) %>%
  summarise(Sum = n()) %>%
  mutate(Graph_Pattern = patterns[Pattern])

sequence = seq(1,max(summary$Sum),1)

ggplot(summary,aes(x=Sum,y=reorder(Pattern,Sum,sum))) + 
  geom_col() +
  theme_minimal() +
  labs(x='Number of Ties/Dresses',
       y=element_blank(),
       title='What Patterns are Most Popular?',
       subtitle='Floral takes an early lead',
       caption=caption) +
  scale_x_continuous(breaks=sequence,labels=sequence)+
  guides(fill='none')



df <- data.frame(level = c("stripe", "b", "stripe", 'd'), outcome = c(2.3, 1.9, 3.2, 1))

p <- ggplot(df, aes(level, outcome)) +
  geom_col_pattern(
    aes(pattern = level, pattern_angle = level, pattern_spacing = level), 
    fill            = 'white',
    colour          = 'black', 
    pattern_density = 0.35, 
    pattern_fill    = 'black',
    pattern_colour  = 'black'
  ) +
  theme_bw() +
  labs(
    title    = "ggpattern::geom_col_pattern()",
    subtitle = 'geometry-based patterns'
  ) +
  scale_pattern_spacing_discrete(range = c(0.01, 0.05)) + 
  theme(legend.position = 'none') + 
  coord_fixed(ratio = 1)

p
