library(tidyverse)
library(lessR)
library(ggimage)
library(showtext)
library(gganimate)
font_add_google("Open Sans", "opensans")
showtext_auto()

distributors <- read_csv('Wiki Get.csv')
score_year <- read_csv('Top100.csv')
colnames(score_year) <- c('Rank','Film','Money1','Money2','Money3','Year')
movies <- read_csv('All Movie Data.csv') %>%
  filter(!is.na(Film))

summary_og <- distributors %>%
  mutate(Current = str_replace(Modern,'20th Century-Fox','20th Century Fox')) %>%
  group_by(Distributor) %>%
  summarise(count = n())

ggplot(summary_og,aes(x=count,y=reorder(Distributor,count,sum),label=count)) +
  geom_col(fill='#A13935') +
  theme_minimal() +
  labs(title='Disney has an unprecedented movie hold') +
  geom_text(nudge_x=ifelse(summary_og$count>9,.4,.2))



summary_modern <- movies %>%
  group_by(Modern) %>%
  summarise(count = n()) %>%
  mutate(isDisney = ifelse(Modern=='Walt Disney Studios','Yes','No'))
  

ggplot(summary_modern,aes(x=count,y=reorder(Modern,count,sum),label=count,fill=isDisney)) +
  geom_col() +
  scale_fill_manual(values=c("Yes"='#A13935','No'='grey')) +
  theme_minimal() +
  theme(panel.grid.minor=element_blank(),
        legend.position='none') +
  labs(title='Walt Disney Studios today',
       subtitle='Merger with 20th Century Fox doubled their movie collection',
       x='Total Movies in Top 100',y=element_blank()) +
  geom_text(nudge_x=ifelse(summary_modern$count>9,1,.5))

#plot.background=element_rect(fill="lightgrey", color="lightgrey")

summary_modern$as_percent
ggplot(summary_modern,aes(x=count,y=reorder(Current,count,sum))) +
  geom_col(fill='#A13935') + coord_polar(theta='x')

PieChart(x=data=summary_modern)
,labels=paste0(summary_modern$Current,', ', summary_modern$count, '%'))

# TIMELINE (redownload)

disney <- read_csv('Disney by Time.csv')
allyears <- tibble(Year=seq(1923,2023,1))
timeline <- allyears %>%
  left_join(disney,by='Year') %>%
  fill(Cumulative)
timeline$Cumulative <- replace_na(timeline$Cumulative,0)

ggplot(timeline,aes(x=Year,y=Cumulative,label=Explanations)) + 
  geom_line(color='#A13935',lwd=2) +
  #geom_text(nudge_x=-5,nudge_y=2,size=3) +
  theme_minimal() +
  #annotate('text',x=1937,y=2,label='Snow White is released',size=3) +
  geom_point(data=filter(timeline,!is.na(Explanations)),aes(x=Year,y=Cumulative),size=4)+
  scale_x_continuous(breaks=seq(1920,2020,10),labels=seq(1920,2020,10)) +
  theme(panel.grid.minor = element_blank()) +
  labs(x=element_blank(),y='How Many Disney films\nare still in the top 100',
       title='Disney\'s Release of Top 100 Films') +
  annotate('text',x=1932,y=4,size=3, label='Disney\'s first full-length\nmovie, \"Snow White\"') +
  annotate('text',x=1964,y=13,size=3, label='Founder Walt\nDisney dies') +
  annotate('text',x=2001,y=17,size=3, label='Acquisition of Pixar') +
  annotate('text',x=2014,y=13,size=3, label='Acquisition of Marvel') +
  annotate('text',x=2006,y=24,size=3, label='Acquisition of Lucasfilm') +
  annotate('text',x=2012,y=44,size=3, label='Acquisition of\n20th Century Fox') +
  annotate('text',x=1989,y=7,size=3, label='\"The Little Mermaid\"\nkickstarts the\nDisney Renaissance')
  

write_csv(filter(movies,Modern!='Walt Disney Studios'),'Other Studio Timeline.csv')

# Hits Since 2010
recent <- tibble("Property" = c("Marvel","Star Wars" ,"Pixar","Remakes","Originals"),
                 "In Top 100" = c(4,4,2,2,0))
ggplot(recent,aes(y=reorder(Property,`In Top 100`),x=`In Top 100`)) + 
  geom_col(fill='#A13935',width = .6) +
  labs(y='Subsidiary',title='Walt Disney Studio\'s Releases since 2010',
       subtitle='Number of releases in top 100') +
  theme_minimal() +
  theme(panel.grid=element_blank(),
        axis.text.y = element_text(size=15),
        axis.title.x = element_blank())

streaks <- tibble("Company" = c('Disney 2015-2019','Disney 1940-1942', 
                                'Amazon MGM 2001-2002','Warner Bros. 1973-1974',
                                'Universal Pictures, 1973-1973'),
                  "Number of consecutive top 100 films by the same publisher" = 
                    c(11,3,2,2,2),
                  'Focus' = c("yes","no","no","no","no")
)
ggplot(streaks,aes(y=reorder(Company,`Number of consecutive top 100 films by the same publisher`),
                   x=`Number of consecutive top 100 films by the same publisher`,
                   label=`Number of consecutive top 100 films by the same publisher`,
                   fill=Focus)) +
  geom_col() +
  scale_fill_manual(values=c('yes'='#A13935','no'='grey'))+
  theme_minimal() +
  theme(legend.position='none') +
  labs(y=element_blank(), title='Disney\'s unprecedented recent box-office domination')+
  geom_text(nudge_x=.2)


### TRY ANIMATING
year <- arrange(movies,Year)
year$x <- rep(seq(1:10),10) * 2
year$y <- c(rep(10,10),rep(9,10),rep(8,10),rep(7,10),rep(6,10),rep(5,10),rep(4,10),rep(3,10),rep(2,10),rep(1,10)) * 3
year$order <- 'ByYear'
rank <- arrange(movies,Rank)
rank$x <- rep(seq(1:10),10) * 2
rank$y <- c(rep(10,10),rep(9,10),rep(8,10),rep(7,10),rep(6,10),rep(5,10),rep(4,10),rep(3,10),rep(2,10),rep(1,10)) * 3
rank$order <- 'ByRank'

movies <- add_row(year, rank)
# Set the number of rows and columns in the grid
n_rows <- 10
n_cols <- 10

# Create a new variable for grid positioning
#movies <- arrange(movies,Rank)
movies$x <- rep(seq(1:10),10) * 2
movies$y <- c(rep(10,10),rep(9,10),rep(8,10),rep(7,10),rep(6,10),rep(5,10),rep(4,10),rep(3,10),rep(2,10),rep(1,10)) * 3
movies$isDisney <- ifelse(movies$Modern=='Walt Disney Studios','Yes','No')

ggplot(movies, aes(x = x, y = y)) +
  geom_image(aes(image = url),size=.063) +
  geom_tile(aes(alpha=isDisney,fill=Modern)) +
  scale_alpha_manual(values =c('Yes'= .7,'No'=0.5)) +
  scale_fill_brewer(palette = "Set3") +  # Choose a color palette
  theme_void() +
  labs(title='All 100 highest-grossing movies in order',
       subtitle='Non-Walt Disney Studios in white') +
  theme(legend.position='none',
        plot.margin = margin(0, 0, 0, 0, "inches")) +
  coord_fixed(ratio = 1) +
  transition_states(
    order,
    transition_length = 3,
    state_length = 10
  )


library(httr)
library(jsonlite)
api_key <- "f5ab5193b16afd89c855dae1001b3bb1"

# Function to get movie release date from TMDB API
get_movie_release_date <- function(movie_name,listing_match=1) {
  # API endpoint
  endpoint <- paste0("https://api.themoviedb.org/3/search/movie?api_key=", api_key, "&query=", gsub(" ", "+", movie_name))
  
  # Send API request
  response <- GET(endpoint)
  
  # Check status code
  if (response$status_code == 200) {
    # Parse response as JSON
    #print('Got something')
    #json_stuff <- read_json(endpoint)
    json_data <- fromJSON(content(response, as='text'))
    # Get first result
    movie_id <- json_data$results$poster_path[listing_match]
    return(paste0('https://image.tmdb.org/t/p/w500',movie_id))
  } else {
    return(NA)
  }
  #image_url <- paste0('https://api.themoviedb.org/3/movie/',movie_id,"?&append_to_response=videos&api_key=", api_key)
}

# Example usage
movie_name <- "Beauty and the Beast"
print(get_movie_release_date(movie_name,2))
n <- 97
movies$url[n] <- get_movie_release_date(movies$Film[n],1)

movies$url[] <- map_chr(movies$Film,get_movie_release_date)

d <- data.frame(x = rnorm(10),
                y = rnorm(10),
                image = sample(c("https://www.r-project.org/logo/Rlogo.png",
                                 "https://jeroenooms.github.io/images/frink.png"),
                               size=10, replace = TRUE)
)
ggplot(d, aes(x, y)) + geom_image(aes(image=image))


#100 BY RECENCY
movies <- arrange(movies,Year)
movies$x <- rep(seq(1:10),10) * 2
movies$y <- c(rep(10,10),rep(9,10),rep(8,10),rep(7,10),rep(6,10),rep(5,10),rep(4,10),rep(3,10),rep(2,10),rep(1,10)) * 3
movies$isDisney <- ifelse(movies$Distributor=='Walt Disney Studios','Yes','No')
movies$isDisney <- ifelse(movies$Modern =='Walt Disney Studios' & movies$Distributor !='Walt Disney Studios','NowDis',movies$isDisney)

ggplot(movies, aes(x = x, y = y)) +
  geom_image(aes(image = url),size=.063) +
  geom_tile(aes(alpha=isDisney,fill=isDisney)) +
  scale_fill_manual(values=c('NowDis'='grey','No'='white',yes='white')) +
  scale_alpha_manual(values =c('Yes'= 0,'No'=0.8,'NowDis'=.5)) +
  #scale_fill_brewer(palette = "Set3") +  # Choose a color palette
  theme_void() +
  labs(title='All 100 highest-grossing movies by release year',
       subtitle='Non-Disney in black, future acquisitions in grey') +
  theme(legend.position='none',
        plot.margin = margin(0, 0, 0, 0, "inches")) +
  coord_fixed(ratio = 1)
