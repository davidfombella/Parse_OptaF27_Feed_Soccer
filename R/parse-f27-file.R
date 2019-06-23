library(tidyverse)
library(xml2)

data <- read_xml('pass_matrix.xml')

meta_data <- data %>%
  xml_attrs() %>% 
  t() %>% 
  as_tibble()


root_players <- xml_find_all(data, '/SoccerFeed/Player') %>%
  xml_attrs() %>%
  map(~as_tibble(t(.))) %>% 
  bind_rows()

node_players <- xml_find_all(data, '/SoccerFeed/Player') %>% 
  map(~.x %>% 
        xml_children() %>% 
        map(function(.y) {.y %>% 
            xml_attrs() %>% 
            t() %>% 
            as_tibble() %>% 
            mutate(n_passes = xml_text(.y))}) %>% 
        bind_rows())



root_players <- root_players %>% 
  mutate(n = map(node_players, count) %>% unlist()) %>% 
  uncount(weights = n)


all_data <- meta_data %>% 
  mutate(n = nrow(bind_rows(node_players))) %>% 
  uncount(weights = n) %>% 
  bind_cols(root_players) %>% 
  bind_cols(bind_rows(node_players))




# CAST TYPES
all_data$n_passes <- as.integer(all_data$n_passes)
all_data$pass_success <- as.integer(all_data$pass_success) 
all_data$x <- as.numeric(all_data$x) 
all_data$y <- as.numeric(all_data$y)


## all_data to view columns and types
all_data %>%
  filter(player_name=="Forward 1")%>%
  select(player_name,x,y, player_name1,n_passes)

## STARTING 11 players dataframe
starting_data <-all_data %>%
  filter(position !="Substitute")




##########################

library(ggsoccer)
library(ggrepel)

## GGPLOT 

  ggplot() +
    annotate_pitch(dimensions = pitch_opta) +
    theme_pitch()+
    geom_point(data=starting_data, aes(x=x, y=y, colour=position, size= pass_success))+
    geom_text_repel(data=distinct(starting_data,player_name,.keep_all=TRUE), aes(x=x, y=y, label=player_name))


  




