library(tidyverse)
library(xml2)

data <- read_xml('../raw-data/pass_matrix.xml')

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

