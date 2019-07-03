library(tidyverse)
library(xml2)

data <- read_xml('pass_matrix_23_2017_g943031_t186.xml')

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




##### ADD destination coordinates to df pass receiver
players <- xml_find_all(data, '/SoccerFeed/Player') %>%
  xml_attrs() %>%
  map(~as_tibble(t(.))) %>% 
  bind_rows()%>%
  select(player_id,x,y)%>%
     rename(player_id1=player_id,x_end=x,y_end=y)




##### ADD new columns all_data left join to player_destination position

all_data <- merge(x = all_data, y = players , by = "player_id1", all.x = TRUE)


# CAST TYPES
all_data$n_passes <- as.integer(all_data$n_passes)
all_data$cross <- as.integer(all_data$cross)
all_data$cross_success <- as.integer(all_data$cross_success) 
all_data$pass_success <- as.integer(all_data$pass_success) 
all_data$x <- as.numeric(all_data$x) 
all_data$y <- as.numeric(all_data$y)
all_data$x_end <- as.numeric(all_data$x_end) 
all_data$y_end <- as.numeric(all_data$y_end)

## all_data to view columns and types
all_data %>%
  filter(player_name=="Casemiro")%>%
  select(player_name,x,y, player_name1,n_passes)

## STARTING 11 players dataframe
starting_data <-all_data %>%
  filter(position !="Substitute")




##########################

library(ggsoccer)
library(ggrepel)




################################################
## GGPLOT 1 Passes number and colour position ##
################################################

  ggplot() +
    annotate_pitch(dimensions = pitch_opta) +
    theme_pitch()+
    geom_point(data=starting_data, aes(x=x, y=y, colour=position, size= pass_success))+
    geom_text_repel(data=distinct(starting_data,player_name,.keep_all=TRUE), aes(x=x, y=y, label=player_name))


################################################
## GGPLOT 2 Passes number and colour position ##
## SCALING PASS SUCCESS  #######################
################################################

ggplot() +
  annotate_pitch(dimensions = pitch_opta) +
  theme_pitch()+
  geom_point(data=starting_data, aes(x=x, y=y, colour=position, size= pass_success))+
  geom_text_repel(data=distinct(starting_data,player_name,.keep_all=TRUE), 
                  aes(x=x, y=y, label=paste(player_name,pass_success))) + 
  scale_size(range = c(0, 10))+
  theme(legend.position="none")




######################################
## GGPLOT 3 -- PASSES ################
######################################

  ggplot() +
    annotate_pitch(dimensions = pitch_opta) +
    theme_pitch()+
    geom_curve(data = starting_data %>%filter(n_passes>4), 
               aes(x = x, y = y, xend = x_end, yend = y_end,size= n_passes,color=player_name),
               position="nudge",
               lineend="round",
               arrow = arrow(length = unit(0.02, "npc")),
               curvature = -0.2)+
  scale_size(range = c(0, 3))+
    geom_point(data=starting_data, aes(x=x, y=y, size= pass_success))+ 
    #scale_size(name = "Passes", range = c(3,7))+
    geom_text_repel(data=distinct(starting_data,player_name,.keep_all=TRUE), 
                    aes(x=x, y=y, label=paste(player_name,pass_success)) ,fontface="bold")+
    scale_size(range = c(0, 10))+
   # theme(legend.position="none")+
     ggtitle("Real Madrid vs Betis Min 5 Passes (BET 3-5 RMA) - Player Average Position based on ball contacts  - David Fombella @bigdatasport") 
  
  
  
######################################
## GGPLOT 4 - CROSSES  ###############
###################################### 
  
  ggplot() +
    annotate_pitch(dimensions = pitch_opta) +
    theme_pitch()+
    geom_curve(data = starting_data %>%filter(cross>0), 
               aes(x = x, y = y, xend = x_end, yend = y_end,size= cross,color=player_name),
               position="nudge",
               lineend="round",
               arrow = arrow(length = unit(0.02, "npc")),
               curvature = -0.1)+
    geom_point(data=starting_data, aes(x=x, y=y, size= cross_success))+ 
    #scale_size(name = "Passes", range = c(3,7))+
    geom_text_repel(data=distinct(starting_data,player_name,.keep_all=TRUE), 
                    aes(x=x, y=y, label=paste(player_name,cross_success)) ,fontface="bold")+
    theme(legend.position="none")+
    ggtitle("Real Madrid Crosses vs Betis (BET 3-5 RMA) - Player Average Position based on ball contacts  - David Fombella @bigdatasport") 
  
  
#################################
## GGPLOT 5 GEOM LINE  ##########
#################################  

  ggplot() +
    annotate_pitch(dimensions = pitch_opta) +
    theme_pitch()+
    geom_segment(data = starting_data %>%filter(n_passes>3), 
               aes(x = x, y = y, xend = x_end, yend = y_end,size= n_passes,color=player_name),
               position="nudge", arrow = arrow(length = unit(0.2, "inches") ) )+
    geom_point(data=starting_data, aes(x=x, y=y, size= pass_success))+ 
    #scale_size(name = "Passes", range = c(3,7))+
    geom_text_repel(data=distinct(starting_data,player_name,.keep_all=TRUE), aes(x=x, y=y, label=player_name))+
    theme(legend.direction = "vertical", legend.box = "vertical")



###############################################
## GGPLOT 6  - LINE WITHOUT SIZE ##############
###############################################


  ggplot() +
    annotate_pitch(dimensions = pitch_opta) +
    theme_pitch()+
    geom_segment(data = starting_data %>%filter(n_passes>3), 
                 aes(x = x, y = y, xend = x_end, yend = y_end,color=player_name),
                 position="nudge", arrow = arrow(length = unit(0.1, "inches") ) )+
    geom_point(data=starting_data, aes(x=x, y=y, size= pass_success))+ 
    #scale_size(name = "Passes", range = c(3,7))+
    geom_text_repel(data=distinct(starting_data,player_name,.keep_all=TRUE), aes(x=x, y=y, label=player_name))+
    theme(legend.direction = "vertical", legend.box = "vertical")
  
  
  
  
  # https://stackoverflow.com/questions/35904363/offset-geom-segment-in-ggplot
  
  # shorten arrows
  segmentsDf <- function(data, shorten.start, shorten.end, offset){
  
  data$dx = data$x_end - data$x
  data$dy = data$y_end - data$y
  data$dist = sqrt( data$dx^2 + data$dy^2 )
  data$px = data$dx/data$dist
  data$py = data$dy/data$dist
  
  data$x = data$x + data$px * shorten.start
  data$y = data$y + data$py * shorten.start
  data$x_end = data$x_end - data$px * shorten.end
  data$y_end = data$y_end - data$py * shorten.end
  data$x = data$x - data$py * offset
  data$x_end = data$x_end - data$py * offset
  data$y = data$y + data$px * offset
  data$y_end = data$y_end + data$px * offset
  
  return(data)
  }

  
  #  tempNodes <- data.frame ('x' = c(10, 40), 'y' = c(10, 30) )
  ## TEST CODE
  # data <- data.frame('x' = c(10,40), 'y' = c(10,30), 'x_end' = c(40,10), 'y_end' = c(30,10))
  
  
  # temp <- segmentsDf(data, 2.5, 2.5, 2)
  
  
  #ggplot it
  
  # ggplot(tempNodes, aes(x = x, y = y)) + 
  #  geom_point(size = 12) + xlim(0,50) + 
  # ylim(0,50) + geom_segment(data = temp, aes(x = x, xend = x_end, y = y, yend = y_end))
  
  
  
  
  
  
  
  
  ############### short arrows players
  
  # short_segments_Players <- segmentsDf(all_data, 2.7, 2.7, 1.15)
  short_segments_Players <- segmentsDf(all_data, 1.5, 1.5, 1)
  
  #########################
  ## new plot shortened
  
  ggplot() +
    #annotate_pitch(dimensions = pitch_opta,fill = "black") +
    annotate_pitch(dimensions = pitch_opta,fill = "gray8") +
    theme_pitch()+
  
    geom_segment(data = short_segments_Players %>%filter(n_passes>3), 
                 aes(x = x, y = y, xend = x_end, yend = y_end,color=n_passes),size=1.3,
                 position="nudge", arrow = arrow(length = unit(0.1, "inches") ) )+
    # scale_colour_gradientn(colours = terrain.colors(10))+
    scale_colour_gradient(name="Passes to a player",space = "Lab",low = "lightpink", high = "red2")+
  #  scale_colour_manual(values = c("red", "blue", "green"))+
    geom_point(data=starting_data, aes(x=x, y=y, size= pass_success),color="gray48")+ 
    scale_size(name="Passes Success Total",range = c(0, 7))+
    geom_text_repel(data=distinct(starting_data,player_name,.keep_all=TRUE), 
                    aes(x=x, y=y, label=player_name),colour="white",fontface="bold")+
    ggtitle("Real Madrid vs Betis Min 5 Passes (BET 3-5 RMA) - Player Average Position based on ball contacts  - David Fombella @bigdatasport") 
  
   # + theme(legend.position="none")
  
  
  
  