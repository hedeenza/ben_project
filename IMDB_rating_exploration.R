library(tictoc)
library(tidyverse)

# Data
mr_robot <- data.frame(Episode = c(1,2,3,4,5,6,7,8,9,10,11,12,13),
                       Season_1 = c(9.2,8.5,8.1,7.9,8.4,9.1,8.4,9.4,9.2,8.7,NA,NA,NA), 
                       Season_2 = c(8.1,8.2,8.3,8.2,8.8,9.0,9.2,8.5,8.5,9.2,8.5,8.8,NA),
                       Season_3 = c(8.7, 9.1,8.7,8.2,9.7,9.6,9.0,9.1,8.6,9.5,NA,NA,NA),
                       Season_4 = c(9.2,8.7,8.7,8.7,9.7,9.1,9.9,9.2,9.7,8.2,9.4,9.5,9.8))

mr_robot_long <- 
  pivot_longer(mr_robot, 
               cols = c(Season_1:Season_4), 
               names_to = "Season", 
               values_to = "Rating", 
               values_drop_na = T)

mr_robot_ratings <- 
  mr_robot_long |>
  ggplot(aes(x = Episode, y = Rating, color = Season)) + 
    geom_point() +
    scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12,13),
                       labels = c('1','2','3','4','5','6','7','8','9','10','11','12','13')) +
    scale_y_continuous(limits = c(7.5,10)) +
    geom_smooth(method = "loess", se =F, linewidth = 0.5) +
    labs(title = "Mr. Robot Episode Ratings",
         subtitle = "IMDB") +
    scale_color_manual(values = c("black","#bd3531","#90593a","#27560c"))

mr_robot_ratings

ggsave(plot = mr_robot_ratings, "Graphs/mr_robot_ratings.png", width = 16, height = 9, units = "cm", dpi = 300)    


got <- data.frame(Episode = c(1,2,3,4,5,6,7,8,9,10),
                  Season_1 = c(8.9,8.6,8.5,8.6,9.0,9.1,9.1,8.9,9.6,9.4), 
                  Season_2 = c(8.6,8.3,8.7,8.6,8.6,8.9,8.8,8.6,9.7,9.3),
                  Season_3 = c(8.6,8.5,8.7,9.5,8.9,8.7,8.6,8.9,9.9,9.1),
                  Season_4 = c(9.0,9.7,8.7,8.7,8.6,9.7,9.0,9.7,9.6,9.6),
                  Season_5 = c(8.3,8.3,8.4,8.5,8.5,7.9,8.8,9.8,9.4,9.1),
                  Season_6 = c(8.4,9.2,8.6,9.0,9.7,8.3,8.5,8.3,9.9,9.9),
                  Season_7 = c(8.5,8.8,9.1,9.7,8.7,9.0,9.4,NA,NA,NA),
                  Season_8 = c(7.6,7.9,7.5,5.5,5.9,4.0,NA,NA,NA,NA)
                  )

got_long <- 
  pivot_longer(got, 
               cols = c(Season_1:Season_8),
               names_to = "Season", 
               values_to = "Rating", 
               values_drop_na = T)

got_ratings <- 
  got_long |>
  filter(Season != 'Season_8') |>
  ggplot(aes(x = Episode, y = Rating, color = Season)) + 
  geom_point() +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10),
                     labels = c('1','2','3','4','5','6','7','8','9','10')) +
  scale_y_continuous(limits = c(7.75,10.1)) +
  geom_smooth(method = "loess", se =F, linewidth = 0.5) +
  labs(title = "Game of Thrones Episode Ratings",
       subtitle = "IMDB") +
  scale_color_manual(values = c("#3759ac","#000000","#5a8328","#912215","#d08d33","#ddc742","#959595","#143082"))

got_ratings

ggsave(plot = got_ratings, "Graphs/got_ratings.png", width = 16, height = 9, units = "cm", dpi = 300)  
