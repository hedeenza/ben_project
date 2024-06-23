# Loading the csv back in 
mtg_cards <- read.csv("Data/mtg_cards.csv")

# Exploring the data
table(mtg_cards$artist) %>% as.data.frame() %>% arrange(desc(Var1))


table(mtg_cards$released_at) %>% as.data.frame() %>% arrange(desc(Var1))

mtg_cards %>% filter(artist == "John Avon")  

# graphing release timeline   
release_timeline <- table(mtg_cards$released_at) %>% as.data.frame() %>% arrange(desc(Var1))
release_labels <- cbind(release_timeline, release_timeline$Var1)
names(release_labels) <- c("Release", "Count", "Year")

release_graphing <- release_labels %>% separate(Year, c("Year", "Month", "Day")) %>% arrange(Release) %>% print()

release_graph <- release_graphing %>% 
  ggplot(aes(x = Release, y = Count, color = Year)) +
  geom_point() +
  geom_vline(xintercept = c(17,44,162,178)) +
  geom_vline(xintercept = 165, color = "red") + 
  
  annotate("text", x = 15, y = 500, label = "Weatherlight", angle = 90) + 
  annotate("text", x = 42, y = 500, label = "Mirridon", angle = 90) + 
  annotate("text", x = 160, y = 500, label = "Modern Horizons", angle = 90) + 
  annotate("text", x = 163, y = 500, label = "Throne of Eldrain", angle = 90, color = "red") + 
  annotate("text", x = 176, y = 500, label = "Secret Lair", angle = 90) + 
  
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 1, size = 5))

release_graph

ggsave(plot = release_graph, "Graphs/Release_Timeline.png", width = 40, height = 15, units = "cm")


"1993 - 97 = beginnings
97 - 02 = experimental (mirridin facelift)
11 - 19 = modern (eliminating a lot of the janky old stuff)
  'modern master set' = nothing new, but stuff you need to play modern
  most people playing modern, but then paradigm shift when 'modern horizons' releases (new cards specifically designed for modern)
    not legal in standard play
19 print some broken cards
  shift to commander focus in 2020 with the release of secret lair (cards specifically referencing commander, rather then just so happening to be good)
modern horizons 2 = the end of 'designed for modern' era 

97 weatherlite*, 03 mirridin*, 11 modern horizons, 13 modern masters, 15 modern masters 2
19 rotating modern horizons broke things*, 2020 secret lair starts commander*, 2021 end of dfm era
  19 power creep really starts (throne of eldrain)

standard = 2 years
  
extended = 4-6 years
  
legacy = everything thats not banned
  lots of counter-spells
vintage = everything except the 'ante' mechanic or cards that cause 'problems', like playing another game with the Sheherrazad
  win on turn 2"

# Artist contributions
artist_count <- table(mtg_cards$artist) %>% as.data.frame() %>% arrange(desc(Freq), Var1)
artist_top <- artist_count %>% filter(Freq >= 100) %>% mutate(Count = fct_reorder(Var1, Freq, .desc = T)) %>% print()

artist_graph <- artist_top %>%
  ggplot(aes(x = Count, y = Freq, fill = Count)) +
  geom_col() +
  annotate("text", x = 15, y = 250, label = "John Avon") +
  annotate("rect", xmin = 14.95, xmax = 15.05, ymin = 190, ymax = 235) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 5),
        axis.title.x = element_blank())  +
  ylab("Number of Art Contributions") + 
  theme(legend.position = "none")

artist_graph

ggsave(plot = artist_graph, "Graphs/Artist_Contribution.png", width = 16, height = 9, units = "cm")



table(mtg_cards$artist) %>% as.data.frame() %>% arrange(Var1)
