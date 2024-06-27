library(tidyverse)
library(stringr)

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

    #ggsave(plot = release_graph, "Graphs/Release_Timeline.png", width = 40, height = 15, units = "cm")


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

    # ggsave(plot = artist_graph, "Graphs/Artist_Contribution.png", width = 16, height = 9, units = "cm")


# Land by Year
table(mtg_cards$type_line) %>% as.data.frame() %>% arrange(desc(Freq))

just_lands <- mtg_cards %>% filter(type_line == "Land")
land_timeline <- just_lands %>% select(name, released_at) %>% separate(released_at, c("Year", "Month", "Day")) %>% arrange(Year)
land_graph <- land_timeline %>% group_by(Year) %>% count(Year)

land_plot <- land_graph %>% 
  ggplot(aes(x = Year, y = n, fill = Year)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 12),
        legend.position = "none") +
  ylab("Number of Lands Released")


land_plot


# Lands in 2022
lands_2022 <- just_lands %>% 
                select(name, set_name, released_at) %>% 
                separate(released_at, c("Year", "Month", "Day")) %>% 
                filter(Year == 2022) %>%
                arrange(Month, Day, set_name)

lands_2022_graph <- lands_2022 %>% 
                      group_by(set_name) %>%
                      count(Day)

lands_2022_plot <- lands_2022_graph %>%
  ggplot(aes(x = set_name, y = n, fill = set_name)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 9),
        legend.position = "none") +
  ylab("Number of Lands Released") +
  xlab("") + 
  
  scale_x_discrete(labels = c("Commander Legends: Battle for Baldur's Gate" = "Commander Legends: \n Battle for Baldur's Gate",
                              "Dominaria United Commander" = "Dominaria United \n Commander",
                              "Heads I Win, Tails You Lose" = "Heads I Win, \n Tails You Lose", 
                              "Kamigawa: Neon Dynasty" = "Kamigawa: \n Neon Dynasty",
                              "Neon Dynasty Commander" = "Neon Dynasty \n Commander",
                              "New Capenna Commander" = "New Capenna \n Commander",
                              "Warhammer 40,000 Commander" = "Warhammer 40,000 \n Commander"))

lands_2022_plot

# Who did the lands of 2022?
land_art_2022 <- 
  mtg_cards %>%
  separate(released_at, c("Year", "Month", "Day")) %>%
  filter(type_line == "Land" & Year == "2022") %>%
  count(artist) %>%
  arrange(desc(n))
  
land_art_2022_sorted <- 
  land_art_2022 %>%
  mutate(Count = fct_reorder(artist, n, .desc = T))

land_art_2022_plot <-
  land_art_2022_sorted %>%
  ggplot(aes(x = Count, y = n, fill = Count)) +
    geom_col() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 9),
        legend.position = "none") +
    ylab("Number of Land Arts") +
    xlab("")
    

land_art_2022_plot
    
# Just John Avon
just_ja <-
  mtg_cards %>%
  separate(released_at, c("Year", "Month", "Day")) %>%
  filter(artist == "John Avon")

just_ja_timeline <-
  table(just_ja$Year) %>% 
  as.data.frame() %>% 
  arrange()

just_ja_plot <- # Timeline of John Avon Arts
  just_ja_timeline %>%
  ggplot(aes(x = Var1, y = Freq, fill = Var1)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 9),
        legend.position = "none") +
  ylab("Number of Arts") +
  xlab("")

just_ja_plot


just_ja_type <- 
  str_split_fixed(just_ja$type_line, " — ", 2) %>%
  print()

  ja_type_clean <- table(just_ja_type[,1]) %>% as.data.frame() %>% arrange()
  
  ja_type_plot <- # how many of each type of card John Avon did art for
    ja_type_clean %>%
      ggplot(aes(x = Var1, y = Freq, fill = Var1)) + 
      geom_col() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 9),
            legend.position = "none") +
      ylab("Number of Arts") +
      xlab("")
  
  ja_type_plot

# Reserved List by Year
reserved_years <- mtg_cards %>% 
  filter(reserved != FALSE) %>% 
  select(name, set_name, released_at) %>% 
  separate(released_at, c("Year", "Month", "Day"))

reserved_timeline <- reserved_years %>% group_by(Year) %>% count(Day)

reserved_years %>% nrow()

reserved_timeline_plot <- reserved_timeline %>%
  ggplot(aes(x = Year, y = n, fill = Year)) + 
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 9),
        legend.position = "none") +
  ylab("Number on the Reserved List") +
  xlab("")

  reserved_timeline_plot

reserved_month <- reserved_years %>% group_by(Month) %>% count(Month) %>% arrange(desc(n))
sum(reserved_month$n)

# Which months had the most cards on the reserve list?
reserved_month_plot <- reserved_month %>% 
  ggplot(aes(x = Month, y = n, fill = Month)) +
  geom_col(position = position_dodge()) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5, size = 12),
        legend.position = "none"
        #panel.background = element_blank()
  ) +
  ylab("Number of Cards on the Reserved List") +
  xlab("") +
  scale_x_discrete(labels = c('01' = "Jan", "02" = "Feb", "03" = "Mar", "04" = "Apr", "05" = "May", "06" = "Jun",
                              "07" = "Jul", "08" = "Aug", "09" = "Sep", "10" = "Oct", "11" = "Nov", "12" = "Dec")) 

reserved_month_plot
    # ggsave(plot = reserved_month_plot, "Graphs/ReservedList_Months.png", width = 16, height = 9, units = "cm")

# What sets were released during those peak reserved list count?
reserved_years %>% 
  filter(Month == '06' | Month == '09' | Month == '10') %>% 
  select(set_name, Year, Month) %>%
  unique() %>% arrange(Month, Year, set_name)

# Which sets have the most reserved list cards?
reserved_sets <- table(reserved_years$set_name) %>% as.data.frame() %>% mutate(Count = fct_reorder(Var1, Freq, .desc = T))

reserved_sets_plot <- reserved_sets %>% 
  ggplot(aes(x = Count, y = Freq, fill = Count)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6),
        legend.position = "none") +
  ylab("Reserved List Cards") +
  xlab("") + 
  scale_x_discrete(labels = c("Duel Decks: Phyrexia vs. the Coalition" = "Duel Decks: \n Phyrexia vs. the Coalition"))

  reserved_sets_plot

  #  ggsave(plot = reserved_sets_plot, "Graphs/ReservedList_Sets.png", width = 16, height = 9, units = "cm")


# Rarity Comparison Mirage and Visions
rarity_comparison <- mtg_cards %>% 
  filter(set_name == "Mirage" | set_name == "Visions") %>%
  group_by(set_name) %>%
  count(rarity)

rarity_comparison %>% ggplot(aes(x = set_name, y = n, fill = rarity)) +
  geom_col(position = position_dodge()) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = 12),
        legend.position = "right") +
  ylab("Number of Cards") +
  xlab("")

# Just the reserved list from Mirage and Visions
rarity_comparison_reserved <- mtg_cards %>% 
  filter(set_name == "Mirage" | set_name == "Visions") %>%
  filter(reserved != FALSE) %>%
  group_by(set_name) %>%
  count(rarity)

rarity_comparison_reserved %>% ggplot(aes(x = set_name, y = n, fill = rarity)) +
  geom_col(position = position_dodge()) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = 12),
        legend.position = "right") +
  ylab("Number of Cards") +
  xlab("")


# Rarity by Year 
rarity_year <- mtg_cards %>% 
  separate(released_at, c("Year", "Month", "Day")) %>%
  group_by(Year) %>%
  count(rarity)
rarity_year_plot <-
  rarity_year %>%
  ggplot(aes(x = Year, y = n, fill = rarity)) +
  geom_col(width = 0.7, position = position_dodge()) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = 8),
        legend.position = "right") +
  ylab("Number of Cards") +
  xlab("") +
  scale_y_continuous(breaks = c(100,200,300,400,500,600,700,800,900,1000,1100,1200,1300),
                     labels = c('100','200','300','400','500','600','700','800','900','1000','1100','1200','1300'))

rarity_year_plot

  #  ggsave(plot = rarity_year_plot, "Graphs/Rarity_Year.png", width = 40, height = 10, units = "cm")


# Rarity before and after everything went off the rails
rarity_count <-
  mtg_cards %>%
  separate(released_at, c("Year", "Month", "Day")) %>%
  filter(Year == "2019" | Year == "2022") %>%
  group_by(Year) %>%
  count(rarity) %>%
  as.data.frame()

rarity_percent <- 
  rarity_count %>%
  group_by(Year) %>%
  mutate(Total = sum(n),
         Percent = n/Total)

rarity_percent_plot <-
  rarity_percent %>%
  ggplot(aes(x = Year, y = Percent, fill = rarity)) +
  geom_col(width = 0.7, position = position_dodge()) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = 12),
        legend.position = "right") +
  ylab("Percent of Total Cards Released") +
  xlab("")

rarity_percent_plot

  #  ggsave(plot = rarity_percent_plot, "Graphs/Rarity_Rails.png", width = 16, height = 16, units = "cm")


# Rarity Regression 
rarity_timeline <- 
  mtg_cards %>%
  separate(released_at, c("Year", "Month", "Day")) %>%
  group_by(Year) %>%
  count(rarity) %>%
  mutate(Total = sum(n),
         Percent = n/Total) %>%
  select(Year, rarity, Percent) %>%
  print()

    # Graphing Rarity over Time
    rarity_timeline_plot <- 
      rarity_timeline %>%
      filter(rarity != "bonus" & rarity != "special") %>%
      ggplot(aes(x = Year, y = Percent, group = rarity, color = rarity)) +
      scale_y_continuous(breaks = c(.05,.10,.15,.20,.25,.30,.35,.40,.45,.50),
                         labels = c("5%", "10%", "15%", "20%", "25%", "30%", "35%", "40%", "45%", "50%")) +
      scale_color_manual(name = "Rarity",
                         breaks = c('common', 'uncommon', 'rare', 'mythic', 'special'),
                         values = c(rare = "#D3202A", uncommon = "#00733E", mythic = "#0E68AB", special = "purple", common = "#150B00")) +  
      geom_line(linewidth = 0.25) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
      xlab("Year") +
      ylab("Percent of Released Cards")
      
      #annotate("rect", xmin = 1, xmax = 5.25, ymin = 0, ymax = 0.475, fill = "blue", alpha = 0.05) +
      #annotate("rect", xmin = 7.75, xmax = 9.25, ymin = 0, ymax = 0.475, fill = "blue", alpha = 0.05) +
      #annotate("rect", xmin = 28.75, xmax = 31.25, ymin = 0, ymax = 0.475, fill = "blue", alpha = 0.05)
      
    rarity_timeline_plot
    
    ggsave(plot = rarity_timeline_plot, "Graphs/Rarity_Line_Timeline.png", width = 16, height = 9, units = "cm", dpi = 300)

?geom_line
    
rarity_wide <- 
  rarity_timeline %>%
  pivot_wider(names_from = rarity, values_from = Percent) %>%
  print()

rarity_timeline_clean <- # this will become a kable table
  mtg_cards %>%
  separate(released_at, c("Year", "Month", "Day")) %>%
  group_by(Year) %>%
  count(rarity) %>%
  mutate(Total = sum(n),
         Percent = round((n/Total)*100,1)) %>%
  select(Year, rarity, Percent) %>%
  pivot_wider(names_from = rarity, values_from = Percent) %>%
  select(Year, common, uncommon, rare, mythic, special, bonus) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  mutate(Sum = sum(common, rare, uncommon, special, mythic, bonus)) %>%
  print(n=31)

# When is there a higher percentage of uncommon than common?
rarity_timeline_clean %>% filter(uncommon - common >= 0) %>% print()

# When is there a higher percentage of rare than common?
rarity_timeline_clean %>% filter(rare - common >= 0) %>% print()

# When is there a higher percentage of rare than uncommon?
rarity_timeline_clean %>% filter(rare - uncommon >= 0) %>% print()


# Regressions of rarity by year 
rarity_transformed <- 
  rarity_timeline_clean %>% 
  transform(Year = as.numeric(Year))

str(rarity_transformed)

lm(mythic ~ Year, data = rarity_transformed)




# Rarity Descriptive Statistics
rarity_summmary <- 
  summary(rarity_timeline_clean) %>% 
  as.data.frame() %>%
  separate(Freq, sep = ":", c("Value", "Number"))

rarity_summary_clean <- rarity_summmary[-c(1:6),] %>% print()

rarity_trim_rarity <- 
  str_trim(rarity_summary_clean$Var2, side = "both")

rarity_trim_measure <- 
  str_trim(rarity_summary_clean$Value, side = "both")

rarity_trim_value <- 
  str_trim(rarity_summary_clean$Number, side = "both") %>%
  str_remove_all("0+$") %>%
  str_replace_all("[:punct:]$", ".0")

rarity_trim <- 
  data.frame(rarity_trim_rarity, rarity_trim_measure, rarity_trim_value)

rarity_summary_final <- # To become a kable table
  rarity_trim %>%
  pivot_wider(names_from = rarity_trim_rarity, values_from = rarity_trim_value) %>% print()


# What's the "Special" Card?
mtg_cards %>% filter(rarity == "special") %>% select(name, released_at, set_name, rarity) %>% arrange(released_at)


# Sets per Year
year_set <- 
  mtg_cards %>%
  separate(released_at, c("Year", "Month", "Day"))

unique_sets <- 
  table(year_set$set_name) %>% 
  as.data.frame() %>% 
  arrange() %>% print()

names(unique_sets) <- c("set_name", "Freq")

set_years <- 
  year_set %>%
  select(Year, set_name)

merged_sets_timeline <- 
  merge(unique_sets, set_years, .by = set_name, all = TRUE) %>% 
  select(set_name, Year) %>%
  unique() %>%
  arrange(Year) %>% print()

set_release_timeline <- 
  table(merged_sets_timeline$Year) %>% 
  data.frame() %>% 
  arrange()

set_release_timeline_plot <- 
  set_release_timeline %>%
  ggplot(aes(x = Var1, y = Freq, fill = Var1)) +
  geom_col() +
  xlab("Year") + 
  ylab("Number of Sets Released") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

set_release_timeline_plot

# Add a box plot of the rarity descriptive statistics