library(jsonlite)
library(tidyverse)
library(data.table)

# Loading the JSON, converting to a data frame
data <- fromJSON("Data/scryfall_mtg_cards_feb_2023.json")
df <- as.data.frame(data)

# Unpackgin the weird lists within lists
df_legalities <- df$legalities %>% as.data.frame()
df_price <- df$prices %>% as.data.frame()

# Pulling the vectors of interest
df_selected <- df %>% select(
  
name,
lang,
released_at,
mana_cost,
cmc,
type_line,
oracle_text,
colors,
color_identity,
keywords,
reserved,
set_name,
set_type,
rarity,
artist,
power,
'_type',
toughness,
booster,
flavor_text,
produced_mana,
oversized)

# Combining everything together
df_combined <- cbind(df_selected, df_legalities, df_price)

# Writing to a CSV
  # write_csv(df_combined, "Data/mtg_cards.csv")

# Loading the csv back in 
mtg_cards <- read.csv("Data/mtg_cards.csv")

# Exploring the data
table(mtg_cards$cmc) %>% as.data.frame() %>% arrange(desc(Var1))

df_combined %>% filter(cmc == 16)  
  
