library(tidyverse)

battles <- read.csv("~/Projects/Pokemon/raw_data/combats.csv")
tests <- read.csv("~/Projects/Pokemon/raw_data/tests.csv")
pokemon <- read.csv("~/Projects/Pokemon/raw_data/pokemon.csv", na.strings=c("",NA))

# set up coloring
types <- unique(pokemon$Type.1)
types2 <- unique(pokemon$Type.2)
colors <- c("chartreuse4","firebrick2","deepskyblue1","chartreuse2","black","darkorchid2","yellow","salmon4","palevioletred1","tomato4","purple","saddlebrown","snow2","paleturquoise1","red","gray13","darkgray","lightslateblue")

## Graphing poke-data
poke_graph <- pokemon %>%
  gather(key=tmp, value=Type, Type.1, Type.2) %>%
  select(-c(tmp)) %>% # toss out whether primary or secondary
  rename(ID = X.,
         Sp.Atk = Sp..Atk,
         Sp.Def = Sp..Def,
         Gen = Generation) %>%
  mutate(Power = pmax(Attack, Sp.Atk), # you can choose your stronger stat
         Resilience = (Defense + Sp.Def)/2, # it's probably 50/50 what they'll attack with
         Value = Power + Resilience) %>%
  mutate(Legendary = as.logical(Legendary), # convert to real booleans
         Gen = factor(Gen), # refactor for coloring
         Type = factor(Type, levels = types, ordered = TRUE)) %>% 
  filter(!is.na(Type)) # remove duplicates that don't have a secondary type

ggplot(data=poke_graph, mapping = aes(Type, Power, fill=Type)) + 
  geom_boxplot(outlier.alpha = 0.5) + 
  facet_wrap(~ Legendary, 
             labeller = as_labeller(c("FALSE" = "Non-Legendary", "TRUE" = "Legendary"))) +
  ggtitle("Comparing Offensive Ability of Pokemon by Type and Legendary Status") + 
  labs(y="Offensive Ability") +
  scale_fill_manual(values=colors) +
  guides(fill=FALSE) + # remove legend for fill
  coord_polar()

ggplot(data = poke_graph %>%
         select(c(Gen, Value, Legendary)) %>%
         group_by(Gen, Legendary) %>%
         summarize(Value = mean(Value))) + 
  geom_bar(mapping = aes(Gen, Value, fill=Gen), col = "black", stat = "identity") + 
  facet_wrap(~ Legendary, 
             labeller = as_labeller(c("FALSE" = "Non-Legendary", "TRUE" = "Legendary"))) +
  ggtitle("Overall Value of Pokémon across generations") + 
  labs(x="Generation", y="Overall Value (Offensive + Defensive)")

ggplot(data = poke_graph %>%
         select(c(Gen, Value, Legendary, Type)) %>%
         group_by(Gen, Legendary) %>%
         mutate(Type = factor(Type, levels = types, ordered = TRUE))) + 
  geom_point(mapping = aes(Gen, Value, col=Type), position="jitter", size = 2) + 
  facet_wrap(~ Legendary, 
             labeller = as_labeller(c("FALSE" = "Non-Legendary", "TRUE" = "Legendary"))) +
  ggtitle("Overall Value of Pokémon across generations") + 
  labs(x="Generation", y="Overall Value (Offensive + Defensive)") + 
  scale_color_manual(values=colors) +
  coord_cartesian(ylim = c(0, 300)) +
  theme(legend.key.size = unit(0.65, "cm"))  # resize

