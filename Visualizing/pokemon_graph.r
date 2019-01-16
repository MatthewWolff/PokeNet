library(tidyverse)

battles <- read.csv("~/Projects/Pokemon/raw_data/combats.csv")
tests <- read.csv("~/Projects/Pokemon/raw_data/tests.csv")
pokemon <- read.csv("~/Projects/Pokemon/raw_data/pokemon.csv", na.strings=c("",NA))

# set up coloring
types <- unique(pokemon$Type.1)
types2 <- unique(pokemon$Type.2)
colors <- c("#7AC74C", "#EE8130", "#6390F0", "#A6B91A", "#A8A77A", "#A33EA1", "#F7D02C", "#E2BF65", "#D685AD", "#C22E28", "#F95587", "#B6A136", "#735797", "#96D9D6", "#6F35FC", "#705746", "#B7B7CE", "#A98FF3")

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
  labs(y="Offensive Ability (Highest Attack Stat)") +
  scale_fill_manual(values=colors) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank()) +
  guides(fill=FALSE) + # remove legend for fill
  coord_polar()

ggplot(data = poke_graph %>%
         select(c(Gen, Value, Legendary)) %>%
         group_by(Gen, Legendary) %>%
         summarize(Value = mean(Value))) + 
  geom_bar(mapping = aes(Gen, Value), col = "black", stat = "identity") + 
  facet_wrap(~ Legendary, 
             labeller = as_labeller(c("FALSE" = "Non-Legendary", "TRUE" = "Legendary"))) +
  ggtitle("Overall Value of Pokémon across generations") + 
  labs(x="Generation", y="Overall Value (Offensive + Defensive)")

poke_scatter <- ggplot(data = poke_graph %>%
                         select(c(Gen, Value, Legendary, Type)) %>%
                         group_by(Gen, Legendary) %>%
                         mutate(Type = factor(Type, levels = types, ordered = TRUE))) + 
  geom_point(mapping = aes(Gen, Value, col=Type), position="jitter", size = 2) + 
  facet_wrap(~ Legendary, 
             labeller = as_labeller(c("FALSE" = "Non-Legendary", "TRUE" = "Legendary"))) +
  ggtitle("Overall Value of Pokémon across generations") + 
  labs(x="Generation", y="Overall Value (Highest Attack Stat + Average Defensive Stat)") + 
  scale_color_manual(values=colors) +
  coord_cartesian(ylim = c(0, 300)) +
  theme(legend.key.size = unit(0.65, "cm"),
        plot.title = element_text(hjust = 0.5),
        axis.line = element_line(colour = "black"))

# markers <- poke_graph %>%
#   select(c(Gen, Value, Legendary, Type)) %>%
#   group_by(Gen, Legendary) %>%
#   summarize(Median = median(Value))
#
# for (i in 1:length(means$Gen)) {
#   poke_scatter <- poke_scatter +
#     geom_segment(aes(x=x, y=y ,yend=yend ,xend=xend),
#                  inherit.aes=FALSE,
#                  data=data.frame(x=as.numeric(markers$Gen[i]) - 0.3,
#                                  xend=as.numeric(markers$Gen[i]) + 0.3,
#                                  y=markers$Median[i],
#                                  yend=markers$Median[i],
#                                  Legendary=markers$Legendary[i]),
#                  size=1)
# }
poke_scatter

