# Preamble

library(dplyr)
library(rjson)
library(ggplot2)

setwd("C:/Users/jack.werner1/Documents")

# Read/format data
pitch15 <- read.csv(file = "PitchData_2015.csv")

strikeNames <- c("Called Strike", "Swinging Strike", "Foul", "Foul Bunt",
                 "Swinging Strike (Blocked)", "Foul Tip", "Foul (Runner Going)",
                 "Swinging Pitchout", "Foul Pitchout", "Missed Bunt")

ballNames <- c("Ball", "Ball In Dirt", "Intent Ball", "Pitchout", "Automatic Ball")

simpleResults <- data.frame(event = as.character(sort(unique(pitch15$event))),
                            simpleEvent = c("Out", "Out", "Out", "Out", "HBP",
                                            "Hit", "Out", "Hit", "Out", "Out",
                                            "Out", "Out", "Out", "Out", "Out",
                                            "HBP", "Hit", "BB", "Out", "Out",
                                            "Out", "Out", "Out", "Out", "Out",
                                            "Hit", "K", "K", "Hit", "Out", "BB"),
                            stringsAsFactors = F)

pitch15_2 <- left_join(pitch15, simpleResults, by = "event")

pitch15_2 <- pitch15_2 %>% group_by(gid, ab_num) %>%
  mutate(pitchResult = trimws(pitch_result), type = trimws(type),
         finalCount = paste0(b, "-", s),
         last = row_number() == n(),
         nballs = pmin(cumsum(type == "B"), 3), nstrikes = pmin(cumsum(type == "S"), 2),
         nextState = ifelse(last, simpleEvent, paste0(nballs, "-", nstrikes)),
         count = lag(as.character(nextState), default = "0-0"),
         balls = lag(nballs, default = 0), strikes = lag(nstrikes, default = 0))

# Kershaw

kershaw <- pitch15_2 %>% filter(pitcher == 477132)
kershaw$pitch_type <- factor(trimws(kershaw$pitch_type))

table(kershaw$pitch_type)/nrow(kershaw)*100

kershaw3 <- filter(kershaw, pitch_type %in% c("CU", "FF", "SL"))
kershaw3$pitch_type <- droplevels(kershaw3$pitch_type)

table(kershaw3$pitch_type)/nrow(kershaw3)*100

ggplot(data=kershaw3, aes(pitch_type, y = (..count..)/sum(..count..))) + 
  facet_grid(balls ~ strikes) +
  geom_bar(fill = "blue") +
  labs(x = "Pitch type", y = "Frequency")


kershaw_freq <- kershaw3 %>% group_by(balls, strikes, pitch_type) %>%
  summarize(npitch = n()) %>%
  ungroup %>% group_by(balls, strikes) %>%
  mutate(total = sum(npitch), freq = npitch/total*100) %>%
  ungroup()

kershaw_freq$pitch_type <- factor(kershaw_freq$pitch_type, levels = c("FF", "CU", "SL"))

ggplot(data=kershaw_freq, aes(pitch_type, y = freq)) + 
  facet_grid(balls ~ strikes) +
  geom_bar(stat = "identity", aes(fill = pitch_type)) +
  labs(x = "Pitch type", y = "Relative Frequency (Percent)")

# Ternary plots
library(ggtern)
library(tidyr)

kershaw_tern <- kershaw_freq %>%
  select(balls, strikes, pitch_type, freq) %>%
  spread(key = pitch_type, value = freq, fill = 0) %>%
  mutate(count = paste0(balls, "-", strikes)) %>%
  select(count, FF, CU, SL) %>%
  as.data.frame()

kershaw_tern

nv = 0.05
pn = position_nudge_tern(y=nv,x=-nv/2,z=-nv/2)

pn = position_nudge_tern(y=nv,x=0,z=0)

ggtern(data=kershaw_tern, aes(x = FF, y = CU, z = SL)) + 
  geom_mask() +
  geom_point() + 
  geom_text(position = pn, aes(label = count)) +
  theme_bw()  +
  theme_showarrows() + 
  theme(legend.position      = c(0, 1),
        legend.justification = c(0, 1),
        legend.box.just      = 'left') +
  
  #tweak guides
  guides(shape= guide_legend(order   =1,
                             override.aes=list(size=5)),
         size = guide_legend(order   =2),
         fill = guide_colourbar(order=3))






