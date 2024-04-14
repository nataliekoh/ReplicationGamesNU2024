
#load packages and data 

library(dplyr)
library(tidyverse)
library(ggplot2)

data(Study_2_Data)

#convert variable 'type' to factor

names <- c(7)
Study_2_Data <- Study_2_Data %>%
  mutate(across(names, as.factor))

#create new variable 'd' to indicate high novelty vs. low novelty film categories

Study_2_Data$d <- recode(Study_2_Data$type, "1" = "Lower novelty film categories","2" = "Lower novelty film categories", "3" = "Lower novelty film categories", "4" = "Lower novelty film categories", "5" = "Higher novelty film categories", "6" = "Higher novelty film categories")

#generate plot

library(ggplot2)

ggplot(Study_2_Data, aes(x = type, y = SD, fill =d)) +
  scale_x_discrete(labels = c('U.S.\n Documentary','World \n Documentary','U.S.\n Narrative','World \n Narrative','Midnight','NEXT')) +
  geom_jitter(aes(color= d),
              alpha = .6,
              width = .27) +
  
  stat_summary(fun.data = "mean_cl_boot", geom = "crossbar",
               position = position_dodge(width=1),
               size=.3, width=.5, alpha=.3) +
  
  ylab("Average Standard Deviation in Ratings") +
  xlab(" ") +
  theme_bw() +
  theme(panel.border = element_blank(), axis.line = element_line()) +
  scale_fill_manual(values=c("cornflowerblue","magenta3")) +
  scale_color_manual(values=c("cornflowerblue", "magenta3")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor =
          element_blank()) +
  theme(text=element_text(size = 20)) +
  theme(legend.position="top") +
  theme(legend.text=element_text(size=20))+
  labs(color=NULL) +
  guides(fill = FALSE) +
  guides(color = guide_legend(override.aes = list(size = 5)))
