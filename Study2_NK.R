# Load packages 
library(dplyr)
library(tidyverse)
library(ggplot2)
library(readxl)
library(plm)
library(stargazer)

# Load data
datapath <- 'Study 2 Data.xlsx'
Study_2_Data <- read_excel(datapath)

# Convert variable 'type' to factor
names <- c(7)
Study_2_Data <- Study_2_Data %>%
  mutate(across(names, as.factor))

# Create new variable 'd' to indicate high novelty vs. low novelty film categories
Study_2_Data$d <- recode(Study_2_Data$type, "1" = "Lower novelty film categories",
                         "2" = "Lower novelty film categories", 
                         "3" = "Lower novelty film categories", 
                         "4" = "Lower novelty film categories", 
                         "5" = "Higher novelty film categories", 
                         "6" = "Higher novelty film categories")

# Fit OLS regression models
# Model 1: variability of archival audience ratings v. categories
# Model 2: same as model 1 but include number of ratings as a regressor
# Model 3: same as model 1 but using only films with >= 20 ratings
# Dummy variables in codebook do NOT specify what reference category is used
lmodel1_NEXT <- lm(SD ~ d1 + d2 + d3 + d4 + d5, data = Study_2_Data)
lmodel1_Midnight <- lm(SD ~ d1 + d2 + d3 + d4 + d5a, data = Study_2_Data)

lmodel2_NEXT <- lm(SD ~ d1 + d2 + d3 + d4 + d5 + n, data = Study_2_Data)
lmodel2_Midnight <- lm(SD ~ d1 + d2 + d3 + d4 + d5a + n, data = Study_2_Data)

Study_2_DataSubset <- subset(Study_2_Data, n >= 20)
lmodel3_NEXT <- lm(SD ~ d1 + d2 + d3 + d4 + d5, data = Study_2_DataSubset)
lmodel3_Midnight <- lm(SD ~ d1 + d2 + d3 + d4 + d5a, data = Study_2_DataSubset)

# Get Huber–White robust standard errors for the reg coefficients for each model
cov1_NEXT <- vcovHC(lmodel1_NEXT, "HC1") # var-covar matrix of coefficient estimates
robust_se_lmodel1_NEXT <- sqrt(diag(cov1_NEXT))
cov1_Midnight <- vcovHC(lmodel1_Midnight, "HC1") 
robust_se_lmodel1_Midnight <- sqrt(diag(cov1_Midnight))

cov2_NEXT <- vcovHC(lmodel2_NEXT, "HC1") 
robust_se_lmodel2_NEXT <- sqrt(diag(cov2_NEXT))
cov2_Midnight <- vcovHC(lmodel2_Midnight, "HC1") 
robust_se_lmodel2_Midnight <- sqrt(diag(cov2_Midnight))

cov3_NEXT <- vcovHC(lmodel3_NEXT, "HC1") 
robust_se_lmodel3_NEXT <- sqrt(diag(cov3_NEXT))
cov3_Midnight <- vcovHC(lmodel3_Midnight, "HC1") 
robust_se_lmodel3_Midnight <- sqrt(diag(cov3_Midnight))

# stargazer(lmodel, lmodel, type = "text",
          # se = list(NULL, robust_se))

# Generate Figure 1
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
  theme(text=element_text(size = 16)) +
  theme(legend.position="top") +
  theme(legend.text=element_text(size=16))+
  labs(color=NULL) +
  guides(fill = FALSE) +
  guides(color = guide_legend(override.aes = list(size = 5)))
