####################################
## R as a Research Tool
## Snehanjana Chatterjee
## Homework 12
## Due Dec 5, 2022
#####################################


## BACKGROUND ABOUT THE DATA-
## The data here consists of three different species of plants-
## Arisaema, Trillium and Maianthemum grown in two different treatments-
## Ambient (that has Garlic Mustard in the plot) and Weeded (plots without
## Garlic Mustard). Garlic Mustard is an invasive species that releases 
## allelochemicals in the plots and disrupts plant-mycorrhizal
## interactions, potentially altering leaf physiologies.

library(ggplot2)
library(dplyr)
library(tidyverse)


GM_data <- read.csv("Garlic_Mustard.csv")

## Selecting the following columns from the data
GM_update <- select(GM_data, plot, plant_ID, 
           Treatment, sp, Chl, Photo, Trmmol, Cond, Ci, 
           WUE, SLA, LMA, Ci.Ca)

## Grouping the data by treatment, species, plot
GM_group <- group_by(GM_update, plot, Treatment, sp)

## Summarizing with WUE
GM_sum <- summarize(GM_group, WUE = mean(WUE))
 
##  Plot showing WUE in the three different species grown in two
## different treatments
ggplot(GM_sum, aes(y= WUE, x = sp, fill = Treatment)) + 
  geom_boxplot() + 
  scale_y_continuous(limits = c(30,120), breaks = seq(30, 120, 30)) +
  scale_fill_discrete(name = "Treatment", labels = c("Ambient", "Weeded")) +
  labs(x= "Three different native species in two different treatments", 
       y= "Water Use Efficiency", 
       title = "Water Use Efficiency in three different native species") +
    theme(legend.text = element_text(colour = "black", size = 12, face = "bold"))+
  theme(text = element_text(size = 15, face = "bold"))

## According to the plot above, we can see that Trillium has more WUE than the
## other two species. Maianthemum has the lowest WUE. Also, Ambient shows
## more WUE than the Weeded treatment.

## Plot showing just the three different species and the difference in their
## Water Use Efficiency
ggplot(GM_sum, aes(sp, WUE,  fill = sp)) + 
  geom_boxplot() + 
  scale_y_continuous(limits = c(30,120), breaks = seq(30, 120, 30)) + 
  scale_fill_discrete(name = "Species", labels = c("Arisaema", "Maianthemum", "Trillium")) +
  labs(x = "Three different native species", 
       y = "Water Use Efficiency", 
       title = "Water Use Efficiency in three different native species") +
  theme(legend.text = element_text(colour = "black", size = 14, face = "italic")) +
  theme(text = element_text(size = 15, face = "bold"))
  
## Amongst the three species, Trillium has the highest WUE, followed by Arisaema
## and Maianthemum.


## Making a statistical model
GM_model <- lm(WUE ~ Treatment * sp, data = GM_sum)

anova(GM_model)

## Only the sp is significant because the value is less than 0.05


## Installed a new package- emmeans

library(emmeans)
GM_em <- emmeans(GM_model, pairwise ~ sp)

## RESULTS-
## $emmeans
##  sp  emmean    SE df lower.CL upper.CL
## Ari   62.2 14.89 19     31.0     93.3
## Mai   47.4 10.94 19     24.5     70.3
## Tri   93.7  9.88 19     73.1    114.4

## Results are averaged over the levels of: Treatment 
## Confidence level used: 0.95 

## $contrasts
## contrast  estimate   SE df t.ratio p.value
## Ari - Mai     14.7 18.5 19   0.798  0.7085
## Ari - Tri    -31.6 17.9 19  -1.767  0.2074
## Mai - Tri    -46.3 14.7 19  -3.142  0.0142


## The first part of the table is estimated marginal means of the 3 species
## along with standard errors and confidence intervals. The second part
## is the contrasts section that have comparisons of interest. In this
## section, we try to understand the differences between the three species.
## As we can see, the p- value difference between Maianthemum and Trillium
## is less than 0.05, which is statistically significant.


## I was trying basic things with this package and I used the adjust
## function to skip multiple comparisons.
GM_em1 <- emmeans(GM_model, pairwise ~ sp, type = "response", adjust ="none")

GM_em1$contrasts %>%
  summary(infer = TRUE)

## Here I tried to do the same kind of analysis like the previous one 
## but with Stomatal Conductance

## Grouping the data by treatment, species, plot
GM_group <- group_by(GM_update, plot, Treatment, sp)

## Summarizing the data with Conductance
GM_Cond <- summarize(GM_group, Cond = mean(Cond))
view(GM_Cond)

##  Plot showing Stomatal Conductance in the three different species grown in two
## different treatments
ggplot(GM_Cond, aes(y= Cond, x= sp, fill = Treatment)) + 
  geom_boxplot() + 
  scale_y_continuous(limits = c(0.02,0.28), breaks = seq(0.02,0.28, 0.08)) +
  scale_fill_discrete(name = "Treatment", labels = c("Ambient", "Weeded")) +
  labs(x= "Three different native species in two different treatments", 
       y= "Stomatal Conductance", 
       title = "Stomatal Conductance in three different native species") +
  theme(legend.text = element_text(colour = "black", size = 12, face = "bold")) +
  theme(text = element_text(size = 15, face = "bold"))

## According to the plot above, we can see the Maianthemum has the highest
## Stomatal conductance followed by Arisaema and Trillium. But there's not much 
## in stomatal conductance between Trillium and Arisaema. Ambient treatment
## shows more stomatal conductance than weeded ones.


## Plot showing just the three different species and the difference in their
## Stomatal Conductance
ggplot(GM_Cond, aes(sp, Cond ,  fill = sp)) + 
  geom_boxplot() + 
  scale_y_continuous(limits = c(0.02,0.28), breaks = seq(0.02,0.28, 0.08)) + 
  scale_fill_discrete(name = "Species", labels = c("Arisaema", "Maianthemum", "Trillium")) +
  labs(x = "Three different native species", 
       y = "Stomatal Conductance", 
       title = "Stomatal Conductance in three different native species") +
  theme(legend.text = element_text(colour = "black", size = 14, face = "italic")) +
  theme(text = element_text(size = 15, face = "bold"))

## In the plot above, we can now clearly see that Trillium shows more stomatal
## conductance than Arisaema, since we are observing them individually.



## We know that water use efficiency is inversely proportional to Stomatal
## Conductance (WUE = Net Photosynthesis/ Stomatal Conductance). Therefore
## we can relate the plots for WUE and Stomtal Conductance and see the
## relationship clearly.


## Making a statistical model
GM_Cond_model <- lm(Cond ~ Treatment * sp, data = GM_Cond)

anova(GM_Cond_model)

## As we can see, there no values less than 0.05, so they are not significant.

## Trying the Stomatal Conductance model with emmeans
GM_Cond_em <- emmeans(GM_Cond_model, pairwise ~ sp)

## The p values are more than 0.05, so they aren't statistically significant.
