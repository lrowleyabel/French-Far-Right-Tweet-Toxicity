########################
#                      #
#  BIVARIATE ANALYSIS  #
#                      #
########################


# LAURENCE ROWLEY-ABEL, UNIVERSITY OF EDINBURGH
# UPDATED: 28/03/23

# DESCRIPTION: This file looks at the distribution of 'toxicity' in the Tweets across party, level of highest previous office and incumbent status.

library(dplyr)
library(ggplot2)
library(ggthemes)
library(patchwork)


setwd("C:/Users/lrowley/OneDrive - University of Edinburgh/French Election Project/Discourse Paper/Analysis")
rm(list = ls())

# Load sample Tweet dataset with results from Perspective API
load("../Data/sample_analysis_dataset.Rda")
df<- sample_tweet_df

# Look at distribution of toxicity scores
p0<- ggplot(df)+
  annotate("segment", y = seq(0.5,1+0.5,0.25), yend = seq(0.5,1+0.5,0.25), x = 0, xend = 0.75, color = "white")+
  annotate("segment", y = 0.5, yend = 1+0.5, x = seq(0, 0.75, 0.05),xend = seq(0, 0.75, 0.05), color = "white")+
  geom_boxplot(aes(x = TOXICITY), width = 0.25, outlier.size = 0.5)+
  scale_y_discrete()+
  theme_economist()+
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        plot.title.position = "plot",
        axis.title.x = element_text(margin = margin(10, 0, 0, 0)),
        plot.title = element_text(margin = margin(10,0,10,0)),
        panel.grid.major.y = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(hjust = 1),
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0, size = 9),
        plot.margin = margin(10,20,10,20))+
  labs(title = "Distribution of Tweet Toxicity by Party", subtitle = "N = 25,000", x = "Toxicity", caption = "Tweet data from Twitter API. Toxicity data from Perspective API.")

p0

# Look at distribution across parties
df$var1<- df$party_list

means_summary<- df%>%
  group_by(var1)%>%
  summarise(mean_tox = mean(TOXICITY, na.rm = T))

p1<- ggplot(df)+
  annotate("segment", y = seq(0.5,nrow(means_summary)+0.5,0.25), yend = seq(0.5,nrow(means_summary)+0.5,0.25), x = 0, xend = 0.75, color = "white")+
  annotate("segment", y = 0.5, yend = nrow(means_summary)+0.5, x = seq(0, 0.75, 0.05),xend = seq(0, 0.75, 0.05), color = "white")+
  geom_boxplot(aes(x = TOXICITY, y = var1, group = var1), width = 0.25, outlier.size = 0.5)+
  scale_y_discrete()+
  theme_economist()+
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        plot.title.position = "plot",
        axis.title.x = element_text(margin = margin(10, 0, 0, 0)),
        plot.title = element_text(margin = margin(10,0,10,0)),
        panel.grid.major.y = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(hjust = 1),
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0, size = 9),
        plot.margin = margin(10,20,10,20))+
        labs(title = "Figure 1: Distribution of Tweet Toxicity by Party", subtitle = "N = 25,000", x = "Toxicity", caption = "Tweet data from Twitter API. Toxicity data from Perspective API.")

p1

# Look at distribution by level of previous highest office held
df$var1<- df$highest_office_level

means_summary<- df%>%
  group_by(var1)%>%
  summarise(mean_tox = mean(TOXICITY, na.rm = T))

p2<- ggplot(df)+
  annotate("segment", y = seq(0.5,nrow(means_summary)+0.5,0.25), yend = seq(0.5,nrow(means_summary)+0.5,0.25), x = 0, xend = 0.75, color = "white")+
  annotate("segment", y = 0.5, yend = nrow(means_summary)+0.5, x = seq(0, 0.75, 0.05),xend = seq(0, 0.75, 0.05), color = "white")+
  geom_boxplot(aes(x = TOXICITY, y = var1, group = var1), width = 0.25, outlier.size = 0.5)+
  scale_y_discrete()+
  theme_economist()+
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        plot.title.position = "plot",
        axis.title.x = element_text(margin = margin(10, 0, 0, 0)),
        plot.title = element_text(margin = margin(10,0,10,0)),
        panel.grid.major.y = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(hjust = 1),
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0, size = 9),
        plot.margin = margin(10,20,10,20))+
  labs(title = "Figure 2: Distribution of Tweet Toxicity by Highest Office Held", subtitle = "N = 25,000", x = "Toxicity", caption = "Tweet data from Twitter API. Toxicity data from Perspective API.")

p2

# Look at distribution by incumbency
df<- df%>%
  filter(!is.na(incumbent))

df$var1<- df$incumbent

means_summary<- df%>%
  group_by(var1)%>%
  summarise(mean_tox = mean(TOXICITY, na.rm = T))

p3<- ggplot(df)+
  annotate("segment", y = seq(0.5,nrow(means_summary)+0.5,0.25), yend = seq(0.5,nrow(means_summary)+0.5,0.25), x = 0, xend = 0.75, color = "white")+
  annotate("segment", y = 0.5, yend = nrow(means_summary)+0.5, x = seq(0, 0.75, 0.05),xend = seq(0, 0.75, 0.05), color = "white")+
  geom_boxplot(aes(x = TOXICITY, y = var1, group = var1), width = 0.25, outlier.size = 0.5)+
  scale_y_discrete()+
  theme_economist()+
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        plot.title.position = "plot",
        axis.title.x = element_text(margin = margin(10, 0, 0, 0)),
        plot.title = element_text(margin = margin(10,0,10,0)),
        panel.grid.major.y = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(hjust = 1),
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0, size = 9),
        plot.margin = margin(10,20,10,20))+
  labs(title = "Distribution of Tweet Toxicity by Incumbent Status",subtitle = "N = 25,000", x = "Toxicity", caption = "Tweet data from Twitter API. Toxicity data from Perspective API.")

p3

# Look at distribution by highest office level within each party

df$var1<- df$highest_office_level

means_summary<- df%>%
  group_by(var1)%>%
  summarise(mean_tox = mean(TOXICITY, na.rm = T))

p4<- ggplot(df)+
  annotate("segment", y = seq(0.5,nrow(means_summary)+0.5,0.25), yend = seq(0.5,nrow(means_summary)+0.5,0.25), x = 0, xend = 0.75, color = "white")+
  annotate("segment", y = 0.5, yend = nrow(means_summary)+0.5, x = seq(0, 0.75, 0.05),xend = seq(0, 0.75, 0.05), color = "white")+
  geom_boxplot(aes(x = TOXICITY, y = var1, group = var1), width = 0.25, outlier.size = 0.5)+
  facet_wrap(~party_list, ncol = 1)+
  party_fill_scale+
  scale_y_discrete()+
  theme_economist()+
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        plot.title.position = "plot",
        axis.title.x = element_text(margin = margin(10, 0, 0, 0)),
        plot.title = element_text(margin = margin(10,0,10,0)),
        panel.grid.major.y = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(hjust = 1),
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0, size = 9),
        plot.margin = margin(10,20,10,20))+
  labs(title = "Figure 3: Distribution of Tweet Toxicity by Party and\nHighest Office Held", subtitle = "N = 25,000", x = "Toxicity", caption = "Tweet data from Twitter API. Toxicity data from Perspective API.")

# Align the first three plots
plots_aligned<- align_patches(p1, p2, p3)

# Save the four plots
ggsave("Plots/Toxicity by Party.png", plot = plots_aligned[[1]], units = "in", width = 8, height = 5, dpi = 1000)
ggsave("Plots/Toxicity by Highest Office Level.png", plot = plots_aligned[[2]], units = "in", width = 8, height = 5, dpi = 1000)
ggsave("Plots/Toxicity by Incumbent Status.png", plot = plots_aligned[[3]], units = "in", width = 8, height = 5, dpi = 1000)
ggsave("Plots/Toxicity by Party and Highest Office Held.png", plot = p4, units = "in", width = 8, height = 12, dpi = 1000)
