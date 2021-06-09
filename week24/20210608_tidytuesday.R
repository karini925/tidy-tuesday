#---------------------------------------
#PACKAGES
#---------------------------------------

library(tidyverse)
library(rtweet)
library(data.table)
library(ggpubr)
library(scales)
library(randomcoloR)
library(patchwork)
library(ggthemes) # Load

set.seed(123)

#---------------------------------------
#DATA
#---------------------------------------

tuesdata <- tidytuesdayR::tt_load('2021-06-08')
fishing <- tuesdata$fishing

#---------------------------------------
#CLEAN-UP
#---------------------------------------

#get top 10 fish species 
fish_keep <- as.data.table(fishing %>% count(species, sort = TRUE))$species[1:10]

#want to look at species across regions over the years 
fishing <-fishing %>% filter(species %in% fish_keep, values>0)

#get the total sum of values of fish collected of each specie at each lake per year
data_plot = fishing %>% 
    group_by(year, lake, species) %>% 
      summarize(sum = sum(values))

#---------------------------------------
#PLOT
#---------------------------------------

n <- 10
palette <- distinctColorPalette(n)

ggplot(data=data_plot, aes(x=year, y=sum, color=species))+
  geom_line()+
  theme_dark()+
  theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1, size=11),
  axis.text.y = element_text(size=9), 
  legend.box.background = element_rect(colour = "black"),
  legend.position="right", 
  plot.title = element_text(hjust = 0.5))+
  facet_wrap(~lake, ncol=1, scales="free_y")+
  labs(x = "Year", y="Grand total of observed",color="Species")+
  scale_x_continuous(breaks=seq(1865,2020,25))+
  scale_color_manual(values=palette)+
  ggtitle("Distribution of most commonly observed fish from 1865-2015")+
  plot_annotation(
    caption = "#TidyTuesday Week24 || Source: Great Lakes Fishery Comission || Visual: Karin Isaev"
  )

#---------------------------------------
#SAVE IMAGE
#---------------------------------------
ggsave("tidytuesday_week24_fish.png", width=8, height=12 , device = "png")

## post tweet with media attachment
post_tweet("a tweet with media attachment", media = "tidytuesday_week24_fish.png")
