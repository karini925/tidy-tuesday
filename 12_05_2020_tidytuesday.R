#tidy-tuesday
#date: May 12, 2020
#author: karin isaev

#this week we are looking at volcano eruption data

#[1] load packages first

library(tidyverse)
library(viridis)
library(cowplot)
library(patchwork)
library(readxl)
library(data.table)
library(hrbrthemes)
library(viridis)
library(ggridges)
library(plotly)

#[2] load some data

#first installed using:
#devtools::install_github("thebioengineer/tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2020-05-12')

volcano <- tuesdata$volcano
eruptions <- tuesdata$eruptions
events <- tuesdata$events
sulfur <- tuesdata$sulfur
tree_rings <- tuesdata$tree_rings

#[3] prepare data for plotting

#let's visualize which volcanoes and regions were most
#erupted in the last 100 years

vol_dat = as.data.table(inner_join(volcano, eruptions))
top_counts= as.data.table(table(vol_dat$country)) %>% arrange(desc(N))
top_counts = top_counts[1:20,]
colnames(top_counts)[1] = "Country"
vol_dat = as.data.table(filter(vol_dat, country %in% top_counts$Country))
summ = as.data.table(table(vol_dat$country, vol_dat$start_year)) %>%
                    filter(N > 0, V2 > 1600)
colnames(summ) = c("country", "")

#[4] plot data

# bubble plot
p = vol_dat %>%
  arrange(desc(vei)) %>%
  filter(!(is.na(vei)), start_year > 1600, country %in% top_counts$Country) %>%
  mutate(vei = as.numeric(vei)) %>%
  ggplot(aes(x=start_year, y=elevation, size = population_within_100_km, fill=country)) +
    geom_point(alpha=0.2, shape=21, color="black") +
    scale_fill_viridis(discrete=TRUE, guide=FALSE, option="A") +
    theme_ipsum() +
    theme(legend.position="bottom") +
    scale_size(name="Population within 100km of volcano") + xlab("Start year")+
    ylab("Elevation")

p1 = ggMarginal(p, type="histogram")

#[5] save plot
