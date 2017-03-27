# This script will import csv files that contain filenames and the number of
# openings (defined as a gap between two or more neighboring cells) and the
# number of cells it then plots them in a pretty way

#install needed libraries
library(tidyverse)
library(RColorBrewer)

# input csv files exported from google sheet "lumens galore"
data_live <- read_csv("~/Documents/R/lumens_counting/lumens galore - export MDCK - live 170323.csv")
data_fixed <- read_csv("~/Documents/R/lumens_counting/lumens galore - export MDCK - fixed 170323.csv")

# tibble that only has the cells and openings for live and fixed data sets
co_live <- select(data_live, cells, openings)
co_live <- na.omit(co_live) #omits NA values
co_live$openings = as.character(co_live$openings)
co_live$cells = as.factor(co_live$cells)

co_fixed <- select(data_fixed, cells, openings)
co_fixed <- na.omit(co_fixed) #omits NA values
co_fixed$openings = as.character(co_fixed$openings)
co_fixed$cells = as.factor(co_fixed$cells)

# plots a histogram of cell counts
#ggplot(data = co_live) +
ggplot(data = co_fixed) +
  geom_bar(mapping = aes(x = cells)) +
  labs(x = "number of cells per cyst") +
  #labs(title = "Histogram of cells per cyst - live")
  labs(title = "Histogram of cells per cyst - fixed")
#ggsave("livecell_hist.eps", plot = last_plot())
ggsave("fixedcell_hist.eps", plot = last_plot())

# plots stacked bar plot of proportions with different openings
#ggplot(data = co_live) +
ggplot(data = co_fixed) +
  geom_bar(mapping = aes(x = cells, fill = openings), position = "fill") +
  scale_fill_brewer(palette = 'Paired') +
  labs(x = "number of cells per cyst") +
  labs(y = "Proportion") +
  #labs(title = "Proportion of cysts with number of openings - live")
  labs(title = "Proportion of cysts with number of openings - fixed") 
#ggsave("livecell_proportion.eps", plot = last_plot())  
ggsave("fixedcell_proportion.eps", plot = last_plot())


#make tibble with cells, openings, Drug
cod_fixed <-co_live <- select(data_fixed, cells, openings, Drug)
cod_fixed <- na.omit(cod_fixed) #omits NA values
cod_fixed$openings = as.character(co_fixed$openings)
cod_fixed$cells = as.factor(co_fixed$cells)

# plotting based on drug treatment (or no treatment)
#re-orders class Drug so that it makes some sense in a 2x4 subplot
cod_fixed$Drug <- factor(cod_fixed$Drug, levels = c("none", "DMSO", "CD", "CT", "ctl", "Y2", "ML7", "Y2_ML7"))

# histogram of different drug treatments
ggplot(data = cod_fixed) +
  geom_bar(mapping = aes(x = cells)) +
  facet_wrap(~ Drug, nrow = 2) +
  labs(x = "number of cells per cyst")
ggsave("fixedcell_hist_drugs.eps", plot = last_plot())    

# proportions of openings in different drug treatments
ggplot(data = cod_fixed) +
  geom_bar(mapping = aes(x = cells, fill = openings), position = "fill")+
  scale_fill_brewer(palette = 'Paired') +
  facet_wrap(~ Drug, nrow = 2, scales = "free_x") +
  labs(x = "number of cells per cyst")
ggsave("fixedcell_proportion_drugs.eps", plot = last_plot())  

# this will make a tibble with columns: cells - openings - count for that combination
#cell_count <- count(cells_openings, cells, openings)
