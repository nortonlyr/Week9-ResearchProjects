library(ggplot2) 
library(readr) 
library(tidyverse,warn.conflicts = FALSE)
library(RColorBrewer)
options(warn=-1,message=-1)
library(dplyr)
library(psych)
library(ggthemes)
setwd('/Users/nli/dev/Week9-ResearchProjects')


#This lego dataset came from 8 different csv files, and it has internal realtionship

colors<-read.csv("./lego-database/colors.csv")
sets<-read.csv("./lego-database/sets.csv")
parts<-read.csv("./lego-database/parts.csv")
inventory_sets<-read.csv("./lego-database/inventory_sets.csv")
inventories<-read.csv("./lego-database/inventories.csv")
inventory_parts<-read.csv("./lego-database/inventory_parts.csv")
themes<-read.csv("./lego-database/themes.csv")
part_categories<-read.csv("./lego-database/part_categories.csv")

#read all the inserted csvfiles
View(colors)
view(sets)
view(parts)
view(inventory_sets)
view(inventories)
view(inventory_parts)
view(themes)
view(part_categories)

#Summary of all csv files
summary(colors)
summary(sets)
summary(parts)
summary(inventory_sets)
summary(inventories)
summary(inventory_parts)
summary(themes)
summary(part_categories)

#type of all csv files
typeof(colors)
typeof(sets)
typeof(parts)
typeof(inventory_sets)
typeof(inventories)
typeof(inventory_parts)
typeof(themes)
typeof(part_categories)


sum(is.na(colors))
sum(is.na(sets))
sum(is.na(parts))
sum(is.na(inventory_sets))
sum(is.na(inventories))
sum(is.na(inventory_parts))
sum(is.na(themes))
sum(is.na(part_categories))
#Since the themes has many missing on parent_id, we might just skip this column,
#We are not delete any data now

head(colors)
num_colors <- length(unique(colors$name))
paste("Number of Unique colors in Lego sets =",num_colors) 

colors<-colors%>%mutate(rgb=paste0("#", str_trim(rgb)))
fav_color <- colors$rgb
names(fav_color) <- fav_color

#simple bar plot
color_counts <- colors %>% 
                group_by(is_trans) %>% 
                summarize(number_of_color = n())
options(repr.plot.width=6, repr.plot.height=4)
ggplot(color_counts, aes(x=is_trans, 
                         y=number_of_color, 
                         fill=as.factor(is_trans))) +
              geom_bar(stat='identity') + 
              theme_economist() +
              theme(legend.position='none')+
              geom_label(aes(label=number_of_color), size=5)




