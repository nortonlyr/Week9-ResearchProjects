library(ggplot2) 
library(readr) 
library(tidyverse,warn.conflicts = FALSE)
library(RColorBrewer)
options(warn=-1,message=-1)
library(dplyr)
library(psych)
library(ggthemes)
library(mongolite)
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
options(repr.plot.width=6, repr.plot.height=3)
ggplot(color_counts, aes(x=is_trans, 
                         y=number_of_color, 
                         fill=as.factor(is_trans))) +
              geom_bar(stat='identity') + 
              theme_economist() +
              theme(legend.position='none')+
              geom_label(aes(label=number_of_color), size=5)


#Average parts per year
options(repr.plot.width=6, repr.plot.height=3)
#select the parameter
avg_parts_per_year <- sets %>% 
              select(year, num_parts) %>% 
              group_by(year) %>% 
              summarize(avg_num_parts=mean(num_parts))
str(avg_parts_per_year)
avg_parts_per_year$year <- as.factor(avg_parts_per_year$year)

avg_parts_per_year %>% ggplot(aes(x=year, 
                                  y=avg_num_parts, 
                                  group=1)) + 
                    geom_line(size=0.5) + 
                    geom_point(color='purple', size=1.5) +
                    geom_smooth(aes(group=1), color = 'gold') +
                    theme_economist() +
                    theme(axis.text.x = 
                          element_text(angle=30, face='bold', hjust=0), 
                              legend.position = 'none') +
                    scale_x_discrete(breaks = 
                        avg_parts_per_year$year[seq(1,
                              length(avg_parts_per_year$year), by = 5)]) +
                    geom_label(data=subset(avg_parts_per_year, avg_num_parts == max(avg_num_parts)),
                        aes(label=avg_num_parts, y = avg_num_parts), vjust = 1.5 ) +
                    ggtitle('Average parts number per year')


#Number sets per year
options(repr.plot.width=6, repr.plot.height=3)
num_sets_per_year <- sets %>%
                      select(year, set_num) %>%
                      group_by(year) %>%
                      summarize(num_sets = n())
#str(num_sets_per_year)
num_sets_per_year$year <- as.factor(num_sets_per_year$year)

num_sets_per_year %>% ggplot(aes(x=year, 
                                 y=num_sets, 
                                 group=1)) + 
                      #geom_bar(stat='identity')
                      geom_line(size=0.5) + 
                      geom_point(color='darkgreen', size=1.5) +
                      geom_smooth(aes(group=1), color = 'yellow') +
                      theme_economist() +
                      theme(axis.text.x = 
                          element_text(angle=0, face='bold', hjust=0), 
                              legend.position = 'none') +   # x-axis scale
                      scale_x_discrete(breaks = 
                          num_sets_per_year$year[seq(1,
                              length(num_sets_per_year$year), by = 5)]) + # x-interval
                      ggtitle('Number of sets number per year')


#Top 10 sets with most parts:
#select the parameter
name_w_most_parts <- sets %>% 
                      select(name, year, num_parts) %>% 
                      arrange(desc(num_parts))
top_10_names <- name_w_most_parts[1:10,]

top_10_names %>% ggplot(aes(x=reorder(name, -num_parts), 
                            y=num_parts, 
                            fill=name)) +
                      geom_histogram(stat='identity') + 
                      theme_fivethirtyeight() +
                      theme(axis.text.x = 
                          element_text(angle=50, face='bold', hjust=1),
                          legend.position = 'none') +
                      geom_label(aes(label=num_parts)) +
                      ggtitle('Top 10 sets with most parts')

