rm(list=ls()) # clearing old objects out of the environment
library(tidyverse)
library(ggplot2)
library(tidytuesdayR)
library(scales)
library(ggthemes)
library(here) # making this code accessible for those using other devices
setwd("/Users/mayaschroder/Desktop/Data Science/TidyTuesday")

tuesdata <- tidytuesdayR::tt_load('2021-07-27')
olympics <- tuesdata$olympics # loading in the data
theme_set(theme_classic())

# I want to look specifically at 3 countries of interest:
mydata <- olympics %>%
  filter(team == "Jamaica"|team == "India"|team == "Germany")

# comparing summer and winter games:
summer <- mydata %>%
  filter(season=="Summer")

# comparing total medals:
plot1 <- summer %>%
  filter(year >= 1962) %>%
  group_by(team, medal) %>%
  summarise(number = n()) %>%
  ungroup() %>%
  drop_na()
plot1$medal = factor(plot1$medal, levels = c("Gold", "Silver", "Bronze"))

total_medals = ggplot(data = plot1) + geom_bar(aes(x = team, y = number, fill = medal), 
                                stat = "identity") + 
  scale_fill_manual(name = "Medal", values = c("gold3", "gray85", "darkgoldenrod4")) + xlab("Country") + 
  ylab("Medals") + labs(title = "Medals Won in the Summer Olympics since 1964")
total_medals

# where do countries dominate?:
plot2 <- summer %>%
  filter(year >= 1962) %>%
  group_by(team, medal, sport) %>%
  summarise(number = n()) %>%
  ungroup() %>%
  drop_na()

plot2 = plot2 %>%
  filter(!(sport == "Taekwondo" | sport == "Modern Pentathlon" | sport == "Trampoline" | 
             sport == "Triathlon" | sport == "Weightlifting" | sport == "Wrestling" | sport == "Trampolining"))

medal_by_sport = ggplot(data = plot2) + geom_bar(aes(x = team, y = number, fill = medal), 
                                               stat = "identity") + 
  scale_fill_manual(name = "Medal", values = c("gold3", "gray85", "darkgoldenrod4")) + xlab("Country") + 
  ylab("Medals") + facet_wrap(~sport) + scale_x_discrete(labels = c("Germany" = "G",
                                                                    "India" = "I",
                                                                    "Jamaica" = "J"))
medal_by_sport

# gender disparities
plot3 <- summer %>%
  filter(year >= 1962) %>%
  group_by(team, sex) %>%
  summarise(number = n()) %>%
  ungroup()

gender_graph = ggplot(data = plot3) + geom_bar(aes(x = team, y = number, fill = sex), 
                                               stat = "identity") + 
  scale_fill_manual(name = "Gender", values = c("violet", "royalblue1")) + 
  xlab("Country") + 
  ylab("Number of Athletes")
gender_graph

citation("tidytuesdayR")