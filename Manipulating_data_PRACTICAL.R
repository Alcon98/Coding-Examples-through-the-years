###This was a practical component I completed in a working with R workshop at The Wildlife Society Annual Conference March 2021
### The workshop was led by Ms. Erica Newton, an analytical biologist from the MNRF
### The goal of this was to practice using tidyverse/dplyr functions to help clean up code and make analysis quicker and more straightforward.

# Use tidyverse/dplyr functions to answer the following questions.
library(tidyverse)

# How many snow courses are in each district? 
snowdata <- read.csv(file.choose())
district_summary <- snowdata %>% 
  select(distname, coursecode) %>% 
  distinct() %>% 
  group_by(distname) %>% 
  tally()
view(district_summary)

##OR

snowdata %>% 
  group_by(distname) %>% 
  summarise(n.courses = n_distinct(coursecode))

##OR
snowdata %>% 
  select(distname, coursecode) %>% 
  distinct() %>% 
  count(distname)

# What is the maximum sdi recorded per course over all years in Fort Frances and Wawa district courses?

maxdf <- snowdata %>% 
  filter(distname %in% c("Fort Frances", "Wawa")) %>% 
  group_by(distname, coursecode) %>% 
  summarize(max.sdi = max(sdi, na.rm = TRUE))

maxdf

# Some courses have not yet taken a GPS location of their snow stations. Produce a table showing which courses do not yet have an accurate ("GPS") location recorded, and which district contact to email in the coming year to remind them to get a GPS location! An "inaccurate" location is a station that has *any value EXCEPT* "GPS" in the Accuracy column.

locations <- read_csv(file.choose())

GIS <- locations %>% 
  select(DISTNAME, Accuracy) %>% 
  group_by(DISTNAME) %>% 
  filter(Accuracy == c('BBA Square (+/5km)'))
view(GIS)

contact <- read_csv(file.choose())
view(contact)

left_join(GIS, contact, by = c('DISTNAME' = 'distname'))


# Produce a ggplot2 graph comparing maximum sdi each year in Sudbury district courses without making a new data frame. Each course's data should be shown on a separate panel.
view(snowdata)
snowdata %>% 
  filter(distname == 'Sudbury') %>% 
  group_by(coursecode, minyear) %>% 
  summarize(max.sdi = max(sdi, na.rm = T)) %>% 
  ggplot(aes(minyear, max.sdi, color = coursecode)) + 
  geom_line(size = 1) +
  geom_point() + 
  facet_wrap(~coursecode) +
  labs(x = 'Year', y = 'Maximum SDI') +
  theme_bw()
