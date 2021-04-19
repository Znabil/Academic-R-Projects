### Statistical Data Analysis
## Homework 1.1
## Team members:
#  Xiao Luo
#  Widad El Abbassi
#  Jiayi Lin
#  Nabil Aziz

# Load packages
library(tidyverse)

# Load the dataset
eval <- get(load('/Users/xiaoluo/Desktop/Homework 1.1/eval.RData'))

## 1.Based on these data, is there evidence that professor rank is independent of gender?
ggplot(data = distinct(eval, prof_id, .keep_all = TRUE), mapping = aes(x = rank, y = ..prop.., group = gender, fill = gender)) +
  geom_bar(position = "dodge", width = .5) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Gender influence on professors' rank", x = "professors' rank", y = "percent")

## 2.There are many categorical variables that can have an influence on course evaluations. Focus on two of them.
ggplot(data = eval, mapping = aes(x = eval)) +
  stat_count(mapping = aes(y = ..prop..)) +
  facet_grid(ethnicity ~ cls_level) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Course evaluation", subtitle = "Ethnicity of professor and class level influence", 
       x = "course evaluation", y = "percent")

## 3.Explore the relationship between course evaluations and a beauty score for each professor. Is there evidence
##   of a different relationship depending on gender?
ggplot(data = eval, mapping = aes(x = bty_avg, y = eval, color = gender)) +
  geom_smooth() +
  labs(title = "Relationship between course evaluations and professor's beauty score",
       subtitle = "Gender influence", x = "beauty score", y = "course evaluation")

## 4. Explore the relationship between course evaluations and professors' rank
ggplot(data = eval, mapping = aes(x = cut_width(eval, 0.3, boundary = 0.5), y = ..prop.., group = rank, fill = rank)) +
  stat_count(position = "dodge", width = .6) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Relationship between course evaluations and professors' rank", x = "evaluation", 
         y = "percent")

ggplot(eval, aes(x = eval, group = rank, colour = rank)) +
  geom_line(stat = "count") +
  labs(title = "Relationship between course evaluations and professors' rank", x = "evaluation", 
       y = "count")

