#Class_RelationalChallenge_2

library(tidyverse)

relational_1_behave <- read_csv("data/relational_1_behavioral.csv")
relational_1_hormone <- read_csv("data/relational_1_hormone.csv")

tidy_hormone <- relational_1_hormone%>%
  pivot_longer(`Baseline CORT`:`Swim DC ratio`, names_to = "Trial", values_to = "Levels" )%>%
  separate(Trial, into = c("Trial","Hormone"))%>%
  select(-Sex, -SES)

tidy_behave <- relational_1_behave%>%
  rename(Trial = Day)

join_rats <- tidy_hormone%>%
  left_join(tidy_behave)

join_rats%>%
  group_by(Sex, Trial, Hormone)%>%
  summarise(mean_attack = mean(Attacks, na.rm = TRUE),
            mean_levels = mean(Levels))

ggplot(join_rats, aes(x = Trial, y = Levels))+
  geom_boxplot(aes(fill = as.factor(Sex)), position = "dodge")+
  facet_wrap(~Hormone, scales = "free_y")

ggplot(join_rats, aes(x = Trial, y = Attacks))+
  geom_boxplot(aes(fill = as.factor(Sex)), position = "dodge")
