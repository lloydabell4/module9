#Class_RelationalChallenge_1

library(tidyverse)

relational_1_HR <- read_csv("data/relational_1_HR.csv")
relational_1_income <- read_csv("data/relational_1_income.csv")

relational_HR_income_join <- relational_1_HR%>%
  separate(`Blood Pressure`, into = c("Systolic","Diastolic"))%>%
  rename(Subject_ID = Patient_ID)%>%
  left_join(relational_1_income, by = "Subject_ID")%>%
  separate(Trial, into = c("Trial","Measurement"))

relational_means <- relational_HR_income_join%>%
  filter(SEX %in% c("F", "M"))%>%
  group_by(Trial, SEX, Measurement)%>%
  summarise(mean_level = mean(Levels, na.rm=TRUE))

ggplot(relational_means, aes(x = Trial, y = mean_level))+
  geom_bar(aes(fill = SEX), stat = "identity", position = "dodge")+
  facet_wrap(~Measurement)+
  scale_x_discrete(limits = c("baseline", "pre", "post"))
