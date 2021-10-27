library(tidyverse)
library(broom)

sleep_reaction <- read_csv("data/sleep.csv")

sleep_reaction$Days <- as.factor(sleep_reaction$Days)
sleep_reaction$Subject <- as.factor(sleep_reaction$Subject)
sleep_reaction$Group <- as.factor(sleep_reaction$Group)

summary_reaction <- sleep_reaction %>%
  group_by(Group,Days)%>%
  summarise(mean_reaction <-  mean(Reaction))

ggplot(sleep_reaction, aes(Days, Reaction, fill = Group))+
  geom_boxplot()

anova_sleep_reaction <- aov(Reaction ~ Days * Group, data = sleep_reaction)
tidy(anova_sleep_reaction)
summary(anova_sleep_reaction)

tukey_sleep_reaction <- TukeyHSD(anova_sleep_reaction, which = 'Days:Group', conf.level = 0.95)

plot(tukey_sleep_reaction)

tidy_tukey <- tidy(tukey_sleep_reaction)%>%
  filter(adj.p.value < 0.05)

tidy_tukey
