install.packages("broom")

library(tidyverse)
library(readr)
library(broom)
behavioral_data <- read_csv("data/relational_1_behavioral.csv")

tidy_behave <- behavioral_data %>%
  pivot_longer('Pins':'Interrupted groom',names_to = "Behavorial_Observation", values_to = "Counts")

factor_behave <- tidy_behave %>% 
  mutate(
    Day = factor(Day, labels = c("Baseline", "Swim")),
    Sex = factor(Sex, labels = c("Male", "Female")),
    SES = factor(SES, labels = c("Enriched","Control")),
    Behavorial_Observation = as.factor(Behavorial_Observation)
    )
factor_behave_attacks <- factor_behave %>%
  filter(Behavorial_Observation =='Attacks')

ggplot(factor_behave_attacks, mapping = aes(Day,Counts, fill = SES))+
  geom_boxplot()

behave_anova_attacks <- aov(Counts ~ SES * Day * Sex, data = factor_behave_attacks)
summary(behave_anova_attacks)

behave_tukey_attacks <- TukeyHSD(behave_anova_attacks, which = 'SES:Day',conf.level=0.95)
tidy_behave_tukey <- tidy(behave_tukey_attacks)
tidy_behave_tukey

factor_behave_attacks%>%
  group_by(Day, SES)%>%
  summarise(count_mean = mean(Counts, na.rm = TRUE),
            count_se = sd(Counts, na.rm = TRUE)/sqrt(n()))%>%
  ggplot(aes(x = Day, y = count_mean, fill = SES, group = SES)) +
  geom_errorbar(aes(ymin = count_mean - count_se, 
                    ymax = count_mean + count_se), position = "dodge", width = 0.7) +
  geom_bar(color = "black", stat = "identity", position = "dodge", width = 0.7) +
  theme_bw() +
  scale_fill_manual(values = c("white", "black")) +
  theme(axis.title = element_text(size = 14),
        legend.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12)) +
  annotate("text", x = 2.17, y = 13, label = "*", size = 8) +
  labs(x = "Day", 
       y = "Number of Attacks",
       title = "Post Stress, Enriched Environments Prevents \nIncrease in Aggressive Behavior",
       caption = "* indicates significance at p < 0.001")



?annotate
