library(tidyverse)
library(readr)
behavioral_data <- read_csv("behavioral data.csv")

tidy_behave <- behavioral_data %>%
  pivot_longer('Pins':'Interrupted groom',names_to = "Behavorial_Observation", values_to = "Counts")

factor_behave <- tidy_behave %>% 
  mutate(
    Day = factor(Day, labels = c("1", "2","3")),
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
plot(behave_tukey_attacks)
behave_tukey_attacks