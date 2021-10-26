library(tidyverse)
library(broom)
library(modelr)

sleep_data <- msleep

#function to produce r-squared on plot
lm_eqn <- function(df){
  m <- lm(sleep_total ~ sleep_rem, data = msleep);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}

?msleep

#build model
mod1 <- lm(sleep_total ~ sleep_rem, data = msleep)
mod1
#plot variables with model
ggplot(msleep, aes(sleep_total, sleep_rem))+
  geom_point()+
  geom_smooth(method="lm")+
  geom_text(x = 5, y = 5, label = lm_eqn(msleep_variables), parse = TRUE)

#add residuals to our data
msleep_residuals_sleeptotalvsrem <- msleep %>% 
  gather_residuals(mod1)

#determine how residuals change across X variable
ggplot(msleep_residuals_sleeptotalvsrem, aes(sleep_total, resid)) + 
  geom_point()

#determine distribution of residuals (normal?)
ggplot(msleep_residuals_sleeptotalvsrem, aes(resid))+
  geom_density()

summary(mod1)
tidy(mod1)
