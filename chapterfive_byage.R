library(moderndive)
library(gapminder)
library(tidyverse)
library(skimr)

options(warn=-1)
evals_ch5 <- evals %>%
  select(ID,score, bty_avg,age)
glimpse(evals_ch5)

#Choose a random sample and view 

evals_ch5 %>%
  sample_n(size=5)
evals_ch5 %>% 
  select(score, age)%>%
  skim()

evals_ch5 %>%
  get_correlation(formula = score ~age)

evals_ch5 %>%
  summarise(correlation=cor(score, age))

ggplot(evals_ch5, aes(x=age,y=score))+
  geom_point()+
  labs(x="Beauty Score",
       y="Rating score",
       tittle="Teaching score vs Beauty Score ")+
  geom_smooth(method = "lm", se=FALSE)

score_model2 <- lm(score~age, data=evals_ch5)

get_regression_table(score_model2)
