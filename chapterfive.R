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
  select(score, bty_avg)%>%
  skim()

evals_ch5 %>%
  get_correlation(formula = score ~bty_avg)

evals_ch5 %>%
  summarise(correlation=cor(score, bty_avg))

ggplot(evals_ch5, aes(x=bty_avg,y=score))+
  geom_point()+
  labs(x="Beauty Score",
       y="Rating score",
       tittle="Teaching score vs Beauty Score ")+
  geom_smooth(method = "lm", se=FALSE)

score_model <- lm(score~bty_avg, data=evals_ch5)

get_regression_table(score_model)

#life expectancy

gapminder2007 <- gapminder %>%
  filter(year==2007)%>%
  select(country, lifeExp,continent,gdpPercap)

glimpse(gapminder2007)

gapminder2007 %>% sample_n(size = 5)


gapminder2007 %>% select(lifeExp, continent)%>% skim()


ggplot(gapminder2007, aes(x=lifeExp))+
  geom_histogram(binwidth = 5, color="white")+
  labs(x="Life expectancy",
       y="number of countries")+
  facet_wrap(~continent, nrow = 2)


ggplot(gapminder2007, aes(x=continent, y=lifeExp))+
  geom_boxplot()+
  labs(x="Life expectancy",
       y="number of countries")
life_Exp_by_Continent<-gapminder2007 %>%
  group_by(continent) %>%
  summarise(median=median(lifeExp),
            mean=mean(lifeExp))

life_Exp_by_Continent

life_Exp_model <-lm(lifeExp~continent, data=gapminder2007)

get_regression_table(life_Exp_model)

regression_points <- get_regression_points(life_Exp_model, ID="country")

regression_points
