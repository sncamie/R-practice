library(tidyverse)
library(moderndive)
library(skimr)
library(ISLR)

evals_ch6 <-evals%>%
  select(ID,score, age, gender)

glimpse(evals_ch6)

evals_ch6 %>% sample_n(size = 5)
  

evals_ch6 %>% select(score, age, gender) %>% skim()

ggplot(evals_ch6, aes(x=age, y=score, color=gender))+
  geom_point()+
  labs(x="Age", y="score", color="gender")+
  geom_smooth(method = "lm",se=FALSE)

#fit regression model

score_model_interaction <- lm(score~age*gender, data = evals_ch6)

get_regression_table(score_model_interaction)


#plotting parallel slopes
ggplot(evals_ch6, aes(x=age, y=score, color=gender))+
  geom_point()+
  labs(x="Age", y="score", color="gender")+
  geom_parallel_slopes(se=FALSE)


regression_points <- get_regression_points(score_model_interaction)

regression_points

#ISLR package 

credit_ch6 <- Credit %>%
  select(ID, debt=Balance, credit_limit= Limit,
         income=Income, credit_rating= Rating, age=Age)
glimpse(credit_ch6)

credit_ch6 %>% sample_n(size = 5)

credit_ch6 %>% select(debt, credit_limit, income) %>% skim()


credit_ch6 %>% get_correlation(debt~credit_limit)

credit_ch6 %>% get_correlation(debt~income)

#we can get the same values via correlation matrix 

credit_ch6 %>% 
  select(debt, credit_limit, income) %>%
  cor()

#lets visualize 

ggplot(credit_ch6, aes(x = credit_limit, y = debt)) +
  geom_point() +
  labs(x = "Credit limit (in $)", y = "Credit card debt (in $)",
       title = "Debt and credit limit") +
  geom_smooth(method = "lm", se = FALSE)
ggplot(credit_ch6, aes(x = income, y = debt)) +
  geom_point() +
  labs(x = "Income (in $1000)", y = "Credit card debt (in $)",
       title = "Debt and income") +
  geom_smooth(method = "lm", se = FALSE)

# Fit regression model:
debt_model <- lm(debt ~ credit_limit + income, data = credit_ch6)
# Get regression table:
get_regression_table(debt_model)
