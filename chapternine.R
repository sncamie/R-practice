library(tidyverse)
library(infer)
library(moderndive)
library(nycflights13)
library(ggplot2movies)

promotions %>%
  sample_n(size = 6) %>%
  arrange(id)

ggplot(promotions, aes(x = gender, fill = decision)) +
  geom_bar() +
  labs(x = "Gender of name on résumé")

#count the numbers 

promotions %>% 
  group_by(gender, decision) %>%
  tally()

#shuffling 
promotions_shuffled

ggplot(promotions_shuffled,
       aes(x = gender, fill = decision)) +
  geom_bar() +
  labs(x = "Gender of résumé name")

#calculate the statistic using infer package 

promotions_generate <-promotions %>%
  specify(formula = decision~gender, success = "promoted")%>%
  hypothesize(null = "independence")%>%
  generate(reps = 1000, type = "permute")
nrow(promotions_generate)


#summary stats 

null_distribution <- promotions %>%
  specify(formula = decision ~ gender, success = "promoted") %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in props", order = c("male", "female"))
null_distribution


obs_diff_prop <- promotions %>%
  specify(decision ~ gender, success = "promoted") %>%
  calculate(stat = "diff in props", order = c("male", "female"))

obs_diff_prop
visualize(null_distribution, bins = 10)+
  shade_p_value(obs_stat = obs_diff_prop, direction = "right")

#movies 

movies_sample


ggplot(data = movies_sample, aes(x = genre, y = rating)) +
  geom_boxplot() +
  labs(y = "IMDb rating")

movies_sample %>%
  group_by(genre) %>%
  summarize(n = n(), mean_rating = mean(rating), std_dev = sd(rating))

#test with sampling 

movies_sample %>%
  specify(formula = rating ~ genre) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  View()

#calculate the summary statistics 
null_distribution_movies <- movies_sample %>%
  specify(formula = rating ~ genre) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in means", order = c("Action", "Romance"))
null_distribution_movies


obs_diff_means <- movies_sample %>%
  specify(formula = rating ~ genre) %>%
  calculate(stat = "diff in means", order = c("Action", "Romance"))
obs_diff_means


visualize(null_distribution_movies, bins = 10) +
  shade_p_value(obs_stat = obs_diff_means, direction = "both")


null_distribution_movies %>%
  get_p_value(obs_stat = obs_diff_means, direction = "both")


# Construct null distribution of t:
null_distribution_movies_t <- movies_sample %>%
  specify(formula = rating ~ genre) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  # Notice we switched stat from "diff in means" to "t"
  calculate(stat = "t", order = c("Action", "Romance"))
visualize(null_distribution_movies_t, bins = 10)


visualize(null_distribution_movies_t, bins = 10, method = "both")
