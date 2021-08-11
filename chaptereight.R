library(infer)
library(tidyverse)
library(moderndive)

pennies_sample

ggplot(pennies_sample, aes(x=year))+
  geom_histogram(binwidth = 10, color="white")

pennies_sample %>%
  summarise(mean_year=mean(year))

#resampling with replacement 

pennies_resample <- tibble(
  year = c(1976, 1962, 1976, 1983, 2017, 2015, 2015, 1962, 2016, 1976,
           2006, 1997, 1988, 2015, 2015, 1988, 2016, 1978, 1979, 1997,1974, 2013, 1978, 2015, 2008, 1982, 1986, 1979, 1981, 2004,
           2000, 1995, 1999, 2006, 1979, 2015, 1979, 1998, 1981, 2015,
           2000, 1999, 1988, 2017, 1992, 1997, 1990, 1988, 2006, 2000)
)
ggplot(pennies_resample, aes(x = year)) +
  geom_histogram(binwidth = 10, color = "white") +
  labs(title = "Resample of 50 pennies")
ggplot(pennies_sample, aes(x = year)) +
  geom_histogram(binwidth = 10, color = "white") +
  labs(title = "Original sample of 50 pennies")


#what is the sample mean of our resampled pennies when the original was 1995

pennies_resample %>%
  summarize(mean_year = mean(year))

pennies_resamples

resampled_means <- pennies_resamples %>%
  group_by(name)%>%
  summarise(mean_year=mean(year))
resampled_means


ggplot(resampled_means, aes(x = mean_year)) +
  geom_histogram(binwidth = 1, color = "white", boundary = 1990) +
  labs(x = "Sampled mean year")

#virtual resampling once 

virtual_resample <- pennies_sample %>%
  rep_sample_n(size = 50, replace = TRUE)

virtual_resample %>%
  summarise(virtual_mean=mean(year))

#repeat the experiment 35 times 
virtual_resamples <- pennies_sample %>%
  rep_sample_n(size = 50, replace = TRUE, reps = 35)

virtual_resamples

virtual_resampled_means <-virtual_resamples %>%
  group_by(replicate)%>%
  summarise(mean_year=mean(year))
virtual_resampled_means 

ggplot(virtual_resampled_means, aes(x = mean_year)) +
  geom_histogram(binwidth = 1, color = "white", boundary = 1990) +
  labs(x = "Resample mean year")

# Repeat resampling 1000 times
virtual_resamples <- pennies_sample %>%
  rep_sample_n(size = 50, replace = TRUE, reps = 1000)
# Compute 1000 sample means
virtual_resampled_means <- virtual_resamples %>%
  group_by(replicate) %>%
  summarize(mean_year = mean(year))
virtual_resampled_means

ggplot(virtual_resampled_means, aes(x = mean_year)) +
  geom_histogram(binwidth = 1, color = "white", boundary = 1990) +
  labs(x = "sample mean")

virtual_resampled_means %>%
  summarize(mean_of_means = mean(mean_year))

virtual_resampled_means %>%
  summarize(SE = sd(mean_year))

#using the infer package to calculate statistical summaries 

pennies_sample %>%
  specify(response = year) %>%
  calculate(stat = "mean")

pennies_sample %>%
  specify(formula = year ~ NULL)


#bootstrapping with infer 

pennies_sample %>%
  specify(response = year) %>%
  generate(reps = 1000, type = "bootstrap")

#bootstrapping and calculating means 
bootstrap_distribution <- pennies_sample %>%
  specify(response = year) %>%
  generate(reps = 1000) %>%
  calculate(stat = "mean")
bootstrap_distribution

visualize(bootstrap_distribution)

#calculating confidence interval with infer() percentile method

percentile_ci <- bootstrap_distribution %>%
  get_confidence_interval(level = 0.95, type = "percentile")
percentile_ci
visualize(bootstrap_distribution) +
  shade_confidence_interval(endpoints = percentile_ci)
visualize(bootstrap_distribution) +
  shade_ci(endpoints = percentile_ci, color = "hotpink", fill = "khaki")

#standard error with infer 

x_bar <- 1995.44

standard_error_ci <- bootstrap_distribution %>%
  get_confidence_interval(type = "se", point_estimate = x_bar)
standard_error_ci

visualize(bootstrap_distribution) +
  shade_confidence_interval(endpoints = standard_error_ci)

#revisiting the bowl experiment 

bowl_sample_1 %>%
  specify(response = color, success = "red")

bowl_sample_1 %>%
  specify(response = color, success = "red") %>%
  generate(reps = 1000, type = "bootstrap")

sample_1_bootstrap <- bowl_sample_1 %>%
  specify(response = color, success = "red") %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "prop")
sample_1_bootstrap

visualise(sample_1_bootstrap)

percentile_ci_1 <- sample_1_bootstrap %>%
  get_confidence_interval(level = 0.95, type = "percentile")
percentile_ci_1

sample_1_bootstrap %>%
  visualize(bins = 15) +
  shade_confidence_interval(endpoints = percentile_ci_1) +
  geom_vline(xintercept = 0.375, linetype = "dashed")

#two sample inference with mybusters data 

mythbusters_yawn %>%
  group_by(group, yawn) %>%
  summarize(count = n())

#perform bootstrap resampling with replacement

first_six_rows <- head(mythbusters_yawn)
first_six_rows

mythbusters_yawn %>%
  specify(formula = yawn ~ group, success = "yes") %>%
  generate(reps = 1000, type = "bootstrap")%>%
  calculate(stat = "diff in props")

bootstrap_distribution_yawning <- mythbusters_yawn %>%
  specify(formula = yawn ~ group, success = "yes") %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in props", order = c("seed", "control"))
bootstrap_distribution_yawning


visualize(bootstrap_distribution_yawning) +
  geom_vline(xintercept = 0)
bootstrap_distribution_yawning %>%
  get_confidence_interval(type = "percentile", level = 0.95)
