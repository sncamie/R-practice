library(fivethirtyeight)
library(nycflights13)

library(dplyr)
#library(skimr)

library(knitr)

library(ggplot2)
library(tidyr)
library(readr)


dem_score <-read_csv("https://moderndive.com/data/dem_score.csv")

dem_score

View(drinks)


drinks_smaller <- drinks %>%
  filter(country %in% c("USA", "China", "Italy", "Saudi Arabia", "Swaziland","South Africa"))%>%
  select(-total_litres_of_pure_alcohol)%>%
  rename(beer=beer_servings, spirit=spirit_servings, wine=wine_servings)
View(drinks_smaller)


dat_long <- drinks_smaller %>% 
  gather("Stat", "Value", -country)

ggplot(data=dat_long, mapping = aes(x=country, y=Value, fill=Stat))+
         geom_bar(stat="identity", position=position_dodge())


drinks_smaller_tidy <-drinks_smaller %>%
  pivot_longer(names_to = "type",
               values_to= "servings",
               cols=-country)
View(drinks_smaller_tidy)

ggplot(drinks_smaller_tidy,aes(x=country, y=servings, fill=type))+
  geom_bar(position = "dodge", stat = "identity")


airlines_safety_smaller<- airline_safety %>%
  select(airline, starts_with("fatalities"))

airlines_safety_smaller


arlines_safety_tidy <- airlines_safety_smaller %>%
  pivot_longer(names_to = "Years",
               values_to="Fatalities",
               cols=-airline)
arlines_safety_tidy

ggplot(arlines_safety_tidy,aes(x=airline, y=Fatalities, fill=Years))+
  geom_bar(position = "dodge", stat = "identity")


guat_dem <- dem_score %>%
  filter(country=="Guatemala")
guat_dem


guat_dem_tidy <- guat_dem %>%
  pivot_longer(names_to = "year",
               values_to ="democracy_score",
               cols= -country,
               names_transform = list(year = as.integer))
guat_dem_tidy


ggplot(guat_dem_tidy, aes(x=year, y=democracy_score))+
  geom_line()+
  labs(x="Year", y="Democracy Score")
