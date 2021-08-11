library(nycflights13)

library(dplyr)
#library(skimr)

library(knitr)

library(ggplot2)

View(flights)
View(planes)
View(airports)
View(airlines)


new_airlines_data <- flights %>%
  inner_join(planes, by="tailnum")

View(new_airlines_data)

select_few <- new_airlines_data %>%
  select(carrier, distance,seats)

View(select_few)


grouped_carriers <- select_few %>%
  group_by(carrier)

View(grouped_carriers)

select_few <- select_few %>%
  mutate(seat_miles=distance * seats)
View(select_few)

final_answer <- select_few %>%
  group_by(carrier) %>%
  select(carrier, seat_miles)
View(final_answer)


ggplot(data=final_answer, mapping=aes(x=carrier, y=seat_miles))+
  geom_col()
