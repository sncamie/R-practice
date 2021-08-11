library(tidyverse)
library(moderndive)

bowl

virtual_shovel<-bowl%>%
  rep_sample_n(size = 50)

virtual_shovel

virtual_shovel %>%
  mutate(is_red=(color=="red"))%>%
  summarise(num_red=sum(is_red)) %>%
  mutate(prop_red=num_red/50)
#repeat experiment 33 times 

virtual_samples<- bowl %>%
  rep_sample_n(size=50, reps=33)

virtual_samples

#count the number of reds

virtual_prop_red <- virtual_samples %>%
  group_by(replicate)%>%
  summarise(red=sum(color=="red")) %>%
  mutate(prop_red=red/50)
virtual_prop_red

#lets visualize

ggplot(virtual_prop_red, aes(x=prop_red))+
  geom_histogram(binwidth = 0.05,boundary=0.4, color="white")+
  labs(x="Propotion of 50 balls that were red",
       title = "33 experiments")
