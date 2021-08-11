library(tidyverse)
library(moderndive)
library(lubridate)
library(infer)
        
getwd()
StudyArea2 <-read.csv("C:/Users/sncamie/Documents/R/R practice/IntroR/Data/StudyArea.csv")



for (fire in 1:nrow(StudyArea2)) {print(StudyArea2[fire, "TOTALACRES"])}


dfCrime <- read.csv("Crime_Data.csv", header = TRUE)

View(dfCrime)

dfCrime_small <- select(dfCrime, "CrimeDate" = "Reported.Date", "Category"="Crime.Subcategory",
                        "Description"="Primary.Offense.Description", "Precinct", "Sector",
                        "Beat", "Neighborhood")

glimpse(dfCrime_small)


Queen_neighborhood <- dfCrime_small %>%
  filter(Neighborhood=="QUEEN ANNE")

glimpse(Queen_neighborhood)

Only_Beat <- Queen_neighborhood %>%
  group_by(Beat) %>%
  summarise(n=n())

head(Only_Beat)

ggplot(Only_Beat, aes(x=Beat, y=n))+
  geom_col(fill="red")


burglaries_at_Queen <- Queen_neighborhood %>%
  filter(Category=="BURGLARY-RESIDENTIAL")

View(burglaries_at_Queen)


Year_of_crime <- burglaries_at_Queen %>%
  mutate(dates=mdy(CrimeDate),
         MONTH=month(dates),
         YEAR=year(dates))

View(Year_of_crime)

Grouped_year <- Year_of_crime %>%
  group_by(YEAR) %>%
  summarise(n=n())
glimpse(Grouped_year)

ggplot(Grouped_year, aes(x=YEAR, y=n))+
  geom_col(fill="red")



Grouped_month <- Year_of_crime %>%
  group_by(MONTH) %>%
  summarise(n=n())
glimpse(Grouped_month)

ggplot(Grouped_month, aes(x=MONTH, y=n))+
  geom_col(fill="red")


#using the filter function 

dfFires <- read_csv("StudyArea.csv", col_types = list(UNIT=col_character()), 
                    col_names = TRUE )
df1k <- dfFires %>%
  filter(TOTALACRES >= 1000, YEAR_==2016)

nrow(df1k)

dfFires2 <- dfFires %>%
  select("Fire"=FIRENAME,"Acres"= TOTALACRES, "yr"=YEAR_)
head(dfFires2)

dfFires3 <- dfFires %>%
  select(contains("DATE"))
glimpse(dfFires3)


#mutate 

df<- dfFires %>%
  select(ORGANIZATI, STATE, YEAR_, TOTALACRES, CAUSE, STARTDATED) %>%
  filter(TOTALACRES >=1000 & CAUSE %in% c("Human", "Natural")) %>%
  mutate(dates= mdy_hm(STARTDATED),
         ) %>%
  mutate(ifelse(YEAR_%in%1980:1989, "1980-1989"), ifelse(YEAR_%in%1990:1999, "1990-1999"),
         ifelse(YEAR_%in%2000:2009, "2000-2009"),ifelse(YEAR_%in%2000:2009, "2010-2016"))
view(df)
