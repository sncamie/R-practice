mydata<-read.csv("C:\\Users\\sncamie\\Documents\\Teaching\\physics 141\\Feedback\\screen_size-data.csv")

dim(mydata)

head(mydata,10)

#1. Data is continuous.
#2. Observations are randomly selected.
#3. To check the data is normally distributed,

qqnorm(mydata$Screen_size.in.cm.)
qqline(mydata$Screen_size.in.cm.,col="red")
#since most of the data lies on the read line, we can say the data is normally distributed


#conduct a one sample t-test

t.test(mydata$Screen_size.in.cm.,mu=10)

#fail to reject H0