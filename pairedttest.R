data3<-read.csv("paired1.csv")
dim(data3)
head(data3,10)

#check for normality 
qqnorm(data3$tyre_1)
qqnorm(data3$tyre_2)
qqline(data3$tyre_1, col='red')
qqline(data3$tyre_2, col='red')

t.test(data3$tyre_1,data3$tyre_2, paired = T)
