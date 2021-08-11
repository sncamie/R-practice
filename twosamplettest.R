data2<-read.csv("ind.csv")

dim(data2)

head(data2,10)

#homegenity of variance

var(data2$screensize_sample1)
var(data2$screensize_sample2)

#variances are close to each we can move along 

#Step 4 - Conduct two-sample t-test

#Null Hypothesis: There is no difference between the mean of two samples
#Alternate Hypothesis: There is difference between the men of two samples

t.test(data2$screensize_sample1,data2$screensize_sample2,var.equal = T)

#fail to reject h0