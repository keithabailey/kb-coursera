# Find the mean of each group
library(dplyr)
library(ggplot2)

########## Part 1 ########

lambda <- 0.2
simulations <- 1000

#Actual mean & sd of exp distribution
exp_mu<-1/lambda
exp_sd<-exp_mu

hist(runif(simulations))

mns = NULL
for (i in 1 : simulations) mns = c(mns, mean(rexp(40, lambda)))
hist(mns)

mean(mns)
se<-sd(mns)/sqrt(40)
exp



################### Toothgrowth Analysis ##############

  cToothGrowth<-ToothGrowth%>%group_by(supp, dose)%>%summarise(len.mean=mean(len),count = n(),len.sd=sd(len), len.95=1.96*len.sd)

# Density plots with means
ggplot(ToothGrowth, aes(x=len, fill=supp)) +
  geom_density() +
  geom_vline(data=cToothGrowth, aes(xintercept=len.mean),
             linetype="dashed", size=1) + facet_grid(dose~supp) +
  geom_text(data=cToothGrowth, aes(x=40,  0.2, label=len.mean), size=3, parse=T) 

ggplot(ToothGrowth, aes(x=len, fill=supp)) +
  geom_density() +
  geom_vline(data=cToothGrowth, aes(xintercept=len.mean),
             linetype="solid", size=1, colour="orange") +
  geom_vline(data=cToothGrowth, aes(xintercept=len.mean+len.95),
             linetype="dashed", size=1, colour="red") +
  geom_vline(data=cToothGrowth, aes(xintercept=len.mean-len.95),
             linetype="dashed", size=1, colour="green") +
  facet_grid(supp~dose)

t.test(x=ToothGrowth$len[ToothGrowth$dose==0.5 & ToothGrowth$supp=="OJ"],
       y=ToothGrowth$len[ToothGrowth$dose==0.5 & ToothGrowth$supp=="VC"])
       
t.test(x=ToothGrowth$len[ToothGrowth$dose==1 & ToothGrowth$supp=="OJ"],
       y=ToothGrowth$len[ToothGrowth$dose==1 & ToothGrowth$supp=="VC"])

t.test(x=ToothGrowth$len[ToothGrowth$dose==2 & ToothGrowth$supp=="OJ"],
       y=ToothGrowth$len[ToothGrowth$dose==2 & ToothGrowth$supp=="VC"])
