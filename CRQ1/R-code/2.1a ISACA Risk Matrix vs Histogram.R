# www.isacajournal-digital.org/isacajournal/2019_volume_3/MobilePagedArticle.action?articleId=1485390#articleId1485390
# Figure 3—R Code to Plot the Bar Chart in Figure 2 
library(ggplot2) 
 
n <- 10000            #iterations 
lp <- c(0, 0.3)       #low probability range
li <- c(0, 10000)     #low impact range
mp <- c(0.3, 0.7)     #medium probability range   
mi <- c(10000, 20000) #medium impact range
hp <- c(0.7, 1.0)     #high probability range   
hi <- c(20000,30000)  #high impact range

slp = runif(n, min(lp), max(lp))   #random numbers in low probability range 
sli = runif(n, min(li), max(li))   #random numbers in low impact range
smp = runif(n, min(mp), max(mp))   #random numbers in med probability range 
smi = runif(n, min(mi), max(mi))   #random numbers in med impact range
shp = runif(n, min(hp), max(hp))   #random numbers in high probability range 
shi = runif(n, min(hi), max(hi))   #random numbers in highimpact range

lr = c(slp*sli, slp*smi, smp*sli)  #low risks defined as green region in risk matrix and its possible variants, R=P*I
mr = c(smp*smi, slp*shi, shp*sli)  #med risks defined as amber region in risk matrix and its possible variants, R=P*I
hr = c(shp*shi, smp*shi, shp*smi)  #high risks defined as red region in risk matrix and its possible variants, R=P*I

dlr = data.frame(x=lr)             #data frame with low risks and label "Low"
dlr$risk = "Low"

dmr = data.frame(x=mr)             #data frame with medium risks and label "Med"  
dmr$risk = "Med"

dhr = data.frame(x=hr)             #data frame with high risks and label "High"  
dhr$risk = "High"

#risk_thresholds <- c(0, HighestLow, LowestMed, HighestMed, LowestHig, HighestHig)

df = rbind(dlr, dmr, dhr)          #bind all data data frames into one dataset

ggplot() + geom_histogram(data = df, aes(x=x, y=..count..,fill=risk),
colour="white", bínwidth = 1000, boundary = 0)+
scale_fill_manual(values = c("#CC0033","#33FF00", "#FFFF33")) + 
  #geom_vline(xintercept = risk_thresholds, linetype = "dashed", color = "black") +
  xlab(label = "Yc") 
                          