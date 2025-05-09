# ISACA example improved with weak consistency rules 
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

LowestLow = min(lp) * min(li)
HighestLow = max(lp) * max(li)
LowestMed = min(mp) * min(mi)
HighestMed = max(mp) * max(mi)
LowestHig = min(hp) * min(hi)
HighestHig = max(hp) * max(hi)




lr = c(slp*sli, slp*smi, slp*shi, smp*sli, shp*sli)  #low risks defined as green region in risk matrix and its possible variants, R=P*I
mr = c(smp*smi, smp*shi, shp*smi)  #med risks defined as amber region in risk matrix and its possible variants, R=P*I
hr = c(shp*shi)                    #high risks defined as red region in risk matrix and its possible variants, R=P*I

dlr = data.frame(x=lr)             #data frame with low risks and label "Low"
dlr$risk = "Low"
dlr$lowest = LowestLow
dlr$highest = HighestLow
dlr$Confidence <- ifelse(dlr$x >= dlr$lowest & dlr$x <= dlr$highest, "In", "Out")
dlr$RealRank <- ifelse(dlr$Confidence == "In", "1 TrueLow", "2 FalseLow")


dmr = data.frame(x=mr)             #data frame with medium risks and label "Med"  
dmr$risk = "Med"
dmr$lowest = LowestMed
dmr$highest = HighestMed
dmr$Confidence <- ifelse(dmr$x >= dmr$lowest & dmr$x <= dmr$highest, "In", "Out")
dmr$RealRank <- ifelse(dmr$Confidence == "In", "3 TrueMedium", "4 FalseMedium")


dhr = data.frame(x=hr)             #data frame with high risks and label "High"  
dhr$risk = "High"
dhr$lowest = LowestHig
dhr$highest = HighestHig
dhr$Confidence <- ifelse(dhr$x >= dhr$lowest & dhr$x <= dhr$highest, "In", "Out")
dhr$RealRank <- ifelse(dhr$Confidence == "In", "6 TrueHigh", "5 FalseHigh")

risk_thresholds <- c(0, HighestLow, LowestMed, HighestMed, LowestHig, HighestHig)

df = rbind(dlr, dmr, dhr)          #bind all data data frames into one dataset

risk_confidence_table <- table(df$risk, df$Confidence)
#risk_confidence_proportions <- prop.table(risk_confidence_table, margin = 1) * 100
risk_confidence_proportions <- round(prop.table(risk_confidence_table, margin = 1) * 100, 1)


#ggplot() + geom_histogram(data = df, aes(x=x, y=..count..,fill=risk),
#colour="white", binwidth = 1000)+
#scale_fill_manual(values = c("#CC0033","#33FF00", "#FFFF33")) + xlab(label = "Yc") 

ggplot() + geom_histogram(data = df, aes(x=x, y=..count..,fill=RealRank),
colour="white", binwidth = 1000, boundary = 0)+
  scale_fill_manual(values = c("2 FalseLow" = "#90EE90", 
                               "1 TrueLow" = "#33FF00", 
                               "3 TrueMedium" = "#FFFF33", 
                               "4 FalseMedium" = "#FFBF00", 
                               "5 FalseHigh" = "#FF6666", 
                               "6 TrueHigh" = "#CC0033")
                    ) + 
  geom_vline(xintercept = risk_thresholds, linetype = "dashed", color = "black") +
  xlab(label = "Yc") +
  annotate(geom="label", x=HighestLow/2, y=n/2, label=paste(risk_confidence_proportions["Low","In"]," %"),
             color="black") +
  annotate(geom="label", x=HighestLow*2, y=n/3, label=paste(risk_confidence_proportions["Low","Out"]," %"),
           color="black") +
  annotate(geom="label", x=HighestMed/1.5, y=n/4, label=paste(risk_confidence_proportions["Med","In"]," %"),
           color="black") +
  annotate(geom="label", x=HighestMed*1.2, y=n/5, label=paste(risk_confidence_proportions["Med","Out"]," %"),
           color="black") +
  annotate(geom="label", x=HighestHig/1.2, y=n/8, label=paste(risk_confidence_proportions["High","In"]," %"),
           color="black") #+
  #annotate(geom="label", x=HighestHig*2, y=n/6, label=paste(risk_confidence_proportions["High","Out"]," %")
  #         color="black")
  
  
  
#labels=c('FalseLow', 'TrueLow', 'FalseMedium', 'TrueMedium','FalseHigh', 'TrueHigh'


                          