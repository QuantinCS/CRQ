#uniform generally

#create vector of volumes for 10000 pipets drawn at random from uniform distribution
pipet = runif(10000, 9.98, 10.02)

# create histogram using 20 bins of size 0.002 mL
pipet_hist = hist(pipet, breaks = seq(9.98, 10.02, 0.002), col = c("blue", "lightblue"), ylab = "number of pipets", xlab = "volume of pipet (mL)", main = NULL)

# overlay points showing expected values for uniform distribution
points(pipet_hist$mids, rep(10000/20, 20), pch = 19)





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

#slp_hist = hist(slp, breaks = seq(0, 0.2), col = c("blue", "lightblue"), ylab = "number of pipets", xlab = "volume of pipet (mL)", main = NULL)

slp_hist = hist(slp, col = c("blue", "lightblue","cyan"), 
                ylab = "number of random cases", 
                xlab = "probability of cases", main = NULL)


slp_hist = hist(slp, col = c("blue", "lightblue","cyan"), 
                ylim = c(39500, 40500),
                ylab = "number of random cases", 
                xlab = "probability of cases", main = NULL)

smp_hist = hist(smp, col = c("red", "coral2","brown"), 
                ylab = "number of random cases", 
                xlab = "probability of cases", main = NULL)


smp_hist = hist(smp, col = c("red", "coral2","brown"), 
                ylim = c(39500, 40500),
                ylab = "number of random cases", 
                xlab = "probability of cases", main = NULL)




x <- seq(1,10000)


udm1 = data.frame(x=x)             
udm1$prob <- runif(udm1$x, min(mp), max(mp))
udm1$imp <- runif(udm1$x, min(mi), max(mi))

udm2 = data.frame(x=x)             
udm2$prob <- runif(udm2$x, min(mp), max(mp))
udm2$imp <- runif(udm2$x, min(hi), max(hi))

udm3 = data.frame(x=x)             
udm3$prob <- runif(udm3$x, min(hp), max(hp))
udm3$imp <- runif(udm3$x, min(mi), max(mi))

df = rbind(udm1, udm2,udm3)          #bind all data data frames into one dataset


#check DF

dim(df)
str(df)
summary(df)
colnames(df)

head(df, n = 10)
tail(crime, n = 5)

View(df)


plot(udm1$prob,udm1$imp, xlim = c(0, 1), ylim = c(0, 30000))
plot(udm2$prob,udm2$imp, xlim = c(0, 1), ylim = c(0, 30000))
plot(udm3$prob,udm3$imp, xlim = c(0, 1), ylim = c(0, 30000))


plot(df$prob,df$imp, xlim = c(0, 1), ylim = c(0, 30000))

rect(min(mp), min(mi), max(mp), max(mi), density = NULL, angle = 45,
     col = NA, border = NULL, lty = par("lty"), lwd = par("lwd"))


udm4 = data.frame(x=x)             
udm4$prob <- runif(udm4$x, min(lp), max(hp))
udm4$imp <- runif(udm4$x, min(li), max(mi))
plot(udm4$prob,udm4$imp, xlim = c(0, 1), ylim = c(0, 30000))




if (FALSE){
  #define x-axis
  x <- seq(-4, 4, length=100)
  
  #calculate uniform distribution probabilities
  y <- dunif(x, min = -3, max = 3)
  
  
  #plot uniform distribution
  plot(slp, type = 'l', lwd = 3, ylim = c(0, .2), col='blue',
     xlab='x', ylab='Probability', main='Uniform Distribution Plot')
}







LowestLow = min(lp) * min(li)
HighestLow = max(lp) * max(li)
LowestMed = min(mp) * min(mi)
HighestMed = max(mp) * max(mi)
LowestHig = min(hp) * min(hi)
HighestHig = max(hp) * max(hi)

lr = c(slp*sli, slp*smi, slp*shi, smp*sli, shp*sli)  #low risks defined as green region in risk matrix and its possible variants, R=P*I
mr = c(smp*smi, smp*shi, shp*smi)  #med risks defined as amber region in risk matrix and its possible variants, R=P*I
hr = c(shp*shi)                    #high risks defined as red region in risk matrix and its possible variants, R=P*I







plot(mr, ylim = c(0, 30000),xlim = c(0,30000), , col='yellow')


