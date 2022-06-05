rm(list=ls())
library(plyr)
library(dplyr)

# Replace mising values with overall mean
#NA2mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
#rawdf[] <- replace(rawdf, TRUE, lapply(rawdf, NA2mean))
#rawdf

# binomial StdDev
binomialStdDev <- function(x){p=(sum(!is.na(x))-sum(x,na.rm=TRUE))/sum(!is.na(x));est = sqrt(p*(1-p)); return(est)}

#Create some same data
mydata <- data.frame("stratum"=c("A","A","A","B","B","B","A","A","A","B","B","B","A","A"),
                     "qtrseq" =c(1,1,2,2,3,3,4,4,5,5,6,6,7,7),
                     "cost"  =c(10,11,20,21,30,32,40,43,50,57,60,65,70,73))
qtrseq_min <- min(mydata$qtrseq)
qtrseq_max <- max(mydata$qtrseq)
by_qtrseq <- group_by(mydata, stratum, qtrseq);  
tq  <- summarise(by_qtrseq,qtr_avg = mean(cost, na.rm = TRUE),qtr_sum = sum(cost, na.rm = TRUE),qtr_sd = sd(cost, na.rm = TRUE),qtr_n = sum(!is.na(cost)))
t6m <- data.frame(stratum=character(),qtrseq= integer(0), sixmo_avg= numeric(0),sixmo_sum= numeric(0),sixmo_sd = numeric(0),sixmo_n= numeric(0)) 
tpy <- data.frame(stratum=character(),qtrseq= integer(0), yr_avg   = numeric(0),yr_sum   = numeric(0),yr_sd    = numeric(0),yr_n   = numeric(0)) 
for(i in qtrseq_max:qtrseq_min){ 
  by_qtrseq <- filter(mydata, qtrseq==i | qtrseq==i-1  )
  t<- cbind("qtrseq"=i,summarise(by_qtrseq, sixmo_avg = mean(cost, na.rm = TRUE),sixmo_sum = sum(cost, na.rm = TRUE),sixmo_sd = sd(cost, na.rm = TRUE),sixmo_n = sum(!is.na(cost))))
  t6m<-rbind(t6m,t)
  
  by_qtrseq <- filter(mydata, qtrseq<=i-2 & qtrseq>=i-5)
  t<- cbind("qtrseq"=i,summarise(by_qtrseq, prioryr_avg = mean(cost, na.rm = TRUE),prioryr_sum = sum(cost, na.rm = TRUE),prioryr_sd = sd(cost, na.rm = TRUE),prioryr_n = sum(!is.na(cost))))
  tpy<-rbind(tpy,t)
}
t6m
tpy

merged <- merge(x=tq,y=t6m,by="qtrseq")
merged <- merge(x=merged,y=tpy,by="qtrseq")
merged[with(merged,order(stratum, qtrseq) ),] 

#Rate Ratio
#=====================
require(epitools)
rateratio(c(15, 41, 19017, 28010),method="midp")$p.value[2,1]

#Odds Ratios
#=====================
dat <- matrix(c(2, 29, 64, 12),2,2,byrow=TRUE)
oddsratio.midp(dat)$p.value[2,1]   #P-value
oddsratio.midp(dat)$measure[2,1] # OR
oddsratio.midp(dat)$measure[2,2] # lower
oddsratio.midp(dat)$measure[2,3] # upper

#Poisson Counts or rates
#==========================
pois.exact(1:10, 101:110)