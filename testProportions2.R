rm(list=ls())


mydata <-data.frame("num1"=c(120,130,140,0),"num2"=c(220,430,560,50),"den1"=c(12000,13000,30000,40000),"den2"=c(52000,73000,99000,40000))

#Midp - fails for o entries
 #tests  <-apply(mydata[,c('num1','num2','den1','den2')], 1, function(x) epitools::rateratio.midp(matrix(x,2,2))) 
 #for (i in 1:nrow(mydata)){mydata$rr[i]= tests[[i]][[2]][[2]];
 #                         mydata$pvalue[i]= tests[[i]][[3]][[2]];
 #                         mydata$SignificanceStatus[i]= ifelse(mydata$pvalue[i]<0.05, ifelse(mydata$rr[i]>1.0, 'lower','higher'),'ns')
 #                         }

#Fisher exact
#================
testsf  <-apply(mydata[,c('num1','num2','den1','den2')], 1, function(x) fisher.test(matrix(x,2,2))) 

for (i in 1:nrow(mydata)){mydata$rr[i]= testsf[[i]][[3]][[1]];
mydata$pvalue[i]= testsf[[i]][[1]][[1]];
mydata$SignificanceStatus[i]= ifelse(mydata$pvalue[i]<0.05, ifelse(mydata$rr[i]>1.0, 'higher','lower'),'ns')
}


T.test <- function(n1, mean1, sd1, n2, mean2, sd2) {
  s <- (((n1 - 1) * sd1^2)+((n2 - 1) * sd2^2)) / (n1+n2 - 2) # weighted variance
  t <- sqrt( n1*n2/(n1+n2)) * (mean1-mean2) / sqrt(s) # t statistic
  df <- n1+n2- 2  # degrees of freedom
  p <- as.vector((1 - pt(abs(t), df)) * 2) # p value
  return(p)
 # c(t = t, p = p)
}

#t-test
#================
T.test(mydata$den1, mydata$num1, mydata$den2, mydata$num2, mydata$den2, mydata$num2)
T.test(0,0,0,0,0,0)
