#Exercise 2 - Big Data


library(data.table)
library(dplyr)
library(ggplot2)

# Preparing Data
# Section 1
dt.yellow.stone = as.data.table(fread("yellowStone.csv"))
dt.yellow.stone = dt.yellow.stone[complete.cases(dt.yellow.stone$TMAX),]
# Section 2
dt.yellow.stone[,DATE := as.Date(DATE,"%Y-%m-%d")]
# Section 3
dt.yellow.stone[,YEAR := as.integer(format(as.Date(dt.yellow.stone$DATE, format="%Y-%m-%d"),"%Y"))]
dt.yellow.stone[,MONTH := as.integer(format(as.Date(dt.yellow.stone$DATE, format="%Y-%m-%d"),"%m"))]
# Section 4

dt.yellow.stone[,NOSAMPLES := sum(YEAR>0),by = paste0(YEAR)]
dt.yellow.stone = dt.yellow.stone[NOSAMPLES >= 250]
dt.yellow.stone[,NOSAMPLES := sum(MONTH>0),by = paste0(MONTH,YEAR)]
# Exploratory data analysis
dt.yellow.stone[,AVGTEMP := mean(TMAX) , by = paste0(MONTH,YEAR)]
# Take from the data, 100 years
dt.yellow.stone = dt.yellow.stone[YEAR > (max(YEAR) - 100)]


# Function show.monthly.temp
show.monthly.temp <- function(dt) {
  monthes = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
  par(mfrow = c(3,4))
  for(i in 1:12){
    temp.dt = dt%>%filter(dt$MONTH == i)%>%group_by(YEAR)%>%summarise(AVGTEMP = unique(AVGTEMP))
    #creating a linear model
    fit = lm(temp.dt$AVGTEMP ~ temp.dt$YEAR)
    
    plot(temp.dt$YEAR,temp.dt$AVGTEMP ,pch = 20,xlab = "YEARS",ylab = "AVG TEMP",
         main = paste(monthes[i],"\n","R^2 = ",round(summary(fit)$r.squared,3), ",p-value = ",round(summary(fit)$coefficients[,4][2],3))) 
    #multiply it in 100 to get the 100 year increase or decrese in celsiuos
    if((coef(fit)[2]*100)>0.5){
      abline(fit,col="red",lwd = 3)
    }
    else if((coef(fit)[2]*100)< -0.5){
      abline(fit,col="green",lwd = 3)
    }
    else{
      abline(fit,col="blue",lwd = 3)
    }
    
  }
}

show.monthly.temp(dt.yellow.stone)


# T-test - Function calc.diff
calc.diff <- function(dt){
  
  display.table = data.table("MONTH" = 1:12,"FIRST_RANGE" ="","LAST_RANGE" ="", "MEAN1"=0, "MEAN2"=0, "DIFF"=0, "N1"=0, "N2"=0, "P.VAL"=0, "FDR"=0, "STATUS"="", "SIG"="")
  
  mean1 = c()
  mean2 = c()
  diff = c()
  first.range = c()
  last.range = c()
  n1 = c()
  n2 = c()
  p_value = c()
  fdr = c()
  
  for(i in 1:12){
    temp.dt = dt%>%filter(dt$MONTH == i)
    temp.dt2 = as.data.table(temp.dt%>%group_by(YEAR)%>%summarise(AVGTEMP = unique(AVGTEMP),NOSAMPLES = unique(NOSAMPLES)))
    temp.dt2[,DIFF := temp.dt2$YEAR-shift(temp.dt2$YEAR)]
    temp.dt2$DIFF[1] = 1 #for the NA in the begining
    
    sum.first = 0
    sum.diff.first = 0
      for(j in 1:nrow(temp.dt2)){
        index = j:(j+9)
        # check if we getting out boundry
        if(index[10] > nrow(temp.dt2))break 
        
        first.10.years = temp.dt2$YEAR[index]
        sum.first = sum(temp.dt2$NOSAMPLES[index])
        sum.diff.first = sum(temp.dt2$DIFF[index])
        # checking if we have enough samples at this first 10 years and if they are really Continuous
        if(sum.first >= 250 && sum.diff.first == 10){
          first.10  = as.data.table(temp.dt%>%filter(temp.dt$YEAR %in% first.10.years))
          break
        }
      }
      
      sum.last = 0
      sum.diff.last = 0
      for(j in nrow(temp.dt2):1){
        #taking the last 10 Continuous years
        index = j:(j-9)
        if(index[10]< 1) break
        
        last.10.years = temp.dt2$YEAR[index]
        sum.last = sum(temp.dt2$NOSAMPLES[index])
        sum.diff.last = sum(temp.dt2$DIFF[index])
        if(sum.last >= 250 && sum.diff.last == 10){
          last.10 = as.data.table(temp.dt%>%filter(temp.dt$YEAR %in% last.10.years))
          break
        }
      }
      
      last.10.years = sort(last.10.years)
  
      
      mean1[i] = mean(first.10 $TMAX)
      mean2[i] = mean(last.10$TMAX)
      diff[i] = abs(mean1[i] - mean2[i])
      if(mean1[i] > mean2[i]){
        diff[i] = diff[i]*(-1)
      }
      first.range[i] = paste0(first.10.years[1] , " - " , first.10.years[10])
      last.range[i] = paste0(last.10.years[1] , " - " , last.10.years[10])
      n1[i] = sum.first
      n2[i] = sum.last
      p_value[i] = t.test(first.10$TMAX,last.10$TMAX,alternative = "two.sided")$p.value
   
  }
  
  display.table[,FIRST_RANGE := first.range]
  display.table[,LAST_RANGE := last.range]
  display.table[,MEAN1 := mean1]
  display.table[,MEAN2 := mean2]
  display.table[,DIFF := diff]
  display.table[,N1 := n1]
  display.table[,N2 := n2]
  display.table[,P.VAL := p_value]
  display.table[,STATUS := ifelse(DIFF < 0,"LOWER","HIGHER")]
  display.table[,FDR := p.adjust(p_value,method = "fdr")]

  display.table$SIG = ""
  display.table$SIG[which(display.table$FDR < 0.001)] = "***"
  display.table$SIG[which(display.table$FDR < 0.01 & display.table$FDR > 0.001)] = "**"
  display.table$SIG[which(display.table$FDR < 0.05 & display.table$FDR > 0.01)] = "*"
  
  print(display.table)
  
}

calc.diff(dt.yellow.stone)


