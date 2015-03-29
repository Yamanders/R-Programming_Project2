rankall <- function (outcome = "", num = "best"){
  ## Function to output data.frame of hospitals in each place that match a ranking.
  ## Written by Amanda Molling 03/25/2015
  
  ## Read outcome data
     data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    ## hosData<- read.csv("hospital-data.csv", colClasses = "character")
     locationsList <- sort(unique(data[, 7]))
     is.na(data) <- data=="Not Available"  
     
  ## Convert string rank to num
     if (num == "best"){ 
         num = 1
        
     }
     originalNum <- num
     
  ## Sources and librarys
  source("testCases.R")
  library(plyr)
  
  df <- as.data.frame(matrix(0, ncol = 2, nrow = length(locationsList)))
  row.names(df) <- locationsList
  colnames(df) <- c("hospital", "state")

  ## Check outcome
    testOutcome(outcome)
  
  ## For each state, find the hospital of the given rank:
  if (outcome == "heart attack"){
    for (i in locationsList){
      temp <- data[data$State == i, ] 
      if (num == "worst"){
        originalNum <- num
        num <- sum(!is.na(temp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
      }
      temp[, 11] <- as.numeric(temp[, 11])        
      hospital <- arrange(temp, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, Hospital.Name)
      df[i, ] <- hospital[num, c(2, 7)]
      num <- originalNum
    }
  }
  
  
      if (outcome == "heart failure"){

        for(i in locationsList){
          temp <- data[data$State == i, ]
          
          if (num == "worst"){
            originalNum <- num
            num <- sum(!is.na(temp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
          }
          temp[, 17] <- as.numeric(temp[, 17])
          hospital <- arrange(temp, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, Hospital.Name)
          df[i, ] <- hospital[num, c(2, 7)]
          num <- originalNum
        }
      }
      

    if (outcome == "pneumonia"){
      for (i in locationsList){
        ##print(i)
        temp <- data[data$State == i, ] 
        if (num == "worst"){
          originalNum <- num
          num <- sum(!is.na(temp$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
          ##print("Num is") 
          ##print(num)
        }
        temp[, 23] <- as.numeric(temp[, 23])
        hospital <- arrange(temp, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, Hospital.Name) 
        #print(tail(hospital[, c(2,7,23)]))
        df[i, ] <- hospital[num, c(2, 7)]
        
        num <- originalNum
      }
    }
  
  
  return(df)
}