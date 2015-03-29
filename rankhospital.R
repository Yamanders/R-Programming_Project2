rankhospital <- function(state = "", outcome = "", num = "best"){
                
  ## Function that returns a d.f. of the highest rated (lowest occurence of death) 
  ## for that outcome
  
  ## Sources
    source("testCases.R")
    ## You may need to install this package : install.packages("plyr")
    library(plyr)
  
  ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    is.na(data) <- data=="Not Available"
   ## data[, c(11, 17, 23)] <- as.numeric(data[, c(11, 17,23)])
    state <- toupper(state)

  ## check that state and outcome are valid
    testOutcome(outcome)
    testState(state)
  
  ## Convert string rank to num
    if (num == "best"){ num = 1}
    #if (num == "worst"){num = }
  
  ##30-day death rate
    if (outcome == "heart attack"){
      search <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
      temp <- data[data$State == state , ]
      if (num == "worst"){
          num <- sum(!is.na(temp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
      }
      temp[, 17] <- as.numeric(temp[, 17])
      
     ## hospital <- arrange(temp, )
      hospital <- arrange(temp, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, State)
      return(hospital[num, 2] )
    }
    
    if (outcome == "heart failure"){
      search <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
      temp <- data[data$State == state , ]
      
      ##temp <
      if (num == "worst"){
        num <- sum(!is.na(temp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
      }
      temp[, 17] <- as.numeric(temp[, 17])
      
      ##hospital <- temp[which.min(temp[,"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"]), 2]    
      hospital <- arrange(temp, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, Hospital.Name)
      #print(head(hospital[, 1:17]))
      print(tail(hospital[, 1:17]))
      return(hospital[num, 2] )
    }
    
    if (outcome == "pneumonia"){
      search <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
      
      if (num == "worst"){
        num <- sum(!is.na(temp$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
      }
      temp[, 17] <- as.numeric(temp[, 17])
      ##hospital <- temp[which.min(temp[,"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"]), 2]  
      hospital <- arrange(temp, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, State)
      return(hospital[num, 2] )
    }
  
} 