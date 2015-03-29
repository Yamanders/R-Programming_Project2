best <- function(state = "", outcome = ""){
  ## Function that reads the outcome of care csv and return the name of the hospital with the best results for 30 days for that cause of death.
  
  ## Read outcome data
  ## data <- read.csv(unzip("https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2FProgAssignment3-data.zip"), colClasses = "character")
  
  
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  is.na(data) <- data=="Not Available"
  
  ## check that state and outcome are valid
      state <- toupper(state)
      outcomes <- c("heart attack", "heart failure", "pneumonia")
      testState <- ( is.element ( state, state.abb ) )  
  
      testOutcome <- ( is.element ( outcome, outcomes ))
  
      if (!testState){
        stop("invalid state")
      } 
      if (!testOutcome){
        stop("invalid outcome")
      }
      else if (  (testState) & (testOutcome) ){
  
    if (outcome == "heart attack"){
      search <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
       temp <- data[data$State == state , ]
      hospital <- temp[which.min(temp[,"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"]), 2]    
      return(hospital)
    }
    
    if (outcome == "heart failure"){
      search <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
      temp <- data[data$State == state , ]
      hospital <- temp[which.min(temp[,"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"]), 2]    
      return(hospital)
     }

    if (outcome == "pneumonia"){
      search <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
      temp <- data[data$State == state , ]
      hospital <- temp[which.min(temp[,"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"]), 2]    
      return(hospital)
    }
    
  }
    
  

  ## Return hospital name in that state w/lowest 30-day death. 
}