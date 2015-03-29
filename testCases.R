testOutcome <- function (deathOutcome){
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  testOutcome <- ( is.element ( deathOutcome, outcomes ))

  if (!testOutcome){
    stop("invalid outcome")
  }
}

testState <- function(deathState){
  testState <- ( is.element ( deathState, state.abb ) ) 
  if (!testState){
  stop("invalid state")
  } 
  
}
