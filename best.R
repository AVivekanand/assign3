best <-function(state,outcome){
  infile <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
 #listoutcome <- list("heart attack","heart failure","pneumonia")
  if (!state %in% infile[,7]){
    stop("invalid state")
  }
  if (outcome == "heart attack"){
    outcome_column_num <- 11
  }
  else if (outcome == "heart failure"){
    outcome_column_num <- 17
  }
  else if (outcome == "pneumonia"){
    outcome_column_num <- 23
  }
  else{
    stop("invalid outcome")
  }
#if (!outcome %in% listoutcome){
#   stop("invalid outcome")
# } 
 reqdata <- subset (infile,State == state,select = c(7,2,outcome_column_num))
 comdata <- na.omit(reqdata)
 colnames(comdata) <- c("State","Hospital_Name","Outcome")
 comdata <- transform(comdata,Outcome = as.numeric(Outcome))
 hospitalname <- with(comdata,tapply(Hospital_Name,Outcome,min))
 #rOW_no <- which.min(as.double(data.state[,Outcome]))
 #colnames(hospitalname) <- c("Outcome")
 #sort(hospitalname[,1])
 return(hospitalname[[1]])
# return(hospital_name)
}