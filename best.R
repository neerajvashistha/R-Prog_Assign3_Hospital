best <- function(state, outcome) {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  file <- read.csv("outcome-of-care-measures.csv",colClasses="character")
  lst<-c("heart attack","heart failure","pneumonia")
  v<-as.character()
  if(!outcome %in% lst){
    stop("invalid outcome")
  }
  if(!state %in% file$State){
    stop("invalid state")
  }
  else{
    if(outcome=="heart attack"){
  
      q<-data.frame(file[,2],file[,7],file[,13])
      x<- data.frame(with(q,subset(q,q[,2] == state)))
  
      y<-which.min(apply(x,MARGIN = 1,min,na.rm=T))
      z<-x[y,]
      as.character(z[1,1])
    
    }else if(outcome=="heart failure"){
      q<-data.frame(file[,2],file[,7],file[,19])
      x<- data.frame(with(q,subset(q,q[,2] == state)))
      
      y<-which.min(apply(x,MARGIN = 1,min,na.rm=T))
      z<-x[y,]
      as.character(z[1,1])
    }else if(outcome=="pneumonia")
    {
      q<-data.frame(file[,2],file[,7],file[,25])
      x<- data.frame(with(q,subset(q,q[,2] == state)))
      
      y<-which.min(apply(x,MARGIN = 1,min,na.rm=T))
      z<-x[y,]
      as.character(z[1,1])
    }
  }
}
