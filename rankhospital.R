rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  file <- read.csv("outcome-of-care-measures.csv",colClasses="character")
  lst<-c("heart attack","heart failure","pneumonia")
  v<-as.numeric(file[,11])
  w<-as.numeric(file[,17])
  u<-as.numeric(file[,23])
# state="WA"
#  outcome="heart attack"
#   num=7
  if(!outcome %in% lst){
    stop("invalid outcome")
  }
  if(!state %in% file$State){
    stop("invalid state")
  }  else{
    if(outcome=="heart attack"){
      
      q<-data.frame(file[,2],file[,7],v)
      x<- data.frame(with(q,subset(q,q[,2] == state)))
      x<-x[order(x[,3]),]
      if(num=="best"){
      y<-which.min(apply(x,MARGIN = 1,min,na.rm=T))
      z<-x[y,]
      as.character(z[1,1])
    }   else  if(num=="worst"){
      y<-which.max(apply(x,MARGIN = 1,min,na.rm=T))
      z<-x[y,]
      as.character(z[1,1])
    }else if(is.numeric(num)){
      as.character(x[num,1])
    }
    
    } else if(outcome=="heart failure"){
      q<-data.frame(file[,2],file[,7],w)
      x<- data.frame(with(q,subset(q,q[,2] == state)))
#       as.numeric(levels(f))[f]
   x<-x[order(x[,3]),]
#       x<-sort(x[,3],)
      if(num=="best"){
        y<-which.min(apply(x,MARGIN = 1,min,na.rm=T))
        z<-x[y,]
        as.character(z[1,1])
      }else  if(num=="worst"){
        y<-which.max(apply(x,MARGIN = 1,min,na.rm=T))
        z<-x[y,]
        as.character(z[1,1])
      }else if(is.numeric(num)){
        as.character(x[num,1])
      }
    }  else if(outcome=="pneumonia")
    {
      q<-data.frame(file[,2],file[,7],u)
      x<- data.frame(with(q,subset(q,q[,2] == state)))
      
      x<-x[order(x[,3]),]
      if(num=="best"){
        y<-which.min(apply(x,MARGIN = 1,min,na.rm=T))
        z<-x[y,]
        as.character(z[1,1])
      }else  if(num=="worst"){
        y<-which.max(apply(x,MARGIN = 1,min,na.rm=T))
        z<-x[y,]
        as.character(z[1,1])
      }else if(is.numeric(num)){
        as.character(x[num,1])
      }
    }
  }
}
