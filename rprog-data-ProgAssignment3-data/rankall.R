rankall <- function(outcome,num="best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  options(warn=-1)

  mm<-data.frame(id=0,nobs=0)
  mm3<-data.frame(hospital=0,state=0)
  if(outcome == "heart attack")
    outcol<-11
  else if(outcome=="heart failure")
    outcol<-17
  else if(outcome=="pneumonia")
    outcol<-23
  else
    stop("invalid outcome")
  outdata<-read.csv("outcome-of-care-measures.csv",colClasses="character",)
  
  unstate<-unique(outdata[,"State"])
  count<-1
  for(i in 1:length(unstate))
  {
  small<-outdata[outdata$State==unstate[i],]
  small<-subset(small, select=c(2,7,outcol))
  bad<-!is.na(as.numeric(small[,3]))
  small<-small[bad,]
  small[,3]<-as.numeric(small[,3])
  mm2<-small[order(small[,3],small[,1]),]
  
  if(num=="best")
  {
    mm3[count,]<-subset(mm2[1,], select=c(1,2))
    count<-count+1
    
  }
  else if(num=="worst")
  {
    
    len<-dim(mm2)
    mm3[count,]<-subset(mm2[len[1],], select=c(1,2))
   count<-count+1
   
  }
  else
  {
    if(as.numeric(num)<=dim(mm2))
    {
      mm3[count,]<-mm2[as.numeric(num),]
      count<-count+1
    }
    else
    {
      mm3[count,2]<-unstate[i]
     mm3[count,1]<-NA
     count<-count+1
      
    }
  }
  ##print(mm2)
  ##mm
  }

  options(warn=0)
  mm3<-mm3[order(mm3[,2]),]
return(mm3)
  
}