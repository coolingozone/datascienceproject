best <- function(state, outcome) {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  options(warn=-1)
  mm<-data.frame(id=0,nobs=0)
outdata<-read.csv("outcome-of-care-measures.csv",colClasses="character",)


small<-outdata[outdata$State==state,]
if(dim(small)==0)
  stop("invalid state")

if(outcome == "heart attack")
   outcol<-11
else if(outcome=="heart failure")
   outcol<-17
else if(outcome=="pneumonia")
  outcol<-23
else
  stop("invalid outcome")
small<-subset(small, select=c(2,outcol))
count<-1
bad<-!is.na(as.numeric(small[,2]))
small<-small[bad,]
small[,2]<-as.numeric(small[,2])

mm<-which(small[,2] == min(small[,2]), arr.ind=TRUE)
##print(class(small[1,2]))
##print(mm)
##print(small[mm,])
mm2<-sort(small[mm,1])
return(mm2[1])
##print(mm2)
##mm
options(warn=0)

}