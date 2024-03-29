complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  result<-data.frame(id=0,nobs=0)
  n<-0;
  for(i in 1:length(id))
  {
    csvname<-sprintf("%03d.csv",id[i])
    full<-paste(directoryr,csvname,sep='/')
    data<-read.csv(full)
    cleandata<-data[complete.cases(data),]
     n<-dim(cleandata["nitrate"])[1]
    
    result[i,1]<-id[i]
    result[i,2]<-n
    
  
  }

    result
  
}