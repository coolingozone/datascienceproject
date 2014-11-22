corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
 result<-vector(mode="numeric", length=0)
  id<-c(1:332)
  count<-0
  for(i in 1:length(id))
  {
    csvname<-sprintf("%03d.csv",id[i])
    full<-paste(directory,csvname,sep='/')
    data<-read.csv(full)
    if(sum(complete.cases(data))>threshold)
    {
   cleandata<-data[complete.cases(data),2:3]
   datamat.mat<-as.matrix(cleandata[,1:2])
    count<-count+1
 
   result[count]<-cor(datamat.mat[,1],datamat.mat[,2],  use = "complete")
    }
}
if(count==0)
{
 return(vector(mode="numeric",length=0))
  #result<-vector(mode="numeric", length=0)
  
}
#print(result)
return(result)

}