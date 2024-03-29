pollutantmean<-function(directory,pollutant,id=1:332)
{
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used

  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  total<-0;
  n<-0;
  mm<-0;
  for(i in 1:length(id))
  {
    csvname<-sprintf("%03d.csv",id[i])
    full<-paste(directory,csvname,sep='/')
    data<-read.csv(full)
    cleandata<-data[complete.cases(data[pollutant]),]
    mm<-sapply(cleandata[pollutant],sum)
    total<-total+mm
   # n<-n+length(cleandata[pollutant])
    n<-n+dim(cleandata[pollutant])[1]
  
}
# print(n)
print(round(total/n, 3))
}
  
  