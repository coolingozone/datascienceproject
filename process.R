mydata=read.csv("datalocation1.csv")
total=0
count=0;
address="00:1a:1e:b6:98:e8"
noscale<-data.frame(x=integer(0),y=numeric(0))
for (i in 1:nrow(mydata))
{
 if(grepl(address,mydata[i,6]))
  {
    total=abs(mydata[i,7])+total
    count=count+1
    noscale[count,1]=count
    noscale[count,2]=abs(mydata[i,7])
  }
  
}
mean=total/count;
total=0
count=0
for (i in 1:nrow(mydata))
{
  if(grepl(address,mydata[i,6])  )
  {
    total=((abs(mydata[i,7])-mean)*(abs(mydata[i,7])-mean))+total
    count=count+1
  }
  
}
dev=sqrt(total/count)
location<-data.frame(x=integer(0),y=numeric(0))
count2=1
for (i in 1:nrow(mydata))
{
  if(grepl(address,mydata[i,6])  )
  {
    location[count2,1]=count2
    location[count2,2]=(abs(mydata[i,7])-mean)/dev
    count2=count2+1
   
  }
  
}




address="00:1a:1e:b8:76:a8"
noscale2<-data.frame(x=integer(0),y=numeric(0))
count=0
for (i in 1:nrow(mydata))
{
  if(grepl(address,mydata[i,6])  )
  {
    total=abs(mydata[i,7])+total
    count=count+1
    noscale2[count,1]=count
    noscale2[count,2]=abs(mydata[i,7])
  }
  
}
mean=total/count;
total=0
count=0
for (i in 1:nrow(mydata))
{
  if(grepl(address,mydata[i,6])  )
  {
    total=((abs(mydata[i,7])-mean)*(abs(mydata[i,7])-mean))+total
    count=count+1
  }
  
}
dev=sqrt(total/count)
location2<-data.frame(x=integer(0),y=numeric(0))
count3=1
for (i in 1:nrow(mydata))
{
  if(grepl(address,mydata[i,6])  )
  {
    location2[count3,1]=count3
    location2[count3,2]=(abs(mydata[i,7])-mean)/dev
    count3=count3+1
    
  }
  
}
matplot(noscale[2],type="p",pch="*",col="red")
par(new=TRUE)
matplot(noscale2[2],type="p",pch="*",col="green")

#matplot(location[2],type="p",pch="*",col="red")
#par(new=TRUE)
#matplot(location2[2],type="p",pch="*",col="green")