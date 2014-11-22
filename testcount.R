mydata=read.csv("datalocation1.csv")
#newdata=mydata[1:22,]

myvars <- c("MAC", "Signal.Strength")
ux<- mydata[myvars]


plot(ux[1:10,1], ux[1:10,2], type = "o", pch = 2, col = "blue")