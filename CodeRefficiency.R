####DATA ENVELOPMENT ANALYSIS 
####Input-oriented, output-oriented and input-output oriented (hyperbolic) measures
####In the code we assume one dataset for the year 2021; we have one output, y, and seven inputs, x1-x7
####FEAR package needs to be installed as it is not available in CRAN. 


library(FEAR)

data(data2021final)
x=matrix(nrow=7,ncol=378)  
x[1,]= data2021final$x1
x[2,]= data2021final$x2
x[3,]= data2021final$x3
x[4,]= data2021final$x4
x[5,]= data2021final$x5
x[6,]= data2021final$x6
x[7,]= data2021final$x7
y=matrix(nrow=1,ncol=378)
y[1,]= data2021final$y

####Input-oriented

dhat=dea(XOBS=x,YOBS=y,RTS=1,ORIENTATION=1,errchk=TRUE)

tmp=boot.sw98(XOBS=x,YOBS=y, NREP=500,DHAT=dhat, RTS=1,ORIENTATION=1, alpha=0.05, CI.TYPE=2,errchk=TRUE)

n=ncol(x)

table.in=matrix(nrow=n,ncol=7)

table.in[,1]=c(1:n)

table.in[,2]=dhat

table.in[,3]=dhat-tmp$bias

table.in[,4]=tmp$bias

table.in[,5]=tmp$var

table.in[,6:7]=tmp$conf.int

write.table(table.in, file="input2021.txt")


####Hyperbolic

dhat=dea(XOBS=x,YOBS=y,RTS=1,ORIENTATION=3,errchk=TRUE)

tmp=boot.sw98(XOBS=x,YOBS=y, NREP=500,DHAT=dhat, RTS=1,ORIENTATION=3, alpha=0.05, CI.TYPE=2,errchk=TRUE)
n=ncol(x)

table.in=matrix(nrow=n,ncol=7)

table.in[,1]=c(1:n)

table.in[,2]=dhat

table.in[,3]=dhat-tmp$bias

table.in[,4]=tmp$bias

table.in[,5]=tmp$var

table.in[,6:7]=tmp$conf.int

write.table(table.in, file="hyperbolic2021.txt")


####Output-oriented

dhat=dea(XOBS=x,YOBS=y,RTS=1,ORIENTATION=2,errchk=TRUE)

tmp=boot.sw98(XOBS=x,YOBS=y, NREP=500,DHAT=dhat, RTS=1,ORIENTATION=2, alpha=0.05, CI.TYPE=2 , errchk=TRUE)

n=ncol(x)

table.in=matrix(nrow=n,ncol=7)

table.in[,1]=c(1:n)

table.in[,2]=dhat

table.in[,3]=dhat-tmp$bias

table.in[,4]=tmp$bias

table.in[,5]=tmp$var

table.in[,6:7]=tmp$conf.int

write.table(table.in, file="output2021.txt")


####Output-oriented a la Farrell 

dhat=dea(XOBS=x,YOBS=y,RTS=1,ORIENTATION=2,errchk=TRUE)

tmp=boot.sw98(XOBS=x,YOBS=y, NREP=500,DHAT=dhat, RTS=1,ORIENTATION=2, alpha=0.05, CI.TYPE=2,OUTPUT.FARRELL = TRUE, errchk=TRUE)

n=ncol(x)

table.in=matrix(nrow=n,ncol=7)

table.in[,1]=c(1:n)

table.in[,2]=1/dhat

table.in[,3]=1/dhat-tmp$bias

table.in[,4]=tmp$bias

table.in[,5]=tmp$var

table.in[,6:7]=tmp$conf.int

write.table(table.in, file="outputf2021.txt")






