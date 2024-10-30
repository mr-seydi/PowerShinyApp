
# Load the data
library(readxl)
Angle_1 <- read_excel("~/Desktop/temp_file/Power_Results_Angle_data_1.xlsx")
Angle_2 <- read_excel("~/Desktop/temp_file/Power_Results_Angle_data_2.xlsx")
MF_1 <- read_excel("~/Desktop/temp_file/Power_Results_MF_1.xlsx")
MF_2 <- read_excel("~/Desktop/temp_file/Power_Results_MF_2.xlsx")

Angle_2_SameSS <- read_excel("/Users/more0056/Desktop/temp_file/Power_Results_Angle_2ss5.xlsx")

TWT_A_1 <- read_excel("/Users/more0056/Desktop/temp_file/Power_Results_Angle_1ss5_TWT.xlsx")
TWT_A_2 <- read_excel("/Users/more0056/Desktop/temp_file/Power_Results_Angle_2ss5_TWT.xlsx")

TWT_MF_1 <- read_excel("/Users/more0056/Desktop/temp_file/Power_MF_TWT_1.xlsx")
TWT_MF_2 <- read_excel("/Users/more0056/Desktop/temp_file/Power_MF_TWT_2.xlsx")

sample<-sample(1:1000,10,replace=FALSE)
par(mfrow=c(2,2))
matplot(0:100,TWT_MF_1[,sample],type="l",xlab="Time",ylab="Pvalue",main="MF, TWT, SS=13, SD=50, NFWHM=5",lwd=2,lty=1)
abline(h=0.05,col="red",lwd=2,lty=2)

matplot(0:100,TWT_MF_2[,sample],type="l",xlab="Time",ylab="Pvalue",main="MF, TWT, SS=13, SD=50, NFWHM=45",lwd=2,lty=1)
abline(h=0.05,col="red",lwd=2,lty=2)

sample<-sample(1:1000,10,replace=FALSE)
par(mfrow=c(2,1))
matplot(0:100,TWT_A_1[,sample],type="l",xlab="Time",ylab="Pvalue",main="Angle, TWT, SS=8, SD=15, NFWHM=5",lwd=2,lty=1)
abline(h=0.05,col="red",lwd=2,lty=2)
matplot(0:100,TWT_A_2[,sample],type="l",xlab="Time",ylab="Pvalue",main="Angle, TWT, SS=8, SD=15, NFWHM=45",lwd=2,lty=1)
abline(h=0.05,col="red",lwd=2,lty=2)




sample<-sample(1:1000,10,replace=FALSE)
par(mfrow=c(2,1))
matplot(0:100,Angle_1[,sample],type="l",xlab="Time",ylab="Pvalue",main="Angle, SPM, SS=21, SD=15, NFWHM=5",lwd=2,lty=1)
abline(h=0.05,col="red",lwd=2,lty=2)
matplot(0:100,Angle_2_SameSS[,sample],type="l",xlab="Time",ylab="Pvalue",main="Angle, SPM, SS=21, SD=15, NFWHM=45",lwd=2,lty=1)
abline(h=0.05,col="red",lwd=2,lty=2)







sample<-sample(1:100,5,replace=FALSE)

#split screen into 3 rows
par(mfrow=c(3,1))

matplot(0:100,Angle_1[,sample],type="l",xlab="Time",ylab="Pvalue",main="Angle",lwd=2,lty=1)
abline(h=0.05,col="red",lwd=2,lty=2)

matplot(0:100,Angle_2[,sample],type="l",xlab="Time",ylab="Pvalue",main="Angle",lwd=2,lty=1)
abline(h=0.05,col="red",lwd=2,lty=2)

matplot(0:100,Angle_2_SameSS[,sample],type="l",xlab="Time",ylab="Pvalue",main="Angle",lwd=2,lty=1)
abline(h=0.05,col="red",lwd=2,lty=2)


#end spliting
par(mfrow=c(1,1))

matplot(0:100,MF_1[,sample],type="l",xlab="Time",ylab="Pvalue",main="SPM, MF, SS=15, SD=50, NFWHM=5",lwd=2,lty=1)
abline(h=0.05,col="red",lwd=2,lty=2)
matplot(0:100,MF_2[,sample],type="l",xlab="Time",ylab="Pvalue",main="SPM, MF, SS=15, SD=50, NFWHM=45",lwd=2,lty=1)
abline(h=0.05,col="red",lwd=2,lty=2)


par(mfrow=c(3,2))

Angel_1_TF<-Angle_1<0.05

hist(apply(Angel_1_TF,2,sum),breaks=seq(0,100,by=1),col="blue")
sum(apply(Angel_1_TF,2,sum)>0)
plot(apply(Angel_1_TF,1,sum)/1000,col="blue",type="l")

Angel2_TF<-Angle_2<0.05

hist(apply(Angel2_TF,2,sum),breaks=seq(0,100,by=1),col="blue")
sum(apply(Angel2_TF,2,sum)>0)
plot(apply(Angel2_TF,1,sum)/1000,col="blue",type="l")

Angel2_TF_SS<-Angle_2_SameSS<0.05
hist(apply(Angel2_TF_SS,2,sum),breaks=seq(0,100,by=1),col="blue")
sum(apply(Angel2_TF_SS,2,sum)>0)
plot(apply(Angel2_TF_SS,1,sum)/1000,col="blue",type="l")


MF_1_TF<-MF_1<0.05

hist(apply(MF_1_TF,2,sum),breaks=seq(0,100,by=1),col="blue")
plot(apply(MF_1_TF,1,sum)/1000,col="blue",type="l")

MF_2_TF<-MF_2<0.05

hist(apply(MF_2_TF,2,sum),breaks=seq(0,100,by=1),col="blue")
plot(apply(MF_2_TF,1,sum)/1000,col="blue",type="l")
