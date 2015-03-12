######################################################################
#R code
n<-100
lamda1<-1/4
lamda2<-1/7
lamda3<-1/5
lamda4<-1/6

x1<-rexp(n,rate=lamda1)
x2<-rexp(n,rate=lamda2)
x3<-rexp(n,rate=lamda3)
x4<-rexp(n,rate=lamda4)

Data<-matrix(nrow=n,ncol=4,0)
Data[,1]<-x1
Data[,2]<-x2
Data[,3]<-x3
Data[,4]<-x4

yi<-rep(-1,n)
ID<-rep("NA",n)

l<-2 #lower bound for masking
u<-10 #upper bound for cencoring

for (i in 1:n){
  if (sum(Data[i,]<=l)==0){
    yi[i]<-min(Data[i,])
    ID[i]<-order(Data[i,])[1]}
  if (sum(Data[i,]<=l)==1){
    yi[i]<-min(Data[i,])
    ID[i]<-order(Data[i,])[1]}
  if (sum(Data[i,]<=l)==2){
     e<-Data[i,Data[i,]<=l]
     yi[i]<-max(e)
     temid<-order(Data[i,])[c(1,2)]
     ID[i]<-toString(sort(temid))}
  if (sum(Data[i,]<=l)==3){
     e<-Data[i,Data[i,]<=l]
     yi[i]<-max(e)
     temid<-order(Data[i,])[c(1,2,3)]
     ID[i]<-toString(sort(temid))}     
  if (sum(Data[i,]<=l)==4){
     e<-Data[i,Data[i,]<=l]
     yi[i]<-max(e)
     ID[i]<-"1,2,3,4"}
  if (sum(Data[i,]>=u)==4){
     yi[i]<-u
     ID[i]<-"c"}
  print(i)
}

cbind(yi,ID)

####################################################################
#count the number of observations
c<-sum(ID=="c")

r1<-sum(ID=="1")
r2<-sum(ID=="2")
r3<-sum(ID=="3")
r4<-sum(ID=="4")

b12<-sum(ID=="1, 2")
b13<-sum(ID=="1, 3")
b14<-sum(ID=="1, 4")
b23<-sum(ID=="2, 3")
b24<-sum(ID=="2, 4")
b34<-sum(ID=="3, 4")

b123<-sum(ID=="1, 2, 3")
b124<-sum(ID=="1, 2, 4")
b134<-sum(ID=="1, 3, 4")
b234<-sum(ID=="2, 3, 4")

b1234<-sum(ID=="1,2,3,4")

sum(c,r1,r2,r3,r4,b12,b13,b14,b23,b24,b34,b123,b124,b134,b234,b1234)

Y<-sum(yi)


#########################################################################
flb <- function(lamda,b1,b2,b3,b4,b12,b13,b14,b23,b24,b34,b123,b124,b134,b234,b1234,c)
    {   
    	n=b1+b2+b3+b4+b12+b13+b14+b23+b24+b34+b123+b134+b234+b1234+c
    	y1=rexp(n,1/(lamda[1]+lamda[2]+lamda[3]+lamda[4]))
    	
    	b1*log(lamda[1])+ b2*log(lamda[2])+b3*log(lamda[3])+b4*log(lamda[4])+
    	b12*log(lamda[1]+lamda[2])+b13*log(lamda[1]+lamda[3])+b14*log(lamda[1]+lamda[4])+
    	b23*log(lamda[2]+lamda[3])+b24*log(lamda[2]+lamda[4])+b34*log(lamda[3]+lamda[4])+
    	b123*log(lamda[1]+lamda[2]+lamda[3])+b124*log(lamda[1]+lamda[2]+lamda[4])+
    	b134*log(lamda[1]+lamda[3]+lamda[4])+b234*log(lamda[2]+lamda[3]+lamda[4])+
    	b1234*log(lamda[1]+lamda[2]+lamda[3]+lamda[4])-(lamda[1]+lamda[2]+lamda[3]+lamda[4])*sum(y1)    	
    }

optim(c(0.1,0.1,0.1,0.1),flb, NULL, 
	b1=r1, b2=r2, b3=r3,b4=r4,b12=b12,b13=b13,b14=b14,b23=b23,
	b24=b24,b34=b34,b123=b123,b124=b124,b134=b134,b234=b234,b1234=b1234,c=c,
	method = "L-BFGS-B",upper=c(1,1,1,1),lower=c(0.001,0.001,0.001),control=list(fnscale=-1))




