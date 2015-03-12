###########################################################
#build a function to estimate lambdas
lambdaest.func<-function(lambda1,lambda2,lambda3,lambda4,n){
  x1<-rexp(n,rate=lambda1)
  x2<-rexp(n,rate=lambda2)
  x3<-rexp(n,rate=lambda3)
  x4<-rexp(n,rate=lambda4)

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
  }


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
#######################################################################
  Est.func<-function(lambdaI1,lambdaI2,lambdaI3,lambdaI4){
    #for j=1
    gama1<-r1+b12*lambdaI1/(lambdaI1+lambdaI2)+b13*lambdaI1/(lambdaI1+lambdaI3)+
           b14*lambdaI1/(lambdaI1+lambdaI4)+
           b123*lambdaI1/(lambdaI1+lambdaI2+lambdaI3)+
           b124*lambdaI1/(lambdaI1+lambdaI2+lambdaI4)+
           b134*lambdaI1/(lambdaI1+lambdaI3+lambdaI4)+
           b1234*lambdaI1/(lambdaI1+lambdaI2+lambdaI3+lambdaI4)
    #for j=2
    gama2<-r2+b12*lambdaI2/(lambdaI1+lambdaI2)+b23*lambdaI2/(lambdaI2+lambdaI3)+
           b24*lambdaI2/(lambdaI2+lambdaI4)+
           b123*lambdaI2/(lambdaI1+lambdaI2+lambdaI3)+
           b124*lambdaI2/(lambdaI1+lambdaI2+lambdaI4)+
           b234*lambdaI2/(lambdaI2+lambdaI3+lambdaI4)+
           b1234*lambdaI2/(lambdaI1+lambdaI2+lambdaI3+lambdaI4)
    #for j=3
    gama3<-r3+b13*lambdaI3/(lambdaI1+lambdaI3)+b23*lambdaI3/(lambdaI2+lambdaI3)+
           b34*lambdaI3/(lambdaI3+lambdaI4)+
           b123*lambdaI3/(lambdaI1+lambdaI2+lambdaI3)+
           b134*lambdaI3/(lambdaI1+lambdaI3+lambdaI4)+
           b234*lambdaI3/(lambdaI2+lambdaI3+lambdaI4)+
           b1234*lambdaI3/(lambdaI1+lambdaI2+lambdaI3+lambdaI4)
    #for j=4
    gama4<-r4+b14*lambdaI4/(lambdaI1+lambdaI4)+b24*lambdaI4/(lambdaI2+lambdaI4)+
           b34*lambdaI4/(lambdaI3+lambdaI4)+
           b124*lambdaI4/(lambdaI1+lambdaI2+lambdaI4)+
           b134*lambdaI4/(lambdaI1+lambdaI3+lambdaI4)+
           b234*lambdaI4/(lambdaI2+lambdaI3+lambdaI4)+
           b1234*lambdaI4/(lambdaI1+lambdaI2+lambdaI3+lambdaI4)

    lambdaNew1<-gama1/Y
    lambdaNew2<-gama2/Y
    lambdaNew3<-gama3/Y
    lambdaNew4<-gama4/Y

    res<-c(lambdaNew1,lambdaNew2,lambdaNew3,lambdaNew4)
    aaa=gama1+gama2+gama3+gama4
    print(aaa)
    print(gama1)
    return(res)
  }


  Atol<-0.000001
  Dis1<-1
  Dis2<-1
  Dis3<-1
  Dis4<-1
  s<-0
  lambda.pre<-c(1/2,1/2,1/2,1/2)

  while ((sum(Dis1>Atol)+sum(Dis2>Atol)+sum(Dis3>Atol)+sum(Dis4>Atol))>0){
    lambda.new<-Est.func(lambda.pre[1],lambda.pre[2],lambda.pre[3],lambda.pre[4])
    Dis1<-abs(lambda.new[1]-lambda.pre[1])
    Dis2<-abs(lambda.new[2]-lambda.pre[2])
    Dis3<-abs(lambda.new[3]-lambda.pre[3])
    Dis4<-abs(lambda.new[4]-lambda.pre[4])
    lambda.pre<-lambda.new
    s<-s+1
  }
  res<-c(lambda.new,s)
  return(res)
}


#############################################################
iter<-1000
rslt1<-matrix(nrow=iter,ncol=5,-1)
rslt2<-matrix(nrow=iter,ncol=5,-1)
rslt3<-matrix(nrow=iter,ncol=5,-1)
rslt4<-matrix(nrow=iter,ncol=5,-1)
lambda1<-1/4
lambda2<-1/7
lambda3<-1/5
lambda4<-1/6


n<-100
for (i in 1:iter){
  rslt1[i,]<-lambdaest.func(lambda1,lambda2,lambda3,lambda4,n)
}












