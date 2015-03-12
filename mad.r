sampleNormGenerate = function(sampleSize,mean,sigma,iterations){
y=matrix(rnorm(iterations*sampleSize,mean,sigma),iterations)
u=apply(y, 1, median)
MAD=apply(abs(y-u), 1, median)
IQR=apply(y, 1, IQR)

c1=sigma/MAD 	# c1.MAD->sigma as m->oo
c2=sigma/IQR		 # c2.IQR->sigma as m->oo
return (cbind(c1,c2))
}

iterations=100
sampleSize=100000
mean=100
sigma=5

x=sampleNormGenerate(sampleSize,mean,sigma,iterations)  # return value x consisting of c1 for MAD and c2 for IQR

t=1:iterations
pdf("mad.pdf")
plot(t,x[,1],"l",xlab="iterations times when sampleSize=100000",ylab="c for MAD",col="blue")
dev.off()
pdf("iqr.pdf")
plot(t,x[,2],"l",xlab="iterations times when sampleSize=100000",ylab="c for IQR",col="blue")
dev.off()