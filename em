rm(list = setdiff(ls(), lsf.str()))

m=7
n=10

censoredData=c(1.613,1.644,1.663,1.732,1.740,1.763,1.778)
iteration=1:m
T1=0
T2=0
for (i in iteration){
	T1= T1+censoredData[i]
	T2= T2+censoredData[i]*censoredData[i]
}
sampleSize=100
u=0:(sampleSize-1)
sigma=1:sampleSize
iteration=1:(sampleSize-1)
iteration2=1:(n-m)
S1=1:(sampleSize-1)
S2=1:(sampleSize-1)

u[1]=mean(censoredData)
sigma[1]=sqrt(var(censoredData))

for(i in iteration){
	a=0
	for (j in iteration2){
		a=a+sigma[i]*dnorm((1.778-u[i])/sigma[i])/(1-pnorm((1.778-u[i])/sigma[i]))
	}
	S1[i]=(n-m)*u[i]+a
	u[i+1]=1/n*(T1+S1[i])
	b=0
	for (j in iteration2){
		b=b+(u[i]+1.778)*sigma[i]*dnorm((1.778-u[i])/sigma[i])/(1-pnorm((1.778-u[i])/sigma[i]))
	}
	S2[i]=(n-m)*(u[i]*u[i]+sigma[i]*sigma[i])+b
	sigma[i+1]=sqrt(1/n*(T2+S2[i])-1/n/n*(T1+S1[i])*(T1+S1[i]))
}



