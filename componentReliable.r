X=c(0.021,0.038,0.054,0.066,0.076,0.078,0.123,0.130,
	0.152,0.159,0.199,0.201,0.204,0.215,0.218,0.281,
	0.295,0.310,0.338,0.341,0.354,0.358,0.431,0.457,
	0.545,0.569,0.677,0.818,0.946,1.486)
M=list(2,1:2,3,3,1:2,2,3,3,1:3,1,3,1,1,3,1:2,1,2,3,3,2,1,2,1:3,3,1:3,2,3,2,2,1)
J=3
n=30
iterations=20

r=array(0,dim=c(n,J,iterations))
lamdaHat=array(0,dim=c(iterations,J))
lamdaHat[1,]=1/c(2,2,2) # initial value

for(s in 1:(iterations-1)){
    for(j in 1:J){

        for(i in 1:n){

            if(M[[i]][[1]] != -1){  
                sumOflamdaHat = 0
                for(k in 1:J){
                    if (k %in% unlist(M[i]))
                        sumOflamdaHat = sumOflamdaHat + lamdaHat[s,k] 
                }
                for(k in 1:J){
                    if (k %in% unlist(M[i]))
                        r[i,k,s] = lamdaHat[s,k]/sumOflamdaHat
                }
            }
        }

        lamdaHat[s+1,j]=sum(r[,j,s])/sum(X)
    } 
}