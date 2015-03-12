n =1000 # sample size
J=4
data = matrix(rep(0,n*J),n)
lamda=1/c(4,5,7,6)
iterations=100

r=array(0,dim=c(n,J,iterations))
lamdaHat=array(0,dim=c(iterations,J))
lamdaHat[1,]=1/c(4,5,7,6) # initial value

lowerBound = 2 #lower bound for masking
upperBound = 10 #upper bound for cencoring

data = mapply(rexp,n,lamda)

lowerM = apply(data,1, function(c) c[c<2] )
betweenM = apply(data,1, function(c) c[c>2 & c<10])
upperM = apply(data,1, function(c) c[c>10])

X = apply(data,1,function(c) {min(c)})

X1 = lapply(lowerM, function(c) {if (length(unlist(c)) %in% c(2,3,4)) max(c)})
X2 = lapply(betweenM, function(c) {if (length(unlist(c))==4) min(c)}) 
X3 = lapply(upperM, function(c) {if (length(unlist(c))==4) min(c)}) 

myConsistFunc = function(a,b) { if(!is.null(b)) a = b;a}
if(!is.null(unlist(X1))) 
    X = mapply(myConsistFunc,X,X1)
if(!is.null(unlist(X2))) 
    X = mapply(myConsistFunc,X,X2) 
if(!is.null(unlist(X3))) 
    X = mapply(myConsistFunc,X,X3) 

M = apply(data,1, function(c) {if(length(c[c<2])==1) order(c)[1]
else if(length(c[c<2]) == 2) order(c)[c(1,2)]
else if(length(c[c<2]) == 3) order(c)[c(1,2,3)]
else if(length(c[c<2]) == 4) order(c)[c(1,2,3,4)]
else if(length(c[c>10]) != 4) order(c)[1]
else -1
})

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





