sim <-
function(ip,x){
	    i = irf(ip, x)
    	d = dim(i$f)
   		u = runif(d[1] * d[2])
    	dim(u) = d
    	return(ifelse(i$f > u, 1, 0))}

