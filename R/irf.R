irf <-
function(ip,x){
	    ni = dim(ip)[1]
	    f = (sapply(1:ni, function(i) {ip[i, 3] + (1 - ip[i, 3])/(1 + exp(-1.7*ip[i, 1] * (x-ip[i, 2])))}))
	    r = list(x = x, f = f)
	    return(r)}

