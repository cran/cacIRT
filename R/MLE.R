MLE <-
function(resp,ip)
	{
		np = nrow(resp)
		logf<-function (x, r, p) {
	    	pr = p[,3] + (1 - p[,3])/(1 + exp(-1.7*p[, 1] * (x-p[, 2])))
	    	ll = r * log(pr) + (1 - r) * log(1 - pr)
	    	lf = sum(ll)
	    	return(lf)}    
		esti<-function(x,resp,ip) optimize(logf, lower = -4, upper = 4, maximum = TRUE, r = resp, p = ip)$maximum
	
	    o = sapply(1:np, function(i) esti(resp = resp[i, ],ip=ip))
	    return((o))
	 }

