tif <-
function (ip, x) 
	{
	    i = iif(ip, x)
	    if (is.null(dim(i$f))) 
	        dim(i$f) = c(length(i$x), length(i$f))
	    f = apply(i$f, 1, sum)
	    r = list(x = i$x, f = f, ni = ncol(i$f))
	    return(r)
	}

