SEM <-
function(ip,x)
	{
		ti<-tif(ip, x)$f	
   		sem <- sqrt(1/ti)
   		return(sem)
   	}

