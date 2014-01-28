gen.rec.raw <- function(Pij, theta){ 	
	
	ut <- theta
	nn <- length(ut) 
	
	if(length(dim(Pij))==2){
		ni <- dim(Pij)[2]
		sc <- ni+1
		
		Pjt <- Pij
	
			Pjt[round(Pjt,5)==1]<- .999999
			Pjt[round(Pjt,5)==0]<- .000001
	 	   	
		Qjt <-  1-Pjt
		H <- apply(Qjt,1,prod)
		Zjt	<- Pjt/Qjt

	zm <- array(0, dim = c(nn, sc, ni))	
		zm[,1,] <- 1
		zm[,2,1] <- Zjt[,1]

	for(i in 2: ni){
			for(s in 2:sc){
				zm[,s,i] <- zm[,s,i-1] + Zjt[,i]*zm[,s-1,i-1]}}	
	
		out<- zm[,,ni]*H
				rownames(out) <- paste("ex.x = ",round(ut,3))
				colnames(out) <-paste("x = " , 0:ni)
	return(out)
	}else{	
			
		nk <- dim(Pij)[2]	
		ni <- dim(Pij)[3]
			
		zm <- array(0, dim = c(nn, nk*ni, ni))	
		zm[,1:nk,1] <- Pij[,,1] 
			
			for(i in 2:ni){
				for(s in i:(i*nk)){
											
			zm[ , s, i] <- rowSums(
							as.matrix(	zm[,s-1:(min(c(s-1, nk))),i-1] * Pij[,1:(min(c(s-1, nk))),i]))
			
			}}			
			
				out<- zm[,ni:(nk*ni),ni]
				rownames(out) <- paste("theta = ",round(ut,3))
				colnames(out) <-paste("x = " , 0:(((nk-1)*ni)))
	return(out)}
	}
