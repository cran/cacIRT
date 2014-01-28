class.Rud <-
function( cutscore, ip, ability = NULL, se = NULL, rdm = NULL, quadrature=NULL, D = 1.7){
	 	
		if(is.null(quadrature)==TRUE){
		if(is.null(ability)==TRUE){
			
			os <- MLE(rdm, ip, D)
			se <- SEM(ip,os, D)} else{
			os <- ability
			se <- se}
	
		results <- Rud.P(cutscore, os, se)
		results
		
		} else {
			
			if(is.list(quadrature)==FALSE) stop("quadrature points and weights must be a list")	
			if(length(quadrature)!=2) stop("quadrature points and weights must be a list of legth 2")
			if(length(quadrature[[1]])!=length(quadrature[[2]])) stop("number of quadrature points and weights do not match")
			
		se<-SEM(ip,quadrature[[1]], D)							
		results <- Rud.D(cutscore, quadrature, se)				
	results}}

