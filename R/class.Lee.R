class.Lee <-
function( cutscore, ip, ability = NULL, rdm = NULL, quadrature=NULL){
	 	
		if(is.null(quadrature)==TRUE){
		if(is.null(ability)==TRUE){
			
			theta <- MLE(rdm, ip)} else{
			theta <- ability}
	
		results <- Lee.P(cutscore, theta, ip)
		results
		
		} else {
			
			if(is.list(quadrature)==FALSE) stop("quadrature points and weights must be a list")	
			if(length(quadrature)!=2) stop("quadrature points and weights must be a list of legth 2")
			if(length(quadrature[[1]])!=length(quadrature[[2]])) stop("number of quadrature points and weights do not match")		
				
		results <- Lee.D(cutscore, quadrature, ip)		
		
	results}}

