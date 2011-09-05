class.Rud <-
function( cutscore, ip, irt.model = "dich", ability = NULL, se=NULL, rdm = NULL, quadrature=NULL){
	
	if(irt.model != "dich") stop("Code for non 3PL models not yet written")  	
		if(is.null(quadrature)==TRUE){
		if(is.null(ability)==TRUE){
			
			os <- MLE(rdm, ip)
			se <- SEM(ip,os)} else{
			os <- ability
			se <- se}
	
		results <- Rud.P(cutscore,os , se)
		results
		
		} else {
			
			if(is.list(quadrature)==FALSE) stop("quadrature points and weights must be a list")	
			if(length(quadrature)!=2) stop("quadrature points and weights must be a list of legth 2")
			if(length(quadrature[[1]])!=length(quadrature[[2]])) stop("number of quadrature points and weights do not match")
			
		se<-SEM(ip,quad[[1]])							
		results <- Rud.D(cutscore, quadrature, se)				
	results}}

