Rud.P <-
function(cutscore, os, sem) 
	 { 
	 	nn<-length(os)
	 	
	 	if(nn != length(sem)) stop("Ability and se of different length")
	
		esacc<-matrix(NA,length(cutscore), nn, dimnames = list(paste("cut at",cutscore), round(os,3)))
		escon <-esacc
		for(j in 1:length(cutscore)){
			cuts<-c(-Inf, cutscore[j], Inf)		 	
	 		categ<-cut(os,cuts,labels=FALSE,right=FALSE)
			
		for(i in 1:nn) {
			esacc[j,i]<-(pnorm(cuts[categ[i]+1],os[i],sem[i])-pnorm(cuts[categ[i]],os[i],sem[i]))
			escon[j,i]<-((pnorm(cuts[2], os[i],sem[i]) - pnorm(cuts[1],os[i],sem[i]))^2	+ (pnorm(cuts[3], os[i],sem[i]) - pnorm(cuts[2],os[i],sem[i]))^2	 )
			}}
		
		ans<- (list("Marginal" = cbind("Accuracy" = rowMeans(esacc), "Consistency" = rowMeans(escon)), "Conditional" = list("Accuracy" =t(esacc), "Consistency" = t(escon))))
				
			
			#print(list("Marinal Accuracy" = round(rowMeans(esacc),4), "Marginal Consistency"=round(rowMeans(escon),4)))
 
 ans
	}

