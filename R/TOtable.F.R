TOtable.F <-
function(theta,os,theta.cutoff,os.cutoff){
		t.cuts<-c(-Inf, theta.cutoff, Inf)
		o.cuts<-c(-Inf, os.cutoff, Inf)
		nc<-length(t.cuts+o.cuts)	
		N<-length(theta+os)
		contab<-matrix(NA,nc,nc)
			rownames(contab)<-c(paste("Truly <", round(t.cuts[(nc-1):1],2)),"margin")
			colnames(contab)<-c(paste("Obs <", round(o.cuts[(nc-1):1],2)),"margin")
	
		true.cat<-cut(theta,t.cuts,labels=FALSE)
		obs.cat<-cut(os,o.cuts,labels=FALSE)
	
			for(i in 1:(nc-1)) for(j in 1:(nc-1))
				contab[i,j]<-length((which(true.cat[obs.cat==j]==i)))
	
		contab[,nc]<-rowSums(contab,na.rm=TRUE)
		contab[nc,]<-colSums(contab,na.rm=TRUE)
	
		P<-sum(diag(contab))/N-1
	
		Pc<-sum(contab[,nc]/N*contab[nc,]/N)-1
		kappa<-(P-Pc)/(1-Pc)
	
		return(list(Table=contab,P=P)) }

