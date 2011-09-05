Lee.rec.D <-
function(cutscore,quadrature,ip){
	
			ut<-quadrature[[1]]
			we <- quadrature[[2]]
			if(dim(ip)[2]==2)
				ip[,3]<-0 
			ni = dim(ip)[1]
			nn<-length(ut)
			sc<-ni+1
			
			exp.TS <- rowSums(sapply(1:ni, function(i) ip[i,3] + 
							(1 - ip[i,3])/(1 + exp(-1.7*ip[i, 1] * 
							(ut-ip[i, 2])))))   
	
	rec.mat <- recursive.raw(ut,ip)

			esacc<-matrix(NA,length(cutscore),nn, dimnames = list(paste("cut at",cutscore), round(ut,3)))
				escon<-esacc
			for(j in 1:length(cutscore)){
				cuts<-c(0, cutscore[j], sc)
	   			categ<-cut(exp.TS,cuts,labels=FALSE)
				bang<-ceiling(cuts)
			for(i in 1:nn){
			esacc[j,i]<-sum(rec.mat[i,(bang[categ[i]]+1):(bang[categ[i]+1])])
			escon[j,i]<-(sum(rec.mat[i,bang[1]:(bang[2])]))^2 + (sum(rec.mat[i,(bang[2]+1):(bang[3])]))^2}
			}
			
				ans<- (list("Marginal" = cbind("Accuracy" = apply(esacc,1,weighted.mean,we), "Consistency" = apply(escon,1,weighted.mean,we)), "Conditional" = list("Accuracy" =t(esacc), "Consistency" = t(escon))))
				
			
			#print(list("Marinal Accuracy" = round(apply(esacc,1,weighted.mean,we),4), "Marginal Consistency"=round(apply(escon,1,weighted.mean,we),4)))
 
 ans
	}

