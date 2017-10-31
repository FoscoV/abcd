cppFunction('
	NumericVector ritaglia(NumericVector nomi, NumericVector obiettivi){
		NumericVector listarella;
		for (int i{0}; i<= nomi.size(); i++){
			for ( int j{0}; j<=obiettivi.size();j++){
				if (nomi[i] == obiettivi[j]){
					listarella.push_back(i+1);
				}
			}
		}
		return listarella;
	}
')




gridABCD<-60
meanParam<-colMeans(desiKg)
d_<-meanParam
gridStep<-meanParam
for(parameter in 1:length(meanParam)){
	dimensioneDimensione<-ceiling((gridABCD/rowSums(sensiB[1,]))*sensiB[,parameter])+1
	if(parameter >=2){
		d_[parameter]<-dimensioneDimensione*d_[parameter-1]
	}else{
		d_[parameter]<-dimensioneDimensione
	}

	gridStep[parameter]<-(max(desiKg[,parameter])-min(desiKg[,parameter]))/dimensioneDimensione
}
d_<-as.integer(d_)


#here qe are going to calculate the valley extension...
valley<-function(meanParams,morphLevel,lowerParam,upperParam,gridStep,d_,flooded){
		floodingLevel<-function(idx){
			if(length(flooded[,1])>=2){
				water<-NULL
				if(any(idx==unlist(flooded[,3]))){
					for(vallata in length(flooded[,1])){
						if(any(unlist(idx==flooded[vallata,3]))){
							water<-max(c(water,flooded[vallata,2]))
						}
					}
				}else{
					water<-morphLevel(idx2coord(idx,d_,gridStep))
				}
			}else{
				water<-morphLevel(idx2coord(idx,d_,gridStep))
			}
			print(water)
			return(water)
		}

	#find the very bottom  point
	giudecca<-abcNVm(par=meanParams,fn=floodingLevel,lb=lowerParam,ub=upperParam)
	giudIdx<-as.integer(coord2idx(giudecca,gridStep,d_))
	flooding<-giudIdx
	waterLevel<-floodingLevel(flooding[1])

	#inizializing the chain by flooding giudecca
	surface<-waterLevel
	flow<-NULL
	candidate<-NULL
	candHei<-NULL




	while(waterLevel <= surface){
		waterLevel<-surface
		flooding<-as.integer(c(flow,flooding))
		neighs<-as.integer(getNeighbours(flooding[1],d_))
		#neighs<-neighs[-c(ritaglia(neighs,flooding))]
		neiGround<-neighs
		for (neG in 1:length(neiGround)){
			neiGround[neG]<-floodingLevel(neiGround[neG])
		}
		candidate<-c(candidate,neighs)
		candHei<-c(candHei,neiGround)
		flow<-candidate[which(candHei==min(candHei))]
		surface<-min(candHei)
	#drop in the whole valley if a valley is encountered.... (optimization available in near future!)
	}
	output<-data.frame(fondo=floodingLevel(giudIdx),quota=waterLevel,volume=list(flooding))
	return(output)
}


orografia<-data.frame(fondo=c(0),quota=c(0),volume=c(0))
for(pippo in 1:2){
	print(pippo)
	orografia<-rbind(orografia,
		valley(startHyp,krfunBra,minHyp,maxHyp,gridStep,d_,orografia))
}
