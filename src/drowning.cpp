#include <Rcpp.h>
include "ndGrid.cpp";
using namespace Rcpp;
using namespace std;


double floodingLevel (NumericVector coords, const IntegerVector flooded, const NumericMatrix waterLevel, NumericVector gridStep,IntegerVector d_){
	int position;
	double floodLevel;
	position = coord2idx(coords,gridStep, d_);
	if (is_true(any(position == flooded))){

		//how are supposed to be stored the waterlevels from previous run?

		//TRIVIAL!!!!! WARNING!!!!
		floodLevel=1;
	}


	return floodLevel;

}

//prepare the grid of values




//find the minimum
void valley(NumericVector meanParam, Function floodingLevel, NumericVector lowerParam, NumericVector upperParam,NumericVector gridStep,IntegerVector d_,IntegerVector & flooded ){
	NumericVector giudecca;
	List giudeccaL;
	giudeccaL=abcNVm(meanParam,
	floodingLevel,
	    lowerParam,
	    upperParam,
	    //int FoodNumber   =
	    20,
	    //int limit        =
	    100,
	    //int maxCycle     =
	    1000,
	    //int criter       =
	    50,
	    //NumericVector
	    gridStep,
	    //NumericVector
	    d_ // , double tol=1e-10
	);
	giudecca=as<NumericVector>(giudeccaL["globalParam"]);
	//include minimum in the grid

	IntegerVector giudIdx;
	giudIdx=coord2idx(giudecca,gridStep,d_);
	//flooding the bottom
	IntegerVector flooding=IntegerVector::create();
	NumericVector groundLevel=NumericVector::create();

	flooding.insert(giudIdx);
	groundLevel.begin(as<double>(giudeccaL["value"]));

	waterShot(flooded,flooding,gridStep,d_,floodingLevel);

	while(flooding[0]>flooding[1])
		waterShot(flooded,flooding,gridStep,d_,floodingLevel);
	flooding.erase(0);
	groundLevel.erase(0);
	double hyperLiter=0;
	for(int i=1; i<groundLevel.size(); i++)
		hyperLiter += (groundLevel[0]-groundLevel[i]);
	//print(hyperLiter);

}

//starting loop
NumericVector waterShot(const IntegerVector flooded , IntegerVector & flooding, NumericVector gridStep, IntegerVector d_, Function model ){
	//look for neighs
	NumericVector neigHeight;
	NumericVector neighs;
	neighs=getNeighbours(flooded[0], d_);
	//check neighs for flooded
	for (int i =(neighs.size()-1);i==0;i--){
		if(is_true(any(neighs[i]==flooding))){
			neighs.erase(i);
		} else {
			if(is_true(any(neighs[i]==flooded)){
				neigHeight.pushback(model(idx2coord(neighs[i],d_,gridStep)));
			}
	}
	double candidate= neigHeight[0];
	int lowNeigh = 0;

	for (int i=1; i<neigHeight;i++){
		if(neigHeight[i]<candidate)
			lowNeigh =i;
	}
	//flood lowNeigh

	flooding.insert(lowNeigh);
	groundLevel.insert(candidate);

}




//minimum of neighs

//waterfill

//if valley filled, mmove to