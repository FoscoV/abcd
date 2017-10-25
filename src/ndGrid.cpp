#include <Rcpp.h>
using namespace Rcpp;

//looking for neighs

// [[Rcpp::export]]
NumericVector getNeighbours (const int idx,const NumericVector d_ ) {
	// Dimension 0 done apart.
	// Neighbours proposed.
	NumericVector neighs;
	int c1 = idx-1, c2 = idx+1;
	// Checking neighbour 1.
	if ((c1 > 0) && (c1/d_[0] == idx/d_[0]))
	neighs.push_back(c1);
	// Checking neighbour 2.
	if (c2/d_[0] == idx/d_[0])
	neighs.push_back(c2);
	for (int i = 1; i < d_.size(); ++i) {
		// Neighbours proposed.
		c1 = idx-d_[i-1];
		c2 = idx+d_[i-1];
		// Checking neighbour 1.
		if ((c1 > 0) && (c1/d_[i] == idx/d_[i]))
		neighs.push_back(c1);
		// Checking neighbour 2.
		if (c2/d_[i] == idx/d_[i])
		neighs.push_back(c2);
	}
	return neighs;
}

//converting from idx to coord
// [[Rcpp::export]]
NumericVector idx2coord (int idx, NumericVector d_, NumericVector gridStep) {
	int ndims_ = gridStep.size();
	NumericVector coords(ndims_);
	coords[ndims_-1] = idx/d_[ndims_-2]; // First step done apart.
	int aux = idx - coords[ndims_-1]*d_[ndims_-2];
	for (int i = ndims_ - 2; i > 0; --i) {
		coords[i] = aux/d_[i-1];
		aux -= coords[i]*d_[i-1];
	}
	coords[0] = aux; //Last step done apart.

	return coords;
}

//converting from coords to idx

// [[Rcpp::export]]
int coord2idx (NumericVector coords,const NumericVector gridStep, int idx,const NumericVector d_) {
	int ndims_ = coords.size();
	for (int i=0;i<ndims_;i++)
		coords[i]=coords[i]/gridStep[i];
	idx = coords[0];
	for(int i = 1; i < ndims_; ++i)
		idx += coords[i]*d_[i-1];

	return idx;
}