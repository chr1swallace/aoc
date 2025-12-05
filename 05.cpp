#include "Rcpp.h"
using namespace Rcpp;

// [[Rcpp::export]]
int count(const double mn, const double mx, NumericMatrix fresh) {
  int nfresh=fresh.nrow();
  int c=0;
  double i=mn;
  while(i<=mx) {
    for(int j=0; j<nfresh; j++) {
      if(fresh(j,0) <= i && fresh(j,1) >= i) {
	c++;
	break;
      }
    }
    i=i+1.0;
  }
  return c;
}
