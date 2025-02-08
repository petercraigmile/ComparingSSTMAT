
#include <math.h>

#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
NumericMatrix cppStar1_mu (NumericMatrix X, NumericMatrix omega) {

  int N = X.nrow();
  int q = X.ncol();
  int nlocs = omega.nrow();
  int t, j, k;
  double mu_t_j;

  NumericMatrix mu(N, nlocs);  

  for (j = 0; j < nlocs; j++) {
    for (t = 0; t < N; t++) {

      mu_t_j = 0.0;

      for (k = 0; k < q; k++) {

	mu_t_j += X(t, k) * omega(j, k);
      }

      mu(t, j) = mu_t_j;
      
    }
  }

  return mu;
}





// [[Rcpp::export]]
NumericMatrix cppStar1_U (NumericMatrix y, NumericVector phi) {

  int N = y.nrow();
  int nlocs = y.ncol();
  int t, j;
  double phi_j;

  NumericMatrix U(N-1, nlocs);  

  for (j = 0; j < nlocs; j++) {

    phi_j = phi[j];

    for (t = 1; t < N; t++) {
      
      U(t-1, j) = y(t, j) - phi_j * y(t-1, j);
    }
  }

  return U;
}



// [[Rcpp::export]]
double cppStar1_cond_loglik (NumericMatrix y, NumericVector phi,
			     NumericMatrix L, NumericMatrix SigmaInv) {

  int N = y.nrow();
  int nlocs = y.ncol();
  int t, j, k;
  double cll, log_det = 0.0, RSS = 0.0;
  NumericVector Ut(nlocs);

  for (j=0; j<nlocs; j++) {

    log_det += log(L(j,j));
  }

  for (t=1; t<N; t++) {

    for (j=0; j<nlocs; j++) {

      Ut[j] = y(t, j) - phi[j] * y(t-1, j);
    }

    for (j=0; j<nlocs; j++) {
      for (k=0; k<nlocs; k++) {

	RSS += Ut[j] * SigmaInv(j,k) * Ut[k];
      }
    }  
  }
  
  cll = -0.5*double(N-1) * (log_det + double(nlocs) *log(2.0 * M_PI)) -0.5 * RSS;  

  return cll;
}



/*

star1.cond.loglik <- function (y, phi, chol.Sigma) {

    ts <- 2:nrow(y)
    ty <- t(y)
   
    U <- t(ty[,ts] - phi * ty[,ts-1])
    
    sum(log.dmvnorm(U, sigma=chol.Sigma, chol=TRUE))
}

*/
