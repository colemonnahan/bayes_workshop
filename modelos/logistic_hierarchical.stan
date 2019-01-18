
data {
 int R;				// number of sites
 int y[R];			// survivors
 int N[R];			// tagged animals
}

parameters {
 real mu;			// hypermean
 real<lower=0> sigma;		// hypersd
 vector[R] theta;		// random effect vector
 }	   

transformed parameters {
 vector[R] p;
 p=inv_logit(theta);
}

model {
 // the priors
 mu~normal(1,1);
 sigma~normal(0,1);
 // hyperdistribution
 theta~normal(mu,sigma);
 // likelihood
 y~binomial(N,p);
}

