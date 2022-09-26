#include "DLom.h"
#include <util/nainf.h>
#include <rng/RNG.h>

#include <cmath>
#include <cfloat>

using std::vector;
using std::exp;
using std::log;
using std::pow;

#define ALPHA(par) (*par[0])
#define SIGMA(par) (*par[1])

namespace jags {
namespace himm {

DLom::DLom()
    : RScalarDist("dlom", 2, DIST_POSITIVE)
{}
  
double DLom::d(double x, PDFType type, vector<double const *> const &par, bool give_log) const
{
  double alpha = ALPHA(par);
  double sigma = SIGMA(par);

  if (x < 0)
    return give_log ? JAGS_NEGINF : 0;

  if (give_log)
  	return (log(alpha) - log(sigma)) + ((alpha+1)*log(sigma) - (alpha+1)*log(x + sigma));
  else
    return alpha/sigma * pow((1 + x/sigma),-(alpha + 1));
  
}

double DLom::p(double x, vector<double const *> const &par, bool lower, bool give_log) const
{
  double alpha = ALPHA(par);
  double sigma = SIGMA(par);
  
  if (x < 0)
    return give_log ? JAGS_NEGINF : 0;

  double logq = -alpha * log(1 + x/sigma);
  if (!lower) {
    return give_log ? logq : exp(logq);
  }
  else {
    return give_log ? log(1 - exp(logq)) : 1 - exp(logq);
  }
}

double DLom::q(double p, vector<double const *> const &par, bool lower, bool log_p) const
{
    if ( (log_p  && p > 0) || (!log_p && (p < 0 || p > 1)) )          
	return JAGS_NAN;
    
    double logp;
	double x;

    if (!lower) {
	if (log_p)
	    logp = p;
	else
	    logp = log(p);
    }
    else {
	if (log_p)
	    logp = log(1 - exp(p)); 
	else
	    logp = log(1 - p);
    }
  	
	x = SIGMA(par) * (exp(logp/-ALPHA(par)) - 1);
	
  return x;
}

double DLom::r(vector<double const *> const &par, RNG *rng) const
{
    return q(rng->uniform(), par, false, false);
}

bool DLom::checkParameterValue (vector<double const *> const &par) const
{
  return (ALPHA(par) > 0 && SIGMA(par) > 0);
}

}  // namespace himm
}  // namespace jags
