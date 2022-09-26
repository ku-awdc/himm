#include <rng/RNG.h>
#include <util/nainf.h>

#include <cmath>
#include <algorithm>

#include <JRmath.h>

#include "DHimm.h"
#include "Himm.h"
#include "pointer_storage.h"

using std::vector;
using std::min;
using std::max;

/*
  Simplest example of Himm
  Parameters are:
    - prevalence at first time point
    - beta exogenous (constant infection rate)
    - beta density (density-dependent infection rate)
    - beta frequency (frequency-dependent infection rate)
    - gamma (recovery rate)
    - sensitivity
    - specificity
  Limitations (to be solved with vector version):
    - No risk factors at farm/animal level (vectorise betas and gamma)
    - Observation layer assumes dichotomous test (vectorise obs layer parameters to arbitrary length)
*/

#define PROB(par) (*par[0])

namespace jags {
namespace himm {

DHimm::DHimm()
    : ScalarDist("dhimm", 7L, DIST_POSITIVE)
{}

bool
DHimm::checkParameterValue (vector<double const *> const &parameters) const
{
  // Check that the external reference/id is still valid:
    return  (PROB(parameters) >= 0.0 && PROB(parameters) <= 1.0);
}

double DHimm::logDensity(double x, PDFType type,
			 vector<double const *> const &parameters,
			 double const *lbound, double const *ubound) const
{
  // Check the pointer storage:
  const int xint = 1L;
  const bool valid = verify_index(xint);
  Himm* himm = get_pointer(xint);

    double d = 0;
    if (x == 1)
	d = PROB(parameters);
    else if (x == 0)
	d = 1 - PROB(parameters);

    return d == 0 ? JAGS_NEGINF : log(d);
}

double DHimm::randomSample(vector<double const *> const &parameters,
			   double const *lbound, double const *ubound,
			   RNG *rng) const
{
    return rng->uniform() < PROB(parameters) ? 1 : 0;
}

double DHimm::typicalValue(vector<double const *> const &parameters,
			   double const *lbound, double const *ubound) const
{
    return PROB(parameters) > 0.5 ? 1 : 0;
}

bool DHimm::canBound() const
{
    return false;
}

bool DHimm::isDiscreteValued(vector<bool> const &mask) const
{
    return true;
}

    double DHimm::KL(vector<double const *> const &par0,
		     vector<double const *> const &par1) const
    {
	double p0 = PROB(par0);
	double p1 = PROB(par1);

	if (p0 == 0) {
	    return - log(1 - p1);
	}
	else if (p0 == 1) {
	    return - log(p1);
	}
	else {
	    return (p0 * (log(p0) - log(p1)) +
		    (1 - p0) * (log(1 - p0) - log(1 - p1)));
	}
    }

}}
