#include <rng/RNG.h>
#include <util/nainf.h>

#include <cmath>
#include <algorithm>

#include <JRmath.h>
//#include <R.h>

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
    : ScalarDist("dhimm", 6L, DIST_SPECIAL)
{}


bool DHimm::checkParameterValue (vector<double const *> const &parameters) const
{
  // Note: the C++ object is re-used between models, so pointer index may vary

  // TODO checks based on parameter values alone:

  return true;
}

double DHimm::logDensity(double x, PDFType type,
			 vector<double const *> const &parameters,
			 double const *lbound, double const *ubound) const
{
  // Set the pointer on first run:
  const int xint = static_cast<int>(x);
  if(m_pointer_index == 0L)
  {
    m_pointer_index = xint;
    printf("SETTING POINTER INDEX to %i\n", xint);
  }

  // Check the pointer hasn't changed:
  if(xint != m_pointer_index)
  {
    printf("CHANGED POINTER INDEX FROM %i To %i\n", m_pointer_index, xint);
    m_pointer_index = xint;
    // return JAGS_NAN;
  }
  // Check the pointer storage:
  const bool valid = verify_index(m_pointer_index);
  if(!valid)
  {
    printf("INVALID POINTER INDEX %i\n", m_pointer_index);
    return JAGS_NAN;
    // throw("Invalid pointer index (response value)");
  }
  // Get the pointer:
  Himm* himm = get_pointer(m_pointer_index);
  // Unless we have already reported who we are, do that:
  if(!m_reported)
  {
    himm->show();
    m_reported = true;
  }

  // Set up parameter vectors:
  const std::vector<double> prv1 = { *parameters[0L] };
  const std::vector<double> beta_const = { *parameters[1L] };
  const std::vector<double> beta_freq = { *parameters[2L] };
  const std::vector<double> gamm = { *parameters[3L] };
  const std::vector<double> test_pars = { *parameters[4L], *parameters[5L] };

  // Set rate parameters:
  himm->setRates(prv1, beta_const, beta_freq, gamm);
  // Set diagnostic test parameters:
  himm->setTestPars(test_pars);

  // Calculate log density etc:
  himm->calculate();

  // Get log density:
  const double dens = himm->logDensity();

  return dens;
  //return d == 0 ? JAGS_NEGINF : log(d);
}

double DHimm::randomSample(vector<double const *> const &parameters,
			   double const *lbound, double const *ubound,
			   RNG *rng) const
{
    return JAGS_NAN;
}

double DHimm::typicalValue(vector<double const *> const &parameters,
			   double const *lbound, double const *ubound) const
{
    return JAGS_NAN;
}

bool DHimm::isDiscreteValued(vector<bool> const &mask) const
{
    return true;
}

double DHimm::KL(vector<double const *> const &par0,
        vector<double const *> const &par1) const
{
	return JAGS_NAN;
}

}}
