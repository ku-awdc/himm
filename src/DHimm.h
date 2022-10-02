#ifndef DHIMM_H_
#define DHIMM_H_

#include <distribution/RScalarDist.h>

namespace jags {
namespace himm {

/**
 * @short Bernoulli distribution
 * <pre>
 * R ~ dbern(p)
 * f(r | p) = p^r * (1 - p)^(1 -r) ; r in 0:1
 * </pre>
 */
class DHimm : public ScalarDist {
private:
  mutable bool m_reported = false;
  mutable int m_pointer_index = 0L;
public:
    DHimm();
    double logDensity(double x, PDFType type,
		      std::vector<double const *> const &parameters,
		      double const *lbound, double const *ubound) const;
    double randomSample(std::vector<double const *> const &parameters,
			double const *lbound, double const *ubound,
			RNG *rng) const;
    double typicalValue(std::vector<double const *> const &parameters,
			double const *lbound, double const *ubound) const;
      bool checkParameterValue(std::vector<double const *> const &parameters)
	const;
    bool isDiscreteValued(std::vector<bool> const &mask) const;
    double KL(std::vector<double const *> const &par1,
	      std::vector<double const *> const &par2) const;
};

}}

#endif /* DHIMM_H_ */
