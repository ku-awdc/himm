#ifndef DLOM_H_
#define DLOM_H_

#include <distribution/RScalarDist.h>

namespace jags {
namespace himm {

/**
 * <pre>
 * x ~ dlom(alpha, sigma);
 * f(x|alpha,sigma) = alpha/sigma * (1 + x/sigma)^-(alpha + 1); x >= 0
	= alpha*sigma^alpha / (x + sigma)^(alpha+1)
 * </pre>
 * @short Lomax distribution
 */

class DLom : public RScalarDist {
 public:
  DLom();

  double d(double x, PDFType type, std::vector<double const *> const &par, bool give_log) const;
  double p(double q, std::vector<double const *> const &par, bool lower, bool give_log) const;
  double q(double p, std::vector<double const *> const &par, bool lower, bool log_p) const;
  double r(std::vector<double const *> const &par, RNG *rng) const;

  bool checkParameterValue(std::vector<double const *> const &par) const;
};

}  // namespace himm
}  // namespace jags

#endif /* DLOM_H_ */
