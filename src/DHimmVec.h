#ifndef DHIMM_VEC_H_
#define DHIMM_VEC_H_

#include <distribution/VectorDist.h>

namespace jags {
namespace himm {

/**
 * @short Mixture of normal distribution
 * <pre>
 * x ~ dnorm(mu, tau, pi)
 * </pre>
 * The parameters mu, tau, pi are conforming vectors or arrays. Each
 * element corresponds to a component of the mixture. If all parameters
 * are vectors then component i is normal with mean mu[i], tau[i] and 
 * probability of selection pi[i]/sum(pi).
 *
 * Note that the parameter pi is redundant: the likelihood depends
 * only on pi/sum(pi).
 */
    class DNormMix : public VectorDist {
    public:
	DNormMix();
	
	double logDensity(double const *x, unsigned int length, PDFType type,
			  std::vector<double const *> const &parameters,
			  std::vector<unsigned int> const &lengths,
			  double const *lower, double const *upper) const;
	void randomSample(double *x, unsigned int length,
			  std::vector<double const *> const &parameters,
			  std::vector<unsigned int> const &lengths,
			  double const *lower, double const *upper, RNG *rng) 
	    const;
	void typicalValue(double *x, unsigned int length,
			  std::vector<double const *> const &parameters,
			  std::vector<unsigned int> const &lengths,
			  double const *lower, double const *upper) const;
	void support(double *lower, double *upper, unsigned int length,
		     std::vector<double const *> const &parameters,
		     std::vector<unsigned int> const &lengths) const;
	bool isSupportFixed(std::vector<bool> const &fixmask) const;
	bool checkParameterLength(std::vector<unsigned int> const &lengths)
	    const;
	bool checkParameterValue(std::vector<double const *> const &parameters,
				 std::vector<unsigned int> const &lengths) 
	    const;
	void typicalValue(double *x, std::vector<double const *> const &par,
			  std::vector<unsigned int> const &lengths)  const;
	unsigned int length(std::vector<unsigned int> const &parlengths) const;
    };

}}


#endif /* DHIMM_VEC_H_ */
