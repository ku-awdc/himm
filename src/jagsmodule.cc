#include <module/Module.h>
#include <function/DFunction.h>
#include <function/PFunction.h>
#include <function/QFunction.h>

#include "DHimm.h"

using std::vector;

namespace jags {
namespace himm {

	class HimmModule : public Module {
	  public:
	    HimmModule();
	    ~HimmModule();

		void Rinsert(RScalarDist *dist);
	};

HimmModule::HimmModule() : Module("himm")
{
  // For functions or scalar/vector distributions:
  insert(new DHimm);

  // For distributions using d/p/q/r:
  // Rinsert(new DLom);
}

void HimmModule::Rinsert(RScalarDist *dist)
{
	insert(dist);
	insert(new DFunction(dist));
	insert(new PFunction(dist));
	insert(new QFunction(dist));
}

HimmModule::~HimmModule()
{
  vector<Function*> const &fvec = functions();
  for (unsigned int i = 0; i < fvec.size(); ++i) {
    delete fvec[i];
  }
  vector<Distribution*> const &dvec = distributions();
  for (unsigned int i = 0; i < dvec.size(); ++i) {
    delete dvec[i];
  }
}

}  // namespace himm
}  // namespace jags

jags::himm::HimmModule _himm_module;
