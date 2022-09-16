#include <Rcpp.h>

#include "HimmWrapper.h"

template <class RcppModuleClassName>
RcppModuleClassName* invalidate_default_constructor() {
  Rcpp::stop("Default constructor is disabled for this class");
  return 0;
}
#define DISABLE_DEFAULT_CONSTRUCTOR() .factory(invalidate_default_constructor)

//using Himm_1x1 = HimmTemplate<1L, 1L>;
//RCPP_EXPOSED_CLASS(Himm_1x1)

// TODO: Rcpp derives class to save retyping below???

RCPP_MODULE(himm){

	using namespace Rcpp;

  using Himm_NxN = HimmTemplate<0L, 0L>;
  class_<Himm_NxN>("Himm_NxN")
    DISABLE_DEFAULT_CONSTRUCTOR()
    .constructor<int, int>("Constructor with 2 arguments")
    .method("show", &Himm_NxN::show, "The show method")
    //    .property("states", &Simulation::GetStates, "Get the total for each state")
    ;

  class_<HimmWrapper>("HimmWrapper")
    .constructor("Constructor with no arguments")
    .method("show", &HimmWrapper::show, "The show method")
//    .property("states", &Simulation::GetStates, "Get the total for each state")
  ;

}

