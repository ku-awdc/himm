#include <Rcpp.h>

#include "HimmWrapper.h"
#include "pointer_storage.h"

template <class RcppModuleClassName>
RcppModuleClassName* invalidate_default_constructor() {
  Rcpp::stop("Default constructor is disabled for this class");
  return 0;
}
#define DISABLE_DEFAULT_CONSTRUCTOR() .factory(invalidate_default_constructor)

//using Himm_1x1 = HimmTemplate<1L, 1L>;
//RCPP_EXPOSED_CLASS(Himm_1x1)

// TODO: Rcpp derives class to save retyping below???

Rcpp::LogicalVector active_index();

RCPP_MODULE(himm){

	using namespace Rcpp;
  
  function("active_index", &active_index, "Get vector of indexes");
  function("show_pointer", &show_pointer, "Show a pointer info");

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

