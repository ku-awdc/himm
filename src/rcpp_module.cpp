#include <Rcpp.h>

#include "ForwardTemplate.h"
#include "HimmTemplate.h"
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

RCPP_MODULE(himm_module){

	using namespace Rcpp;
  
  function("active_index", &active_index, "Get vector of indexes");
  function("show_pointer", &show_pointer, "Show a pointer info");

//  using Himm_Nx10 = HimmTemplate<0L, 10L, 1024L>;
  
  using Himm_Nx5 = HimmTemplate<0L, 5L, 32L>;
  class_<Himm_Nx5>("Himm_Nx5")
    DISABLE_DEFAULT_CONSTRUCTOR()
    .constructor<int, int>("Constructor with 2 arguments")
    .method("show", &Himm_Nx5::show, "The show method")
    .method("calculate_zi", &Himm_Nx5::calculateZi, "The show method")
    .method("addData", &Himm_Nx5::addData, "The show method")
    .method("calculate", &Himm_Nx5::calculate, "The show method")
    .method("test", &Himm_Nx5::test, "The show method")
    .method("obsprev", &Himm_Nx5::obsprev, "The show method")
    .method("getZis", &Himm_Nx5::getZis, "The show method")      
    .method("getObsProbs", &Himm_Nx5::getObsProbs, "The show method")      
    .property("zs", &Himm_Nx5::getZs, "Get z matrix")
    .property("log_density", &Himm_Nx5::logDensity, "Get z matrix")
    .property("pointer_index", &Himm_Nx5::getIndex, "Get z matrix")
    //    .property("states", &Simulation::GetStates, "Get the total for each state")
    ;

}

