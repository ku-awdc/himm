#include <Rcpp.h>

int test(const int par)
{
  return par+1L;
}


RCPP_MODULE(test){

 Rcpp::function("test", &test);

}
