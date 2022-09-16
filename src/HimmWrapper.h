#include <Rcpp.h>

/*
 * The purpose of this class is to hold vectors of
 * different types of templated class, and serve them
 * up to R/JAGS based on a requested index value
 *
 * Note: this class should be a singleton within the
 * R package (created on load/attach)
 */

#include "HimmTemplate.h"


class HimmWrapper
{
private:
  static int s_instance;


public:
  HimmWrapper()
  {

  }

  void show()
  {
    Rcpp::Rcout << "hello\n";
  }


};

int HimmWrapper::s_instance = 0L;
