#ifndef POINTER_H_
#define POINTER_H_

#include "Himm.h"
#include <Rcpp.h>

void add_pointer(Himm* pointer);
bool verify_index(const int index);
Rcpp::LogicalVector active_index();
Himm* get_pointer(const int index);

#endif // POINTER_H_
