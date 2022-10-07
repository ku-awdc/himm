#ifndef POINTER_H_
#define POINTER_H_

class Himm;

int add_pointer(Himm* pointer);
void remove_pointer(size_t pt_index);
void show_pointer(size_t pt_index);
bool verify_index(const size_t pt_index);
Himm* get_pointer(const size_t pt_index);

// Can't include this here:
// #include <Rcpp.h>
// Rcpp::LogicalVector active_index();

#endif // POINTER_H_
