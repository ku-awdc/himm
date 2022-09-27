#ifndef POINTER_H_
#define POINTER_H_

class Himm;

int add_pointer(Himm* pointer);
void remove_pointer(int index);
void show_pointer(int index);
bool verify_index(const int index);
Himm* get_pointer(const int index);

// Can't include this here:
// #include <Rcpp.h>
// Rcpp::LogicalVector active_index();

#endif // POINTER_H_
