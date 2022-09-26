// Storage of pointers

#include <Rcpp.h>
#include <vector>

#include "Himm.h"
#include "pointer_storage.h"

std::vector<Himm*> himm_pointers;
std::vector<bool> himm_active;

void add_pointer(Himm* pointer)
{
  himm_pointers.push_back(pointer);
  himm_active.push_back(true);
}

bool verify_index(const int index)
{
  if(index < 1L) return false;
  if(index > himm_active.size()) return false;
  return himm_active[index-1L];
}

Rcpp::LogicalVector active_index()
{
  Rcpp::LogicalVector rv = Rcpp::as<Rcpp::LogicalVector>(himm_pointers);
  return rv;
}

Himm* get_pointer(const int index)
{
  if(!verify_index(index)) Rcpp::stop("Index inactive");
  return himm_pointers[index-1L];
}
