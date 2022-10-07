// Storage of pointers

#include <Rcpp.h>
#include <vector>

#include "Himm.h"
#include "pointer_storage.h"

std::vector<Himm*> himm_pointers;
std::vector<bool> himm_active;

int add_pointer(Himm* pointer)
{
  himm_pointers.push_back(pointer);
  himm_active.push_back(true);
  return himm_active.size();
}

void remove_pointer(size_t pt_index)
{
  if(!verify_index(pt_index)) Rcpp::stop("Index inactive");
  himm_active[pt_index-1L] = false;
  himm_pointers[pt_index-1L] = 0L;
}

void show_pointer(size_t pt_index)
{
  if(!verify_index(pt_index)) Rcpp::stop("Index inactive");
  himm_pointers[pt_index-1L]->show();
}

bool verify_index(const size_t pt_index)
{
  if(pt_index < 1L) return false;
  if(pt_index > himm_active.size()) return false;
  return himm_active[pt_index-1L];
}

Rcpp::LogicalVector active_index()
{
  Rcpp::LogicalVector rv = Rcpp::wrap(himm_active);
  return rv;
}

Himm* get_pointer(const size_t pt_index)
{
  if(!verify_index(pt_index)) Rcpp::stop("Index inactive");
  return himm_pointers[pt_index-1L];
}
