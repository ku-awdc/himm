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

void remove_pointer(int index)
{
  if(!verify_index(index)) Rcpp::stop("Index inactive");
  himm_active[index-1] = false;
  himm_pointers[index-1] = 0;
}

void show_pointer(int index)
{
  if(!verify_index(index)) Rcpp::stop("Index inactive");
  himm_pointers[index-1]->show();
}

bool verify_index(const int index)
{
  if(index < 1L) return false;
  if(index > himm_active.size()) return false;
  return himm_active[index-1L];
}

Rcpp::LogicalVector active_index()
{
  Rcpp::LogicalVector rv = Rcpp::wrap(himm_active);
  return rv;
}

Himm* get_pointer(const int index)
{
  if(!verify_index(index)) Rcpp::stop("Index inactive");
  return himm_pointers[index-1L];
}
