#include <Rcpp.h>
#include "Himm.h"

template<int T_nP, int T_nT>
class HimmTemplate : public Himm
{
  private:


  public:
    HimmTemplate(const int nP, const int nT)
    {

    }

    void show()
    {
      Rcpp::Rcout << "hello from Himm nr " << pointer_index << std::endl;
    }
    
    double density()
    {
      return 0.0;
    }
    
    ~HimmTemplate()
    {
      
    }

};
