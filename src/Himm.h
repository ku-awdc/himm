#ifndef HIMM_H_
#define HIMM_H_

#include "pointer_storage.h"
// Virtual base class for Himm

class Himm
{
protected:
  int pointer_index;
public:
  Himm()
  {
    pointer_index = add_pointer(this);
  }

  virtual double logDensity() = 0;

  virtual void show()
  {
  }

  /*
  int getIndex()
  {
    return pointer_index;
  }
  */
  
  virtual void setRates(const std::vector<const double> prv1, const std::vector<const double> beta_const,
                        const std::vector<const double> beta_freq, const std::vector<const double> gamm) = 0;
  
  virtual void setTestPars(const std::vector<const double> test_pars) = 0;
    
  virtual void calculate() = 0;

  virtual ~Himm()
  {
    remove_pointer(pointer_index);
  }
};

#endif // HIMM_H_
