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

  virtual double density() = 0;

  virtual void show()
  {
  }

  virtual ~Himm()
  {
    remove_pointer(pointer_index);
  }
};

#endif // HIMM_H_
