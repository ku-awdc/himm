#include <Rcpp.h>
#include <array>

#include "Himm.h"

template<int T_nP, int T_nT, int T_2pT>
class HimmTemplate : public Himm
{
  private:
    std::array<double, T_2pT> m_comb_probs;
    std::vector<double> m_ind_probs;
    std::vector<std::array<int, T_nT>> m_data;
    std::array<std::array<int, T_nT>, T_2pT> m_zs;

    double m_p1 = 0.1;
    double m_beta_const = 0.1;
    double m_beta_freq = 0.1;
    double m_gamma = 0.1;

    double m_se = 0.99;
    double m_sp = 0.99;

    const int m_nP;
    double m_logdens = 0.0;

  public:
    HimmTemplate(const int nP, const int nT) :
      m_nP(nP)
    {
      // TODO: allow arbitrary nT with vector rather than array at some point
      if(nT != T_nT) Rcpp::stop("Non-matching nT and T_nT");
      if(std::pow(2L, nT) != T_2pT) Rcpp::stop("Non-matching 2^nT and T_nT");

      for(int i=0; i<T_2pT; ++i)
      {
        m_zs[i] = binarise(i);
      }

      m_data.resize(nP);
    }

    std::array<int, T_nT> binarise(int num)
    {
      std::array<int, T_nT> rv;
      for(int i=0; i<rv.size(); ++i)
      {
        rv[i] = 0L;
      }

      int pos = T_nT-1L;
      while(num != 0L)
      {
        rv[pos] = num % 2L;
        num /= 2L;
        pos--;
      }

      return rv;
    }

    Rcpp::IntegerMatrix getZs() const
    {
      Rcpp::IntegerMatrix rv(T_2pT, T_nT);
      for(int i=0L; i<T_2pT; ++i)
      {
        for(int j=0L; j<T_nT; ++j)
        {
          rv(i,j) = m_zs[i][j];
        }
      }
      return rv;
    }

    double dbern(const int y, const double prob)
    {
      return y==0L ? log(1.0-prob) : log(prob);
    }

    double obsFun(const int zi, const int yi)
    {
      const std::array<int, T_nT>& ys = m_data[yi];
      const std::array<int, T_nT>& zs = m_zs[zi];

      double ll = 0.0;
      for(int i=0; i<T_nT; ++i)
      {
        ll += dbern(ys[i], zs[i]*m_se + (1L-zs[i])*(1.0-m_sp));
      }

      return exp(ll);
    }

    double calculateZi(int zi)
    {
      if(zi < 0L || zi >= T_2pT) Rcpp::stop("zi out of range");
      const std::array<int, T_nT>& zs = m_zs[zi];
      // std::array<double, T_nT> pa;

      // pa[0L] = m_p1;
      double za = (zs[0L]==0L) ? (1.0-m_p1) : m_p1;
      double tp = za;

      for(int t=1L; t<T_nT; ++t)
      {
        // zp = zs[t-1L] * (1.0 - (std::pow(1.0-m_beta_freq, pa[t-1L])) + (1L-zs[t-1L])*(1.0-m_gamma);
        const double zp = (zs[t-1L]==0L) ? m_beta_const : (1.0-m_gamma);
        // pa[t] = zp;

        za = (zs[t]==0L) ? (1.0-zp) : zp;
        tp *= za;
      }

      return tp;
    }

    void show()
    {
      //Rcpp::Rcout << "hello from Himm nr " << pointer_index << std::endl;
      printf("hello from Himm\n");

    }

    int getIndex()
    {
      return pointer_index;
    }

    double logDensity()
    {
      return m_logdens;
    }

    double test(const double p1)
    {
      m_se = 0.9;
      m_sp = 0.99;
      m_beta_const = 0.05;
      m_gamma = 0.08;
      m_p1 = p1;

      calculate();

      return logDensity();

    }

    void setRates(const std::vector<double> prv1, const std::vector<double> beta_const,
                  const std::vector<double> beta_freq, const std::vector<double> gamm)
    {
      // Ignore beta_freq for now
      m_p1 = prv1[0L];
      m_beta_const = beta_const[0L];
      m_beta_freq = beta_freq[0L];
      m_gamma = gamm[0L];
    }

    void setTestPars(const std::vector<double> test_pars)
    {
      m_se = test_pars[0L];
      m_sp = test_pars[1L];
    }

    double obsprev(int tp)
    {
      double tot=0.0;
      for(int i=0L; i<m_nP; ++i)
      {
        tot += m_data[i][tp-1L];
      }

      return tot/m_nP;
    }

    Rcpp::NumericVector getZis()
    {
      std::array<double, T_2pT> zis;
      for(int z=0L; z<T_2pT; ++z)
      {
        zis[z] = calculateZi(z);
      }

      Rcpp::NumericVector rv = Rcpp::wrap(zis);

      return rv;
    }

    Rcpp::NumericMatrix getObsProbs()
    {
      Rcpp::NumericMatrix rv(T_2pT, m_nP);

      for(int i=0L; i<m_nP; ++i)
      {
        for(int z=0L; z<T_2pT; ++z)
        {
          rv(z,i) = obsFun(z,i);
        }
      }

      return rv;
    }

    void calculate()
    {
      std::array<double, T_2pT> zis;
      for(int z=0L; z<T_2pT; ++z)
      {
        zis[z] = calculateZi(z);
      }

      double total=0.0;
      for(int i=0L; i<m_nP; ++i)
      {
        double itotal = 0.0;
        for(int z=0L; z<T_2pT; ++z)
        {
          itotal += (zis[z] * obsFun(z, i));
        }
        total += log(itotal);
      }

      m_logdens = total;
    }

    void addData(Rcpp::IntegerMatrix data)
    {
      if(data.ncol()!=T_nT) Rcpp::stop("Wrong col dim");
      if(data.nrow()!=m_nP) Rcpp::stop("Wrong row dim");

      for(int i=0L; i<m_nP; ++i)
      {
        for(int t=0L; t<T_nT; ++t)
        {
          m_data[i][t] = data(i,t);
        }
      }
    }

    ~HimmTemplate()
    {

    }

};
