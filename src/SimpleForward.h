#include <Rcpp.h>
#include <array>
#include <math.h>

#include "Himm.h"

class SimpleForward : public Himm
{
  private:
    std::vector<bool> m_data;
    std::vector<double> m_seprob;
    std::vector<double> m_spprob;
    /*
    std::array<bool, 200L> m_data;
    std::array<double, 200L> m_seprob;
    std::array<double, 200L> m_spprob;
    */

    double m_p1 = 0.1;
    double m_beta_const = 0.1;
    double m_gamma = 0.1;

    double m_se = -1.0;
    double m_sp = -1.0;

    const size_t m_nP;
    const size_t m_nT;
    double m_logdens = 0.0;

  public:
    SimpleForward(const int nP, const int nT) :
      m_nP(nP), m_nT(nT)
    {
      m_data.resize(m_nP*m_nT);
      m_seprob.resize(m_nP*m_nT);
      m_spprob.resize(m_nP*m_nT);
    }

    void addData(Rcpp::IntegerMatrix data)
    {
      if(data.ncol()!=m_nT) Rcpp::stop("Wrong col dim");
      if(data.nrow()!=m_nP) Rcpp::stop("Wrong row dim");

      for(int i=0L; i<m_nP; ++i)
      {
        for(int t=0L; t<m_nT; ++t)
        {
          m_data[m_nT*i + t] = data(i,t) == 1L;
        }
      }
    }

    void setRates(const std::vector<double> prv1, const std::vector<double> beta_const,
                  const std::vector<double> beta_freq, const std::vector<double> gamm)
    {
      if(beta_freq[0L]!=0.0)
      {
        Rcpp::Rcout << "Note: invalid non-zero beta_freq" << std::endl;
        Rcpp::stop("Invalid non-zero beta_freq");
      }

      m_p1 = prv1[0L];
      m_beta_const = beta_const[0L];
      m_gamma = gamm[0L];
    }

    void setTestPars(const std::vector<double> test_pars)
    {
      if(std::abs(m_se - test_pars[0L]) > 0.0001 || std::abs(m_sp - test_pars[1L]) > 0.0001)
      {
        m_se = test_pars[0L];
        m_sp = test_pars[1L];

        const double sp = std::log(m_sp);
        const double sp1m = log1m(m_sp);
        const double se = std::log(m_se);
        const double se1m = log1m(m_se);

        for(size_t i=0L; i<m_seprob.size(); ++i)
        {
          m_seprob[i] = m_data[i] ? se : se1m;
          m_spprob[i] = m_data[i] ? sp1m : sp;
        }

      }
    }

    double log1m(const double p)
    {
      return std::log1p(-p);
    }

    double log_sum_exp(const double u, const double v)
    {
      const double m = std::max(u, v);
      return m + std::log(std::exp(u - m) + std::exp(v - m));
    }

    void calculate()
    {

      const double p1 = std::log(m_p1);
      const double p1m = log1m(m_p1);
      const double be = std::log(m_beta_const);
      const double be1m = log1m(m_beta_const);
      const double ga = std::log(m_gamma);
      const double ga1m = log1m(m_gamma);

      m_logdens = 0.0;

      size_t i=0L;
      for(size_t p=0L; p<m_nP; ++p)
      {
        std::array<double, 2L> logalpha;
        logalpha[0L] = p1m + m_spprob[i];
        logalpha[1L] = p1 + m_seprob[i];

        for(size_t t=1L; t<m_nT; ++t)
        {
          i++;
          const std::array<double, 2L> lastlogalpha = logalpha;

          {
            const double acc0 = lastlogalpha[0L] + be1m + m_spprob[i];
            const double acc1 = lastlogalpha[1L] + ga + m_spprob[i];
            logalpha[0L] = log_sum_exp(acc0, acc1);
          }

          {
            const double acc0 = lastlogalpha[0L] + be + m_seprob[i];
            const double acc1 = lastlogalpha[1L] + ga1m + m_seprob[i];
            logalpha[1L] = log_sum_exp(acc0, acc1);
          }

        }
        i++;

        m_logdens += log_sum_exp(logalpha[0L], logalpha[1L]);
      }

      /*
      for(size_t p=0L; p<m_nP; ++p)
      {
        std::array<double, 2L> logalpha;
        logalpha[0L] = std::log(1.0-m_p1) + std::log(m_data[p*m_nT] ? (1.0 - m_sp) : m_sp);
        logalpha[1L] = std::log(m_p1) + std::log(m_data[p*m_nT] ? m_se : (1.0 - m_se));

        for(size_t t=1L; t<m_nT; ++t)
        {
          const std::array<double, 2L> lastlogalpha = logalpha;

          {
            std::array<double, 2L> accumulator;
            accumulator[0L] = lastlogalpha[0L] + std::log(1.0 - m_beta_const) + std::log(m_data[p*m_nT+t] ? (1.0 - m_sp) : m_sp);
            accumulator[1L] = lastlogalpha[1L] + std::log(m_gamma) + std::log(m_data[p*m_nT+t] ? (1.0 - m_sp) : m_sp);
            logalpha[0L] = std::log(std::exp(accumulator[0L]) + std::exp(accumulator[1L]));
          }

          {
            std::array<double, 2L> accumulator;
            accumulator[0L] = lastlogalpha[0L] + std::log(m_beta_const) + std::log(m_data[p*m_nT+t] ? m_se : (1.0 - m_se));
            accumulator[1L] = lastlogalpha[1L] + std::log(1.0 - m_gamma) + std::log(m_data[p*m_nT+t] ? m_se : (1.0 - m_se));
            logalpha[1L] = std::log(std::exp(accumulator[0L]) + std::exp(accumulator[1L]));
          }
        }

        m_logdens += std::log(std::exp(logalpha[0L]) + std::exp(logalpha[1L]));
      }
      */

      /*
  for(a in 1:Nani){
    # Starting negative:
    logalpha[a,1,1] <- log(1 - p1) + log(Obs[a,1]*(1-sp) + (1-Obs[a,1])*sp)
    # Starting positive:
    logalpha[a,1,2] <- log(p1) + log(Obs[a,1]*se + (1-Obs[a,1])*(1-se))

    # Define arbitrarily:
    accumulator[a,1,1] <- 0
    accumulator[a,1,2] <- 0

    for(t in 2:Ntime){

      # Status negative to negative:
      accumulator_n[a,t,1] <- logalpha[a,t-1,1] + log(1 - beta) + log(Obs[a,t]*(1-sp) + (1-Obs[a,t])*sp)
      # Status positive to negative:
      accumulator_n[a,t,2] <- logalpha[a,t-1,2] + log(gamma) + log(Obs[a,t]*(1-sp) + (1-Obs[a,t])*sp)
      # Sum probabilities:
      logalpha[a,t,1] <- log(exp(accumulator_n[a,t,1]) + exp(accumulator_n[a,t,2]))

      # Status negative to positive:
      accumulator_p[a,t,1] <- logalpha[a,t-1,1] + log(beta) + log(Obs[a,t]*se + (1-Obs[a,t])*(1-se))
      # Status positive to positive:
      accumulator_p[a,t,2] <- logalpha[a,t-1,2] + log(1-gamma) + log(Obs[a,t]*se + (1-Obs[a,t])*(1-se))
      # Sum probabilities:
      logalpha[a,t,2] <- log(exp(accumulator_p[a,t,1]) + exp(accumulator_p[a,t,2]))

    }

    final_logalpha[a] <- -log(exp(logalpha[a,Ntime,1]) + exp(logalpha[a,Ntime,2]))
    Zeros[a] ~ dpois(final_logalpha[a])
  }

      */

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

    ~SimpleForward()
    {

    }

};
