#annual yields standard
  #adapt to any frequency (daily, weekly, monthly, quarterly)

#simple interest rate ==============================
#simple interest yield
simple_interest_yield_factor<- function(rate, time){
  yield<-(1+rate*time)
  return(yield)
}

#simple interest income
simple_interest_income<-function( Investment, rate, time){
  yield<- simple_interest_yield_factor(rate, time)
  Income <- yield*Investment
  return(Income)
}

#fv

#simple interest discount
simple_interest_discount_factor<- function(rate, time){
  discount<-1/(1+rate*time)
  return(discount)
}

#simple interest price
simple_interest_price<-function( Investment, rate, time){
  discount<- simple_interest_discount_factor(rate, time)
  Price <- discount*Investment
  return(Price)
}


#compound interest ==================================

#effective rate
effective_rate<-function(nominal, interval = "annually"){
  
  if(interval == "continuous"){
    r_ef <- exp(nominal)-1
    return(r_ef)
  }
  
  m_df<-data.frame(interval=c("annually","semi-annually", "quarterly", "monthly"),
                   m=c(1,2,4,12))
  m<-m_df$m[m_df$interval==interval]
  r_ef<-(1+nominal/m)^(m)-1
  
  return(r_ef)
}

#inflation 
real_rate<-function(nominal, inflation = 0){
  real_rate<-(nominal - inflation)/(1+inflation)
  return(real_rate)
}

#yield factor
compound_interest_yield_factor<-function(rate, time, interval = "annually", inflation = 0){
  
  rate_ef<-effective_rate(rate, interval)
  rate_real<-real_rate(rate_ef, inflation)
  
  yield<-(1+rate)^(time)
  return(yield)
}

#compound interest income
compound_interest_income<-function( Investment, rate, time, interval = "annually", inflation = 0){
  
  yield<-compound_interest_yield_factor(rate, time, interval, inflation)
  Income<- yield*Investment
  return(Income)
}

#Future Value
FV_compound_interest <- function(investment, rate, interval = "annually", inflation = 0, freq = "annually"){
  
  time_window<-length(investment)
  future_values<-c()
  
  k_df<-data.frame(freq=c("annually","semi-annually", "quarterly", "monthly"),
                     k=c(1,2,4,12))
  k<-k_df$k[k_df$freq==freq]
  
  for(i in 1:time_window){
    t_k<-(time_window-i)/k
    future_values[i]<-compound_interest_income(investment[i], rate, t_k, interval, inflation )
  }
  
  Future_Value<-sum(future_values)
  
  #  return(list(fv,FV))
  return(Future_Value)
}


#Discount factor
compound_interest_discount_factor<- function( rate, time, interval = "annually", inflation = 0){
  
  rate_ef<-effective_rate(rate, interval)
  rate_real<-real_rate(rate_ef, inflation)
  
  discount<-1/((1+rate_real)^(time))
  
  return(discount)
  
}

#present price
compound_interest_price<-function(Investment, rate, time, interval = "annually" , inflation = 0){
  discount<-compound_interest_discount_factor(rate, time, interval, inflation )
  Price<-discount*Investment
  return(Price)
}

#Present Value
PV_compound_interest <- function(cashflow, rate, interval = "annually", inflation = 0, freq = "annually"){
  
  time_window<-length(cashflow)
  present_values<-c()

  k_df<-data.frame(freq=c("annually","semi-annually", "quarterly", "monthly"),
                   k=c(1,2,4,12))
  k<-k_df$k[k_df$freq==freq]
  
  for(i in 1:time_window){
    t_k<-(i-1)/k
    present_values[i]<-compound_interest_price(cashflow[i], rate, t_k, interval, inflation)
  }
  
  Present_Value<-sum(present_values)
  
  #  return(list(pv,PV))
  return(Present_Value)
}

#Periodic Cash Flow
PV_periodic_cashflow<-function(cashflow, rate, interval = "annually", inflation = 0, freq = "annually"){
  
  rate_ef<-effective_rate(rate, freq)
  real_rate<-inflation_rate(rate, inflation)
  
  pv_cycle<-PV_compound_interest(cashflow, rate, interval, inflation, freq)
  
  period<-length(cashflow)
  
  #implement interval
  #vary start point
  PV_total<-pv_cycle/(1 - (1/(1+real_rate)^(period-1)))
  
  #return(list(pv_cycle, PV_total,l))
  return(PV_total)
}

#periodic cashflow 
cycle_cashflow<-function(PresentValue, rate, period){
  
  cash_flow<-PresentValue*(1 - (1/(1+rate)^(period)))
  return(cash_flow)
}


#irr - Newton Method 
IRR_Newton_Method<-function(cash_flow){
  
  d_time_window<-length(cash_flow)-1
  d_cash_flow<-cash_flow[-1]*1:d_time_window 
  
  lambda_O<-1
  lambda_k<-0
  d_lambda<-1
  int<-0
  
  while(abs(d_lambda)>10^-10){
    lambda_k<-lambda_O - PV_compound_interest(cash_flow, 1/lambda_O-1) /PV_compound_interest(d_cash_flow, 1/lambda_O-1)
    d_lambda<-lambda_k-lambda_O
    lambda_O<-lambda_k
    int<-int+1
  }
  
  irrNM<-1/lambda_k - 1
  
  return(irrNM)
}
