#annual yields standard
  #adapt to any frequency (daily, weekly, monthly, quarterly)

#inflation 
inflation_rate<-function(nominal, inflation){
  real_rate<-(nominal - inflation)/(1+inflation)
  return(real_rate)
}

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

#pv

#compound interest ==================================
#yield factor
compound_interest_yield_factor<-function(rate, time, interval){
  
  m<-c()
  
  #compound condition: annual, quarterly, etc. for effective rate
  
  if(interval == "annually"){m<-1}
  if(interval == "semi-annually"){m<-2}
  if(interval == "quarterly"){m<-4}
  if(interval == "monthly"){m<-12}
  
  yield<-(1+rate/m)^(time*m)

  if(interval == "continuous"){
    yield <- exp(time*rate)
    return(yield)
  }
  
  return(yield)
}

#compound interest income
compound_interest_income<-function( Investment, rate, time, interval = "annually" ){

  yield<-compound_interest_yield_factor(rate, time, interval)
  Income<- yield*Investment
  return(Income)

}

#Future Value
FV_compound_interest <- function(investment, rate, interval = "annually", inflation = 0){
  
  real_rate<-inflation_rate(rate, inflation)
  time_window<-length(investment)
  future_values<-c()
  
  for(i in 1:time_window){
    future_values[i]<-compound_interest_income(investment[i], real_rate, time_window-i, interval)
  }
  
  Future_Value<-sum(future_values)
  
  #  return(list(fv,FV))
  return(Future_Value)
}


#Discount factor
compound_interest_discount_factor<- function( rate, time, interval = "annually"){
  
  #m<-c()
  
  if(interval == "annually"){m<-1}
  if(interval == "semi-annually"){m<-2}
  if(interval == "quarterly"){m<-4}
  if(interval == "monthly"){m<-12}
  
  discount<-1/((1+rate/m)^(time*m))
  
  if(interval == "continuous"){
    discount<-1/(exp(time*rate))
    return(discount)
  }
  
  return(discount)
  
}

#present price
compound_interest_price<-function(Investment, rate, time, interval = "annually" ){
  discount<-compound_interest_discount_factor(rate, time, interval)
  Price<-discount*Investment
  return(Price)
}

#Present Value
PV_compound_interest <- function(cashflow, rate, interval = "annually", inflation = 0){
  
  real_rate<-inflation_rate(rate, inflation)
  time_window<-length(cashflow)
  present_values<-c()
  
  for(i in 1:time_window){
    present_values[i]<-compound_interest_price(cashflow[i], real_rate, i-1, interval)
  }
  
  Present_Value<-sum(present_values)
  
  #  return(list(pv,PV))
  return(Present_Value)
}

#Periodic Cash Flow
PV_periodic_cashflow<-function(cashflow, rate, interval, inflation = 0){
  
  real_rate<-inflation_rate(rate, inflation)
  pv_cycle<-PV_compound_interest(cashflow, real_rate, interval)
  
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

