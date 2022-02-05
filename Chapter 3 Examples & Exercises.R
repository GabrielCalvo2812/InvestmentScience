getwd()
setwd("C:/Users/User/Documents/R")
source("InvestmentScience_Chapter2_functions.R")

source()

cf<-c(-714.57+95.85, rep(95.85, 11))

irr_st<-IRR_Newton_Method(cf)

(1+irr_st/12)^12


annuity_price<-function(income, rate, period = "perpetual"){
  
  if(period == "perpetual"){
    price <- income/rate
  }
  if(is.numeric(period)){
    price <- income*1/rate*(1 - 1/(1+rate)^period ) 
  }
  
  return(price)
}


annuity_income<-function(price, rate, period = "perpetual"){
  if(period == "perpetual"){
    income <- price*rate
  }
  if(is.numeric(period)){
    income <- ( price*rate*(1+rate)^period   )/((1+rate)^period - 1 )
  }
  
  return(income)
}

#############################
#examples
#############################

#3.1
A<-1000
r<-.1
annuity_price(A, r)

#3.2
loan<-1000
r<-1
time<-12*5
annuity_income(1000, .01, period = 60)

#adept the frequencies and interest rates

#3.3
loan<-203150
pts<-.01
monthly_payments<-annuity_income(203150, .07883/12, period = 30*12)
#1474.10 , ok
loan_adjusted<-annuity_price(monthly_payments, .07625/12, period = 30*12)
#208267.83 

fees<-loan*pts
expenses<- loan_adjusted-loan-fees

#3.4

annuity_income(100000, .16, 10)
25000-annuity_income(100000, .16, 10)
cf<-c(-100000, rep(25000, 10))
IRR_Newton_Method(cf)

PV_compound_interest(cf, .16)
annuity_income(PV_compound_interest(cf, .16), .16,10)

#3.5
#accrued interest

Accrued_interest<- 83/(83+99)*.09/2

#current yield
current_yield<-function(price, coupom_rate){
  c_yield<-coupom_rate/price
  return(c_yield)
}

#yield to call


#duration

duration <- function(cashflow, rate, interval = "annually", inflation = 0, freq = "annually"){
  
  time_window<-length(cashflow)
  present_values<-c()
  timed_pv<-c()
  
  k_df<-data.frame(freq=c("annually","semi-annually", "quarterly", "monthly"),
                   k=c(1,2,4,12))
  k<-k_df$k[k_df$freq==freq]
  
  for(i in 1:time_window){
    t_k<-(i-1)/k
    present_values[i]<-compound_interest_price(cashflow[i], rate, t_k, interval, inflation)
    timed_pv[i]<-present_values[i]*t_k
  }
  
  Duration<-sum(timed_pv)/sum(present_values)
  
  return(Duration)
}


#3.6 

c_rate36<-.07
y_rate36<-.08
years36<-3

CF36<-c(0,rep(c_rate36/2, years36*2))
CF36[length(CF36)]<-CF36[length(CF36)]+1

duration(CF36, y_rate36, interval = "semi-annually", freq="semi-annually")
PV_compound_interest(CF36, y_rate36, interval="semi-annually",  freq="semi-annually")

CF36irr<-CF36
CF36irr[1]<--  PV_compound_interest(CF36, y_rate36, interval="semi-annually",  freq="semi-annually")

IRR_Newton_Method(CF36irr)


#3.7

c_rate37<-.1
y_rate37<-.1
years37<-30

CF37<-c(0,rep(c_rate37/2, years37*2))
CF37[length(CF37)]<-CF37[length(CF37)]+1

duration(CF37, y_rate37, interval= "semi-annually",freq="semi-annually")

PV_compound_interest(CF37, y_rate37/2)
PV_compound_interest(CF37, y_rate37, interval = "semi-annually", freq = "semi-annually")


CF37irr<-CF37
CF37irr[1]<- -1
IRR_Newton_Method(CF37irr)

#maccalin without cashflow

macaulin_duration<-function(yield_rate, coupom_rate, time_horizon){
  
  mc_duration<-(1+yield_rate)/
  
}

(1+y_rate37)/(2*y_rate37) - (1+y_rate37+years37*2*(c_rate37-y_rate37))/(2*c_rate37*((1+y_rate37)^(years37*2)-1) + 2*y_rate37)

#3.7


#3.8


#3.9


#3.10

