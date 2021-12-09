getwd()
setwd("C:/Users/User/Documents/R")
source("InvestmentScience_Chapter2_functions.R")
rm(list=ls())
#########################
#Examples
#########################

#2.1
CF<-c(-2,1,1,1)
r<-0.10

FV_compound_interest(CF,r) #.648

#2.2
PV_compound_interest(CF,r) #.487

#2.3

irr<-IRR_Newton_Method(CF) #23.37%

#2.4

CF24a<-c(-1,2)
CF24b<-c(-1,0,3)

PV_compound_interest(CF24a, r) #.818
PV_compound_interest(CF24b, r) #1.479

#2.5

irr25a<-IRR_Newton_Method(CF24a) #100%
irr25b<-IRR_Newton_Method(CF24b) #73.2%

#2.6

CF26<-c(0, rep(2*10^6, 10))
PV_compound_interest(CF26, r)
#12.28 million

#2.7

CF27a<-c(20000, rep(1000,3))
CF27b<-c(30000, rep(2000,5))

#one way
PV27a<-PV_compound_interest(CF27a, r) #22.4k
PV27b<-PV_compound_interest(CF27b, r) #37.6k

CF27aM<-rep(c(PV27a,rep(0,3)),3)
CF27bM<-rep(c(PV27b,rep(0,5)),2)

PV27aM<-PV_compound_interest(CF27aM, r) #48k
PV27bM<-PV_compound_interest(CF27bM, r) #58k

#another way
CF27aM<-rep(CF27a,3)
CF27bM<-rep(CF27b,2)

PV27aM<-PV_compound_interest(CF27aM, r) #48k
PV27bM<-PV_compound_interest(CF27bM, r) #58k

#2.8

#buiding cahflows examples
CF28<- list()
PV28<- c()
for(i in 1:10){
  CF28[[i]] <- c(10, seq(2, i+1))
  PV28[i] <- PV_periodic_cashflow(CF28[[i]] , r)
}

which.min(PV28) #5, 64.5

#2.9
#deprecission =====
#example 2.9

A<- 10000
salvage<- 2000
income<-c(0,rep(3000,4))
t<-4
depreciation<-c(0,rep((A-salvage)/t, 4))
initial_income<-c(-A, income[-1])
initial_income[length(initial_income)]<-tail(initial_income,1)+salvage

tax_rate<-.43
tax_income<-income-depreciation
taxes<-tax_income*tax_rate

final_income<-initial_income-taxes

PV_compound_interest(initial_income, rate=.10) #875
PV_compound_interest(final_income, rate=.10) #-487

#2.10
CF10<-c(-10000, 5000, 5000, 5000, 3000)
f<-0.04

PV210<-PV_compound_interest(CF10, rate = r, inflation = f)
#5819



#########################
#Exercises
#########################

#1
A1<-1 
r1<-0.033
t1<-1998-1776 #year of the book

Income<-compound_interest_income(A1, r1, t1)
#1349

r1b<-2*r1
Income_b<-compound_interest_income(A1, r1b, t1)
#1452444

#2 
#demonstration


#3
r3a<-0.03
rate3a<-effective_rate(r3a, "monthly")#3.04%

r3b<-0.18
rate3b<-effective_rate(r3b, interval = "monthly") #19.56%
rate3c<-effective_rate(r3b, "quarterly") #19.25%

#4 

CF4<-c(-1, 1, 1)

irr4<-IRR_Newton_Method(CF4) #61.8%

#5
10000000/20
cash_flow_5<- rep(500000,20)
r5<-0.1

PV5<-PV_compound_interest(cash_flow_5,r)
#4.68 million


#6 

#6 months
CF61<-rep(-1000, 6)
CF62<-c(-1900,rep(-900, 5))

CF6<-CF62-CF61

r6<- .12

PV6new<-PV_compound_interest(CF6, r6, freq="monthly") #-413
PV61new<-PV_compound_interest(CF61, r6, freq="monthly")
PV62new<-PV_compound_interest(CF62, r6, freq="monthly")

#1 year

CF61b<-rep(-1000, 12)
CF62b<-c(-1900,rep(-900, 11))

CF6b<-CF62b-CF61b

PV6bnew<-PV_compound_interest(CF6b, r6, freq="monthly") #139
PV61bnew<-PV_compound_interest(CF61b, r6, freq="monthly")
PV62bnew<-PV_compound_interest(CF62b, r6, freq="monthly")

#add other frequencies in PV function

#7

#example 2.4
r<-0.1
CF7a<-c(-1,2)
CF7b<-c(-1,0,3)

PV7a<-PV_compound_interest(CF7a,r)
PV7b<-PV_compound_interest(CF7b,r)

compound_interest_income(PV7b+1, r, 3) #3.3


#8

CF8A<- -c(6000, rep(8000,4))
CF8B<- -c(30000, rep(2000,4))
CF8B[length(CF8B)]<-CF8B[length(CF8B)]+10000 #resale

IRR_Newton_Method(CF8B-CF8A)
#yes, it yields a return of 12.9%

#9

r9<-0.05

t9_old<-5
CF9_new<-c(20000, rep(0,20))

PV9_new<-PV_periodic_cashflow(CF9_new, r9) #32097

Price_old<-cycle_cashflow(PV9_new, r9, t9_old)
#6948

#10

Barrels<-c(80000, 70000, 50000, 30000, 10000)

Option2<-Barrels*5

GrossRevenue<-c(1600000, 1400000, 1000000, 600000, 200000)
Netincome<-c(1200000, 1000000, 500000, 200000, 50000)

Option1<-c()
for(i in 1:length(GrossRevenue)){
  Option1[i]<-min(GrossRevenue[i]*.22, Netincome[i]*.5)
}

Depletion<-c()
for(i in 1:length(GrossRevenue)){
  Depletion[i]<-max(Option1[i], Option2[i])
}

Tax_income<-Netincome-Depletion

Taxes<-Tax_income*.45

AT_income<-Netincome-Taxes

#a

Table2_7<-data.frame(Barrels, GrossRevenue, Netincome, Option1, Option2, Depletion, Tax_income, Taxes, AT_income)

#b
rate10<-.20
AT_cash_flow<-c(-10^6, AT_income)
PV10<-PV_compound_interest(AT_cash_flow, rate10)
#5.21 million

#IRR
irr10<-IRR_Newton_Method(AT_cash_flow)
#52.1 %

#11

Project1<-c(-100, rep(30,5))
Project2<-c(-150, rep(42,5))
rate11<-0.05

#irr

irrp1<-IRR_Newton_Method(Project1) #15.23
irrp2<-IRR_Newton_Method(Project2) #12.37
#irrp1

#PV
PVp1<-PV_compound_interest(Project1,rate11) #29.884
PVp2<-PV_compound_interest(Project2,rate11) #31.838
#PVp2



#12
30/100
42/150

#13
sum(Project1)
sum(Project2)


Project3<-Project2-Project1

irr3<-IRR_Newton_Method(Project3)
#6.4%

#14

A14<-1
Dep_rate1<-c(-1,.25,.38,.37)
Dep_rate2<-c(-1,1/3,1/3,1/3)

PV14<-list()

for(i in 1:50){
  PV14[[i]]<-c(PV_compound_interest(Dep_rate1, i/100),
               PV_compound_interest(Dep_rate2, i/100),
               PV_compound_interest(Dep_rate1-Dep_rate2, i/100)
               )
}

IRR_Newton_Method(Dep_rate1-Dep_rate2)


#15
A15<-10000000
GRevenue15<-c(rep(1000000*3.3,5))
labor15<-c(rep(10000*30, 5))
Raw_mat15<-c( rep(100*100,5))
Deprec15<-c()

for(i in 1:5){
  Deprec15[i]<-A15*(1/5)
}

tax_income15<-GRevenue15-labor15-Raw_mat15-Deprec15
taxes15<-tax_income15*.34

Netincome15<-GRevenue15-labor15-Raw_mat15

AT_income15<-Netincome15-taxes15

cash_flow_15<-c(-A15,AT_income15 )
rate15<-.12
PV_compound_interest(cash_flow_15, rate15)
#-435k

f15<-.04

PV15<-PV_compound_interest(cash_flow_15, rate15, inflation = f15)
#680731

#89000 (?)

