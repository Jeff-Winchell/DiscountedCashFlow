library(RUnit)

#In reality, these two functions's bodies will call global variables which are saved in memory the first time the function is called during a session. The global variables likely will be populated from a file, which itself is updated on some prescribed frequency. Or the global variables will be initialized to get data from a direct connection to some website to screen scrape the number from. It is TBD
Get_BBB=function() return (.0329)
Get_Prime=function() return (.035)

Monthly_Payment.Normal = function(Loan_Balance,Interest_Rate,Remaining_Months) 
  return (round((Loan_Balance * Interest_Rate/12)/(1 - (1 + Interest_Rate/12)^(-Remaining_Months)),2))
Monthly_Payment.Interest_Only = function(Loan_Balance,Interest_Rate) 
  return (round(Loan_Balance*Interest_Rate/12,2))
Cash_Flow_NPV= function(Payment,Debt_Fund_Ratio,Cost_Of_Deposit2,Cost_Of_Equity_Capital,Month) 
    return (round(Payment/((1+(Debt_Fund_Ratio*Cost_Of_Deposit2+(1-Debt_Fund_Ratio)*Cost_Of_Equity_Capital)/12)^Month),2))

DCF=function(Loan,Interest_Rate,Term,
  Debt_Fund_Ratio,Cost_Of_Deposit,Cost_Of_Equity_Capital,
  Initial_Interest_Only_Periods=0) {
  
  # parameter checks
  checkTrue((Term>=12)&(Term<=180))
  checkEquals(Term%%1,0) #(no non-integer number of months for loan)
  Loan_Balance=-Loan #Spec uses negative numbers for intial
  checkTrue((Loan_Balance>=5000)&(Loan_Balance<=4000000))
  if (length(Interest_Rate)==1)
    Interest_Rate=rep(Interest_Rate,Term)

  checkTrue((max(Interest_Rate)<=Get_BBB()+.12)&(min(Interest_Rate)>=Get_BBB()-.01))
  checkTrue(min(Interest_Rate)>=0)
  
  if (length(Debt_Fund_Ratio)==1)
    Debt_Fund_Ratio=rep(Debt_Fund_Ratio,Term)
  checkTrue((min(Debt_Fund_Ratio)>=.76)&(max(Debt_Fund_Ratio)<=.92))
  if (length(Cost_Of_Deposit)==1)
    Cost_Of_Deposit=rep(Cost_Of_Deposit,Term)
  checkTrue((max(Cost_Of_Deposit)<=Get_Prime()+.0525)&(min(Cost_Of_Deposit)>=Get_Prime()+.0025))
  if (length(Cost_Of_Equity_Capital)==1)
    Cost_Of_Equity_Capital=rep(Cost_Of_Equity_Capital,Term)
  checkTrue((min(Cost_Of_Equity_Capital)>=.06)&(max(Cost_Of_Equity_Capital)<=.165))
  
  Payments=numeric(0)
  Interest_Charges=numeric(0)
  NPV=numeric(0)
  for (Month in seq_len(Term)) {
    Payments[Month]=ifelse(Month>Initial_Interest_Only_Periods,
                      Monthly_Payment.Normal(Loan_Balance,Interest_Rate[Month],Term-Month+1),
                      Monthly_Payment.Interest_Only(Loan_Balance,Interest_Rate[Month]))
    NPV[Month]=Cash_Flow_NPV(Payments[Month],Debt_Fund_Ratio[Month],Cost_Of_Deposit[Month],Cost_Of_Equity_Capital[Month],Month)
    Interest_Charges[Month]=round(Loan_Balance*Interest_Rate[Month]/12,2)
    Loan_Balance=round((Loan_Balance-(Payments[Month]-Interest_Charges[Month])),2)
    checkTrue(Loan_Balance>=0)
  }
  npv <- function(i, cf, t=seq(along=cf)-1) sum(cf/(1+i)^t) 
  irr <- function(cf) { uniroot(npv, c(0,1), cf=cf)$root }
  return (list(Payments,sum(NPV),irr(c(Loan,Payments)),sum(Interest_Charges)))
}