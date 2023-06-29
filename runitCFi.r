library(RUnit)
#Due to parameters being used in defineTestSuite in TestSuite.r, the test functions must start with the word test for the test suite to find them and the unit test R scripts with tests must start start with the word runit

#In reality, these two functions's bodies will call global variables which are saved in memory the first time the function is called during a session. The global variables likely will be populated from a file, which itself is updated on some prescribed frequency. Or the global variables will be initialized to get data from a direct connection to some website to screen scrape the number from. It is TBD
Get_BBB=function() return (.0329)
Get_Prime=function() return (.035)


test.Monthly_Payment.Normal <- function() {
  checkException(Monthly_Payment.Normal("xx"))
  checkEquals(Monthly_Payment.Normal(1000,.12,12), 88.85)
  checkTrue(system.time(Monthly_Payment.Normal(1000,.12,12))[[3]]<.01)
}
test.Monthly_Payment.Interest_only <- function() {
  checkException(Monthly_Payment.Interest_Only("xx"))
  checkEquals(Monthly_Payment.Interest_Only(1000,.12), 10)
  checkTrue(system.time(Monthly_Payment.Interest_Only(1000,.12))[[3]]<.01)
}

test.Cash_Flow_NPV=function() {
  checkException(Cash_Flow_NPV("xx"))
  checkEquals(Cash_Flow_NPV(10,1,.24,.085,1),Cash_Flow_NPV(10,0,.05,.24,1)) 
  checkEquals(Cash_Flow_NPV(10,1,.12,.085,1),9.9) 
  checkEquals(Cash_Flow_NPV(10,1,.24,.085,1),Cash_Flow_NPV(10,.42,.24,.24,1)) 
  checkEquals(zapsmall(Cash_Flow_NPV(10,1,.24,.085,2)),9.61) 
  checkTrue(system.time(Cash_Flow_NPV(10,1,.12,.085,1))[[3]]<.01)
} 

test.DCF=function() {
  checkException(DCF("xx"))
  checkException(DCF(-1000,Get_BBB(),12,.8,.06,.085,1)) #Loan too small should cause error
  checkException(DCF(-40000000,Get_BBB(),12,.8,.06,.085,1)) #Loan too big should cause error
  checkException(DCF(-10000,Get_BBB()-.011,12,.8,.06,.085,1)) #Interest rate too small should cause error
  checkException(DCF(-10000,Get_BBB()+.13,12,.8,.06,.085,1)) #Interest rate too large should cause error
  checkException(DCF(-10000,Get_BBB(),1,.8,.06,.085,1)) #Term length too small should cause error
  checkException(DCF(-10000,Get_BBB(),1188,.8,.06,.085,1)) #Term length too long should cause error
  checkException(DCF(-10000,Get_BBB(),12,.3,.06,.085,1)) #Debt Fund Ratio too small should cause error
  checkException(DCF(-10000,Get_BBB(),12,1,.06,.085,1)) #Debt Fund Ratio too large should cause error
  checkException(DCF(-10000,Get_BBB(),12,.8,Get_Prime(),.085,1)) #Cost of Deposits too small should cause error
  checkException(DCF(-10000,Get_BBB(),12,.8,Get_Prime()+.06,.085,1)) #Cost of Deposits too large should cause error
  checkException(DCF(-10000,Get_BBB(),12,.8,Get_Prime(),.04,1)) #Cost of Equity Capital too small should cause error
  checkException(DCF(-10000,Get_BBB(),12,.8,Get_Prime(),.2,1)) #Cost of Equity Capital too large should cause error
  checkEquals(DCF(-10000,.06,12,.85,.06,.085,12)[[3]],600)
  checkEquals(DCF(-10000,.06,12,.85,.06,.085,0)[[3]],327.96)
  checkEquals(DCF(-10000,c(.06,.06,.05,.05,.05,.06,.07,.06,.05,.07,.08,.05),12,.85,.06,.085,0)[[3]],314.58)
  checkTrue(system.time(DCF(-10000,.06,180,.85,.05,.085,1))[[3]]<.03)
}