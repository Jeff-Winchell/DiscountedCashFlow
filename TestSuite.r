library(RUnit)
testsuite.DCF <- defineTestSuite("Discounted Cash Flow",
                                 dirs=getSrcDirectory(function(dummy) {dummy}),
                                 testFileRegexp = "^runit.+.r",
                                 testFuncRegexp = "^test.+",
                                 rngKind = "Marsaglia-Multicarry",
                                 rngNormalKind = "Kinderman-Ramage")
printTextProtocol(runTestSuite(testsuite.DCF))
