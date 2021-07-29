### ----- simulation -----

# flag to determine if to do a new simulation or not  
# TRUE: to use the existed data from the results file (default); FALSE: to simulate from the scratch 
simulationMode = FALSE

if(simulationMode){
  # to determine the flexible cost (flexible cost = flexibleK * fiexd cost)
  flexibleK = c(1.5, 3, 6)
  # the origianl data file used in the paper
  resultFilePath = 'results/0619'
  
  ## load records of costs of residual and neural gas tree under high and low penalty cost structures
  # neural gas trees of different size
  highNeuralCosts = readRDS(paste0(resultFilePath, '/', 'highNeuralCosts.rds')) # binary neural gas tree
  lowNeuralCosts = readRDS(paste0(resultFilePath, '/', 'lowNeuralCosts.rds')) # binary neural gas tree
  highSingleCost = readRDS(paste0(resultFilePath, '/', 'highSingleCost.rds')) # neural gas tree with single scenario path under high penalty cost structure
  highLargeCost = readRDS(paste0(resultFilePath, '/', 'highLargeCost.rds')) # neural gas tree with 32 scenario paths (twice as many as a binary one)
  lowSingleCost = readRDS(paste0(resultFilePath, '/', 'lowSingleCost.rds'))
  lowLargeCost = readRDS(paste0(resultFilePath, '/', 'lowLargeCost.rds'))
  
  # residual trees of different size
  # bin = 2
  highResidualCosts2 = readRDS(paste0(resultFilePath, '/', 'highResidualCosts2.rds'))
  lowResidualCosts2 = readRDS(paste0(resultFilePath, '/', 'lowResidualCosts2.rds'))
  # bin = 4
  highResidualCosts4 = readRDS(paste0(resultFilePath, '/', 'highResidualCosts4.rds'))
  lowResidualCosts4 = readRDS(paste0(resultFilePath, '/', 'lowResidualCosts4.rds'))
  # bin = 5
  highResidualCosts5 = readRDS(paste0(resultFilePath, '/', 'highResidualCosts5.rds'))
  lowResidualCosts5 = readRDS(paste0(resultFilePath, '/', 'lowResidualCosts5.rds'))
  
  ## load records of the orders of neural gas tree under high and low penalty cost structures
  highFlexibleOrders = readRDS(paste0(resultFilePath, '/', 'highFlexibleOrders.rds'))
  highFixedOrders = readRDS(paste0(resultFilePath, '/', 'highFixedOrders.rds'))
  lowFlexibleOrders = readRDS(paste0(resultFilePath, '/', 'lowFlexibleOrders.rds'))
  lowFixedOrders = readRDS(paste0(resultFilePath, '/', 'lowFixedOrders.rds'))
  
  ## load the computation time
  neuralTime = readRDS(paste0(resultFilePath, '/', 'neuralTime.rds'))
  residualTime2 = readRDS(paste0(resultFilePath, '/', 'residualTime2.rds')) # time of residual tree which bin = 2
  residualTime4 = readRDS(paste0(resultFilePath, '/', 'residualTime4.rds')) # bin = 4
  residualTime5 = readRDS(paste0(resultFilePath, '/', 'residualTime5.rds')) # bin = 5
  
  
}else{
  # create files to restore the data
  simulationDate = format(Sys.Date() ,"%m%d")
  resultFilePath = paste0('results/', simulationDate)
  realizationFilePath = paste0('realizations/', simulationDate)
  testSetFilePath = paste0('testSets/', simulationDate)
  # create a new file to restore results of the new simulation
  if(!dir.exists(resultFilePath)){
    dir.create(resultFilePath)
  }
  # create a new file to restore the realization of the new simulation
  if(!dir.exists(realizationFilePath)){
    dir.create(realizationFilePath)
  }
  # create a new file to restore the test set of the new simulation
  if(!dir.exists(testSetFilePath)){
    dir.create(testSetFilePath)
  }
  
  ## set up training set(realizations) & test set
  realizationSize = 50 # data size of the training set
  realizations = getRealizations(realizationSize) # training set
  saveRDS(realizations, paste0(realizationFilePath, '/', 'realizations.rds'))
  
  testSize = 200 # data size of the test set
  
  ## set up cost structure
  flexibleK = c(1.5, 3, 6) # to determine the flexible cost (flexible cost = flexibleK * fiexd cost)
  highCostStructure = rep(list(0), length(flexibleK)) # record the cost structure under high peanlty with different flexible costs
  lowCostStructure = rep(list(0), length(flexibleK)) # record the cost structure under low peanlty with different flexible costs
  for (i in seq_along(flexibleK)) {
    # set high penalty = 4.5 * flexible cost
    highCostStructure[[i]] = getCostStructure(flexibleK=flexibleK[i], penaltyK=4.5)
    # set low penalty = 1.5 * flexible cost
    lowCostStructure[[i]] = getCostStructure(flexibleK=flexibleK[i], penaltyK=1.5)
  }
  
  ## set up tree structure
  standardTreeStructure = c(1, 2, 4, 8, 16) # a binary tree, used by both neural gas and residaul tree
  singlePathTreeStructure = c(1, 1, 1, 1, 1) # used by neural gas tree only
  largeTreeStructure = c(1, 4, 8, 16, 32) # used by neural gas tree only
  residualTreeStructure4 = c(1, 4, 16, 64, 256) # used by residual tree (bin=4)
  residualTreeStructure5 = c(1, 5, 25, 125, 625)
  
  ## records of costs of residual and neural gas tree under high and low penalty cost structures
  # neural gas trees of different size
  # binary neural gas trees
  highNeuralCosts = rep(list(c()), 3)
  lowNeuralCosts = rep(list(c()), 3)
  # neural gas trees that have only one scenario path
  highSingleCost = rep(list(c()), 3)
  lowSingleCost = rep(list(c()), 3)
  # neural gas trees that have 32 scenario paths
  highLargeCost = rep(list(c()), 3)
  lowLargeCost = rep(list(c()), 3)
  
  # residual trees of different size
  # bin = 2
  highResidualCosts2 = rep(list(c()), 3)
  lowResidualCosts2 = rep(list(c()), 3)
  # bin = 4
  highResidualCosts4 = rep(list(c()), 3)
  lowResidualCosts4 = rep(list(c()), 3)
  # bin = 5
  highResidualCosts5 = rep(list(c()), 3)
  lowResidualCosts5 = rep(list(c()), 3)
  
  ## records of orders of neural gas tree under high and low penalty cost structures
  highFlexibleOrders = rep(list(c()), 3)
  highFixedOrders = rep(list(c()), 3)
  #
  lowFlexibleOrders = rep(list(c()), 3)
  lowFixedOrders = rep(list(c()), 3)
  
  ## records of computation time
  neuralTime = list(build=c(), opt=c())
  residualTime2 = list(build=c(), opt=c()) # time of residual tree(bin = 2
  residualTime4 = list(build=c(), opt=c()) # bin = 4
  residualTime5 = list(build=c(), opt=c()) # bin = 5
  
  
  ## start simulating
  rounds = 3 # total simulation rounds
  for (eachRound in 1:rounds){
    print(paste0('round ', eachRound))
    
    ## build trees and recourd the building time for some of the trees (use elapsed time)
    # build neural gas trees
    neuralTreeSingle = getNeuralGasTree(singlePathTreeStructure, realizations)
    
    startTime = proc.time()[[3]]
    neuralTree = getNeuralGasTree(standardTreeStructure, realizations)
    neuralTime$build = c(neuralTime$build, proc.time()[[3]] - startTime) # record the building time
    
    neuralTreeLarge = getNeuralGasTree(largeTreeStructure, realizations)
    print('built neural gas trees')
    
    # build residual trees
    productStaticCovariates = getStaticCovariates() # represent the static covariates of a new product
    
    startTime = proc.time()[[3]]
    residualTree2 = getResidualTree(realizations, productStaticCovariates, 2) # take training set(realizations) and covariates to build the residual tree
    residualTime2$build = c(residualTime2$build, proc.time()[[3]] - startTime)
    print('built residual tree, bin = 2')
    
    startTime = proc.time()[[3]]
    residualTree4 = getResidualTree(realizations, productStaticCovariates, 4)
    residualTime4$build = c(residualTime4$build, proc.time()[[3]] - startTime)
    print('built residual tree, bin = 4')
    
    startTime = proc.time()[[3]]
    residualTree5 = getResidualTree(realizations, productStaticCovariates, 5)
    residualTime5$build = c(residualTime5$build, proc.time()[[3]] - startTime)
    print('built residual tree, bin = 5')
    
    ## simulate under different cost structures
    testSet = getTestSet(productStaticCovariates, testSize) # simulate the test set given the new product's covariates above
    saveRDS(testSet, paste0('testSets/', simulationDate, '/test', eachRound, '.rds'))
    
    # simulate with different sets of flexible cost under high and low penalty cost structure each round
    for (i in seq_along(flexibleK)) {
      print(paste0('flexibleK:', flexibleK[i]))
      
      ## high penalty
      # simulate
      highNeuralResults = simulate(neuralTree, testSet, standardTreeStructure, highCostStructure[[i]])
      highNeuralSingleResults = simulate(neuralTreeSingle, testSet, singlePathTreeStructure, highCostStructure[[i]])$cost # record the cost only
      highNeuralLargeResults = simulate(neuralTreeLarge, testSet, largeTreeStructure, highCostStructure[[i]])$cost # record the cost only
      
      highResidualResults2 = simulate(residualTree2, testSet, standardTreeStructure, highCostStructure[[i]])$cost # record the cost only
      HighResidualResults4 = simulate(residualTree4, testSet, residualTreeStructure4, highCostStructure[[i]])$cost # record the cost only
      print('residual tree bin 4 high solved')
      highResidualResults5 = simulate(residualTree5, testSet, residualTreeStructure5, highCostStructure[[i]])$cost # record the cost only
      print('residual tree bin 5 high solved')
      
      # record the results of each round 
      highNeuralCosts[[i]] = c(highNeuralCosts[[i]], sum(highNeuralResults$cost)) # sum up the average cost of four periods
      highSingleCost[[i]] = c(highSingleCost[[i]], sum(highNeuralSingleResults))
      highLargeCost[[i]] = c(highLargeCost[[i]], sum(highNeuralLargeResults))
      
      highResidualCosts2[[i]] = c(highResidualCosts2[[i]], sum(highResidualResults2))
      highResidualCosts4[[i]] = c(highResidualCosts4[[i]], sum(HighResidualResults4))
      highResidualCosts5[[i]] = c(highResidualCosts5[[i]], sum(highResidualResults5))
      
      highFlexibleOrders[[i]] = c(highFlexibleOrders[[i]], sum(highNeuralResults$flexibleOrders)) # sum up the average flexible orders of four periods
      highFixedOrders[[i]] = c(highFixedOrders[[i]], sum(highNeuralResults$fixedOrders)) # sum up the average fiexd orders of four periods
      
      ## low penalty
      # simulate
      startTime = NA
      if(flexibleK[i] == 1.5){print(1);startTime = proc.time()[[3]]}
      lowNeuralResults = simulate(neuralTree, testSet, standardTreeStructure, lowCostStructure[[i]])
      if(flexibleK[i] == 1.5){neuralTime$opt = c(neuralTime$opt, proc.time()[[3]] - startTime)}
      
      lowNeuralSingleResults = simulate(neuralTreeSingle, testSet, singlePathTreeStructure, lowCostStructure[[i]])$cost # record the cost only
      lowNeuralLargeResults = simulate(neuralTreeLarge, testSet, largeTreeStructure, lowCostStructure[[i]])$cost # record the cost only
      
      if(flexibleK[i] == 1.5){print(2);startTime = proc.time()[[3]]}
      lowResidualResults2 = simulate(residualTree2, testSet, standardTreeStructure, lowCostStructure[[i]])$cost
      if(flexibleK[i] == 1.5){residualTime2$opt = c(residualTime2$opt, proc.time()[[3]] - startTime)}
      
      if(flexibleK[i] == 1.5){print(3);startTime = proc.time()[[3]]}
      lowResidualResults4 = simulate(residualTree4, testSet, residualTreeStructure4, lowCostStructure[[i]])$cost
      if(flexibleK[i] == 1.5){residualTime4$opt = c(residualTime4$opt, proc.time()[[3]] - startTime)}
      print('residual tree bin 4 low solved')
      
      if(flexibleK[i] == 1.5){print(4);startTime = proc.time()[[3]]}
      lowResidualResults5 = simulate(residualTree5, testSet, residualTreeStructure5, lowCostStructure[[i]])$cost
      if(flexibleK[i] == 1.5){residualTime5$opt = c(residualTime5$opt, proc.time()[[3]] - startTime)}
      print('residual tree bin 5 low solved')
      
      # record the results of each round 
      lowNeuralCosts[[i]] = c(lowNeuralCosts[[i]], sum(lowNeuralResults$cost))
      lowSingleCost[[i]] = c(lowSingleCost[[i]], sum(lowNeuralSingleResults))
      lowLargeCost[[i]] = c(lowLargeCost[[i]], sum(lowNeuralLargeResults))
      
      lowResidualCosts2[[i]] = c(lowResidualCosts2[[i]], sum(lowResidualResults2))
      lowResidualCosts4[[i]] = c(lowResidualCosts4[[i]], sum(lowResidualResults4))
      lowResidualCosts5[[i]] = c(lowResidualCosts5[[i]], sum(lowResidualResults5))
      
      lowFlexibleOrders[[i]] = c(lowFlexibleOrders[[i]], sum(lowNeuralResults$flexibleOrders))
      lowFixedOrders[[i]] = c(lowFixedOrders[[i]], sum(lowNeuralResults$fixedOrders))
      
    }
    
    # save the results at the final round
    if (eachRound == rounds){
      ## high penalty
      saveRDS(highNeuralCosts, paste0(resultFilePath, '/', 'highNeuralCosts.rds'))
      saveRDS(highSingleCost, paste0(resultFilePath, '/', 'highSingleCost.rds'))
      saveRDS(highLargeCost, paste0(resultFilePath, '/', 'highLargeCost.rds'))
      
      saveRDS(highResidualCosts2, paste0(resultFilePath, '/', 'highResidualCosts2.rds'))
      saveRDS(highResidualCosts4, paste0(resultFilePath, '/', 'highResidualCosts4.rds'))
      saveRDS(highResidualCosts5, paste0(resultFilePath, '/', 'highResidualCosts5.rds'))
      
      saveRDS(highFlexibleOrders, paste0(resultFilePath, '/', 'highFlexibleOrders.rds'))
      saveRDS(highFixedOrders, paste0(resultFilePath, '/', 'highFixedOrders.rds'))
      
      ## low penalty
      saveRDS(lowNeuralCosts, paste0(resultFilePath, '/', 'lowNeuralCosts.rds'))
      saveRDS(lowSingleCost, paste0(resultFilePath, '/', 'lowSingleCost.rds'))
      saveRDS(lowLargeCost, paste0(resultFilePath, '/', 'lowLargeCost.rds'))
      
      saveRDS(lowResidualCosts2, paste0(resultFilePath, '/', 'lowResidualCosts.rds'))
      saveRDS(lowResidualCosts4, paste0(resultFilePath, '/', 'lowResidualCosts4.rds'))
      saveRDS(lowResidualCosts5, paste0(resultFilePath, '/', 'lowResidualCosts5.rds'))
      
      saveRDS(lowFlexibleOrders, paste0(resultFilePath, '/', 'lowFlexibleOrders.rds'))
      saveRDS(lowFixedOrders, paste0(resultFilePath, '/', 'lowFixedOrders.rds'))
      
      saveRDS(neuralTime, paste0(resultFilePath, '/', 'neuralTime.rds'))
      saveRDS(residualTime2, paste0(resultFilePath, '/', 'residualTime2.rds'))
      saveRDS(residualTime4, paste0(resultFilePath, '/', 'residualTime4.rds'))
      saveRDS(residualTime5, paste0(resultFilePath, '/', 'residualTime5.rds'))
      
      paste0('done, results saved')
    }
  }
}


### ----- visualization -----
## --- cost ratio ---
# records of cost ratios between nerual gas and residual trees
highRatio2 = rep(list(c()), 3) # ratio between neural gas tree & residual tree (bin=2) under high penalty cost structure
highRatio4 = rep(list(c()), 3)
highRatio5 = rep(list(c()), 3)
lowRatio2 = rep(list(c()), 3)
lowRatio4 = rep(list(c()), 3)
lowRatio5 = rep(list(c()), 3)

# calculate ratios
for (i in seq_along(flexibleK)){
  highRatio2[[i]] = highNeuralCosts[[i]] / highResidualCosts2[[i]]
  highRatio4[[i]] = highNeuralCosts[[i]] / highResidualCosts4[[i]]
  highRatio5[[i]] = highNeuralCosts[[i]] / highResidualCosts5[[i]]
  
  lowRatio2[[i]] = lowNeuralCosts[[i]] / lowResidualCosts2[[i]]
  lowRatio4[[i]] = lowNeuralCosts[[i]] / lowResidualCosts4[[i]]
  lowRatio5[[i]] = lowNeuralCosts[[i]] / lowResidualCosts5[[i]]
}

## box plot
highFlexible1.5 = data.frame(bin2=highRatio2[[1]], bin4=highRatio4[[1]], bin5=highRatio5[[1]]) # cost ratio under high penalty cost structure and flexible cost = 1.5 * fixed cost 
highFlexible6 = data.frame(bin2=highRatio2[[3]], bin4=highRatio4[[3]], bin5=highRatio5[[3]])
lowFlexible1.5 = data.frame(bin2=lowRatio2[[1]], bin4=lowRatio4[[1]], bin5=lowRatio5[[1]])
lowFlexible6 = data.frame(bin2=lowRatio2[[3]], bin4=lowRatio4[[3]], bin5=lowRatio5[[3]])

x11(width=70,height=60)
par(mfrow=c(2,2))
# high penalty
boxplot(highFlexible1.5, outline=FALSE, ylim=c(0.35, 1.15), xlab='bin num (residual tree)', ylab='cost ratio')
mytitle = "high penalty(= 4.5 * flexible cost)"
mysubtitle = "flexible cost = 1.5 * fixed cost"
mtext(side=3, line=2, cex=1.2, mytitle)
mtext(side=3, line=0.5, cex=1.1, mysubtitle)
#
boxplot(highFlexible6, outline=FALSE, ylim=c(0.35, 1.15), xlab='bin num (residual tree)', ylab='cost ratio')
mytitle = "high penalty(= 4.5 * flexible cost)"
mysubtitle = "flexible cost = 6 * fixed cost"
mtext(side=3, line=2, cex=1.2, mytitle)
mtext(side=3, line=0.5, cex=1.1, mysubtitle)
# low penalty
boxplot(lowFlexible1.5, outline=FALSE, ylim=c(0.6, 1.3), xlab='bin num (residual tree)', ylab='cost ratio')
mytitle = "low penalty(= 1.5 * flexible cost)"
mysubtitle = "flexible cost = 1.5 * fixed cost"
mtext(side=3, line=2, cex=1.2, mytitle)
mtext(side=3, line=0.5, cex=1.1, mysubtitle)
#
boxplot(lowFlexible6, outline=FALSE, ylim=c(0.6, 1.3), xlab='bin num (residual tree)', ylab='cost ratio')
mytitle = "low penalty(= 1.5 * flexible cost)"
mysubtitle = "flexible cost = 6 * fixed cost"
mtext(side=3, line=2, cex=1.2, mytitle)
mtext(side=3, line=0.5, cex=1.1, mysubtitle)


## the 10th and 90th quantile for each cost ratio
highPercentile2 = lapply(highRatio2, function(x){round(quantile(x, c(0.1, 0.9)), 3)}) # Percentile of cost ratio between neural gas tree & residual tree (bin=2) under high penalty cost structure
highPercentile4 = lapply(highRatio4, function(x){round(quantile(x, c(0.1, 0.9)), 3)}) # Percentile of cost ratio between neural gas tree & residual tree (bin=4) under high penalty cost structure
highPercentile5 = lapply(highRatio5, function(x){round(quantile(x, c(0.1, 0.9)), 3)})
lowPercentile2 = lapply(lowRatio2, function(x){round(quantile(x, c(0.1, 0.9)), 3)})
lowPercentile4 = lapply(lowRatio4, function(x){round(quantile(x, c(0.1, 0.9)), 3)})
lowPercentile5 = lapply(lowRatio5, function(x){round(quantile(x, c(0.1, 0.9)), 3)})


## wilcox.test (two sided)
# high penalty & flexible cost = 1.5 * fixed cost
wilcox.test(highNeuralCosts[[1]], highResidualCosts2[[1]], paired=TRUE)
wilcox.test(highNeuralCosts[[1]], highResidualCosts4[[1]], paired=TRUE)
wilcox.test(highNeuralCosts[[1]], highResidualCosts5[[1]], paired=TRUE)
# high penalty & flexible cost = 6 * fixed cost
wilcox.test(highNeuralCosts[[3]], highResidualCosts2[[3]], paired=TRUE)
wilcox.test(highNeuralCosts[[3]], highResidualCosts4[[3]], paired=TRUE)
wilcox.test(highNeuralCosts[[3]], highResidualCosts5[[3]], paired=TRUE)

# low penalty & flexible cost = 1.5 * fixed cost
wilcox.test(lowNeuralCosts[[1]], lowResidualCosts2[[1]], paired=TRUE)
wilcox.test(lowNeuralCosts[[1]], lowResidualCosts4[[1]], paired=TRUE)
wilcox.test(lowNeuralCosts[[1]], lowResidualCosts5[[1]], paired=TRUE)
# low penalty & flexible cost = 6 * fixed cost
wilcox.test(lowNeuralCosts[[3]], lowResidualCosts2[[3]], paired=TRUE)
wilcox.test(lowNeuralCosts[[3]], lowResidualCosts4[[3]], paired=TRUE)
wilcox.test(lowNeuralCosts[[3]], lowResidualCosts5[[3]], paired=TRUE)

## wilcox.test(one sided)
# high penalty & flexible cost = 6 * fixed cost
wilcox.test(highNeuralCosts[[3]], highResidualCosts2[[3]], alternative="less", paired=TRUE)
wilcox.test(highNeuralCosts[[3]], highResidualCosts4[[3]], alternative="less", paired=TRUE)
wilcox.test(highNeuralCosts[[3]], highResidualCosts5[[3]], alternative="less", paired=TRUE)
# low penalty & flexible cost = 6 * fixed cost
wilcox.test(lowNeuralCosts[[3]], lowResidualCosts2[[3]], alternative="less", paired=TRUE)
wilcox.test(lowNeuralCosts[[3]], lowResidualCosts4[[3]], alternative="less", paired=TRUE)
wilcox.test(lowNeuralCosts[[3]], lowResidualCosts5[[3]], alternative="less", paired=TRUE)


## --- computation time ---
# computation time includes three parts: total time = tree building time + optimization time
neuralTimeTotal = neuralTime$build + neuralTime$opt
residualTimeTotal2 = residualTime2$build + residualTime2$opt # total time of residual tree (bin=2) under low penaly & flexible cost = 1.5 * fixed cost
residualTimeTotal4 = residualTime4$build + residualTime4$opt
residualTimeTotal5 = residualTime5$build + residualTime5$opt

## the 10th and 90th quantile for each computation time
round(quantile(neuralTime$build, c(0.1, 0.9)), 3)
round(quantile(neuralTime$opt, c(0.1, 0.9)), 3)

round(quantile(residualTime2$build, c(0.1, 0.9)), 3)
round(quantile(residualTime2$opt, c(0.1, 0.9)), 3)

round(quantile(residualTime4$build, c(0.1, 0.9)), 3)
round(quantile(residualTime4$opt, c(0.1, 0.9)), 3)

round(quantile(residualTime5$build, c(0.1, 0.9)), 3)
round(quantile(residualTime5$opt, c(0.1, 0.9)), 3)

round(quantile(neuralTimeTotal, c(0.1, 0.9)), 3)
round(quantile(residualTimeTotal2, c(0.1, 0.9)), 3)
round(quantile(residualTimeTotal4, c(0.1, 0.9)), 3)
round(quantile(residualTimeTotal5, c(0.1, 0.9)), 3)

## box plot
x11(width=70,height=60)
par(mfrow=c(2,2), oma=rep(0, 4), mar=c(2, 4, 2, 2))
boxplot(neuralTimeTotal, outline=FALSE, xaxt='n', ylab='Total time', main='NeuralGas tree (1, 2, 4, 8, 16)')
boxplot(residualTimeTotal2, outline=FALSE,  xaxt='n', ylab='Total time', main='Residual tree (1, 2, 4, 8, 16)')
boxplot(residualTimeTotal4, outline=FALSE, xaxt='n', ylab='Total time', main='Residual tree (1, 4, 8, 16, 64)')
boxplot(residualTimeTotal5, outline=FALSE, xaxt='n', ylab='Total time', main='Residual tree (1, 5, 25, 125, 625)')


## --- supplier ratio ---
# calculate supplier ratio: flexible orders / flxible orders + fixed orders
highSupplierRatio = c() # suuplier ratio under high penalty cost structure
lowSupplierRatio = c()
for (i in seq_along(flexibleK)){
  highSupplierRatio = c(highSupplierRatio, mean(highFlexibleOrders[[i]]) / (mean(highFlexibleOrders[[i]]) + mean(highFixedOrders[[i]])))
  lowSupplierRatio = c(lowSupplierRatio, mean(lowFlexibleOrders[[i]]) / (mean(lowFlexibleOrders[[i]]) + mean(lowFixedOrders[[i]])))
}

plot(1:length(flexibleK), highSupplierRatio, type='b',lty=2, lwd=2, col='blue',
     xlab='flexible cost', ylab='flexible ratio', xaxt='n', yaxt='n', ylim=c(0, 0.4))
axis(1, at=1:length(flexibleK), labels=flexibleK)
axis(2, at=seq(0, 0.4, 0.05))
lines(1:length(flexibleK), lowSupplierRatio, type='b', lty=2, lwd=2, col='red')
legend('topright', legend=c('high penalty', 'low penalty'), col=c('blue', 'red'), text.col=c('blue', 'red'),
       lty=2, lwd=2, cex = 0.85)


## --- value of flexibility ---
# record the cost ratio between different size of neural gas trees under high & low penalty cost structure
stdRatioHigh = rep(list(c()), 3) # cost ratio = cost of the standard neural gas tree (binary tree) / cost of the single path neural gas tree
largeRatioHigh = rep(list(c()), 3) # cost ratio = cost of the large neural gas tree/ cost of the single path neural gas tree
stdRatioLow = rep(list(c()), 3)
largeRatioLow = rep(list(c()), 3)
# calculate ratios
if(simulationMode){
  highCost = readRDS(paste0(resultFilePath, '/', 'highCost.rds'))
  lowCost = readRDS(paste0(resultFilePath, '/', 'lowCost.rds'))
  for (i in seq_along(flexibleK)){
    stdRatioHigh[[i]] = highCost[[i]] / highSingleCost[[i]]
    largeRatioHigh[[i]] = highLargeCost[[i]] / highSingleCost[[i]]
    
    stdRatioLow[[i]] = lowCost[[i]] / lowSingleCost[[i]]
    largeRatioLow[[i]] = lowLargeCost[[i]] / lowSingleCost[[i]]
  }
}else{
  for (i in seq_along(flexibleK)){
    stdRatioHigh[[i]] = highNeuralCosts[[i]] / highSingleCost[[i]]
    largeRatioHigh[[i]] = highLargeCost[[i]] / highSingleCost[[i]]
    
    stdRatioLow[[i]] = lowNeuralCosts[[i]] / lowSingleCost[[i]]
    largeRatioLow[[i]] = lowLargeCost[[i]] / lowSingleCost[[i]]
  }
}

## box plot
highRatio1.5 = data.frame(standard=stdRatioHigh[[1]], large=largeRatioHigh[[1]])
highRatio6 = data.frame(standard=stdRatioHigh[[3]], large=largeRatioHigh[[3]])
lowRatio1.5 = data.frame(standard=stdRatioLow[[1]], large=largeRatioLow[[1]])
lowRatio6 = data.frame(standard=stdRatioLow[[3]], large=largeRatioLow[[3]])

x11(width=60,height=60)
par(mfrow=c(2,2))
# high penalty
boxplot(highRatio1.5, outline=FALSE, ylab='cost ratio', ylim=c(0.2, 1))
mytitle = "high penalty(= 4.5 * flexible cost)"
mysubtitle = "flexible cost = 1.5 * fixed cost"
mtext(side=3, line=2, cex=1.2, mytitle)
mtext(side=3, line=0.5, cex=1.1, mysubtitle)
#
boxplot(highRatio6, outline=FALSE, ylab='cost ratio', ylim=c(0.2, 1))
mytitle = "high penalty(= 4.5 * flexible cost)"
mysubtitle = "flexible cost = 6 * fixed cost"
mtext(side=3, line=2, cex=1.2, mytitle)
mtext(side=3, line=0.5, cex=1.1, mysubtitle)
# low penalty
boxplot(lowRatio1.5, outline=FALSE, ylab='cost ratio', ylim=c(0.5, 1.3))
mytitle = "low penalty(= 1.5 * flexible cost)"
mysubtitle = "flexible cost = 1.5 * fixed cost"
mtext(side=3, line=2, cex=1.2, mytitle)
mtext(side=3, line=0.5, cex=1.1, mysubtitle)
#
boxplot(lowRatio6, outline=FALSE, ylab='cost ratio', ylim=c(0.5, 1.3))
mytitle = "low penalty(= 1.5 * flexible cost)"
mysubtitle = "flexible cost = 6 * fixed cost"
mtext(side=3, line=2, cex=1.2, mytitle)
mtext(side=3, line=0.5, cex=1.1, mysubtitle)