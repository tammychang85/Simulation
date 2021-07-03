###  ---- compare neural gas tree and residual tree ----
realizationSize = 50
testSize = 200
realizations = getRealizations(realizationSize)
round = 30

flexibleK = c(1.5, 3, 6) # parameters of the flexible cost
highCostStructure = rep(list(0), length(flexibleK)) # cost structure under high peanlty with different flexibleK
lowCostStructure = rep(list(0), length(flexibleK)) # cost structure under low peanlty with different flexibleK
for (i in seq_along(flexibleK)) {
  highCostStructure[[i]] = getCostStructure(flexibleK=flexibleK[i], penaltyK=4.5)
  lowCostStructure[[i]] = getCostStructure(flexibleK=flexibleK[i], penaltyK=1.5)
}


## costs of two algos under high and low penalty 
highNeuralCosts = rep(list(c()), 3)
lowNeuralCosts = rep(list(c()), 3)

# bin = 2
highResidualCosts2 = rep(list(c()), 3)
lowResidualCosts2 = rep(list(c()), 3)
# bin = 4
highResidualCosts4 = rep(list(c()), 3)
lowResidualCosts4 = rep(list(c()), 3)
# bin = 5
highResidualCosts5 = rep(list(c()), 3)
lowResidualCosts5 = rep(list(c()), 3)

# start simulating
standardTreeStructure = c(1, 2, 4, 8, 16)
residualTreeStructure4 = c(1, 4, 16, 64, 256)
residualTreeStructure5 = c(1, 5, 25, 125, 625)

for (eachRound in 1:round){
  print(paste0('round ', eachRound))
  # build trees
  neuralTree = getNeuralGasTree(standardTreeStructure, realizations)
  print('build neural gas tree')
  
  productStaticCovariates = getStaticCovariates()
  residualTree2 = getResidualTree(realizations, productStaticCovariates, 2)
  print('build residual tree, bin = 2')

  residualTree4 = getResidualTree(realizations, productStaticCovariates, 4)
  print('build residual tree, bin = 4')
  
  residualTree5 = getResidualTree(realizations, productStaticCovariates, 5)
  print('build residual tree, bin = 5')
  
  # simulate under different cost structures
  testSet = getTestSet(productStaticCovariates, testSize)
  saveRDS(testSet, paste0('testSets/testSet', eachRound, '.rds'))
  for (i in seq_along(flexibleK)) {
    print(paste0('flexibleK：', flexibleK[i]))
    ## high penalty
    # calculate the cost
    eachHighNeuralCost = simulate(neuralTree, testSet, standardTreeStructure, highCostStructure[[i]])$cost
    eachHighResidualCost2 = simulate(residualTree2, testSet, standardTreeStructure, highCostStructure[[i]])$cost
    eachHighResidualCost4 = simulate(residualTree4, testSet, residualTreeStructure4, highCostStructure[[i]])$cost
    print('residual tree bin 4 high solved')
    eachHighResidualCost5 = simulate(residualTree5, testSet, residualTreeStructure5, highCostStructure[[i]])$cost
    print('residual tree bin 5 high solved')
    # record costs of each round 
    highNeuralCosts[[i]] = c(highNeuralCosts[[i]], sum(eachHighNeuralCost))
    highResidualCosts2[[i]] = c(highResidualCosts2[[i]], sum(eachHighResidualCost2))
    highResidualCosts4[[i]] = c(highResidualCosts4[[i]], sum(eachHighResidualCost4))
    highResidualCosts5[[i]] = c(highResidualCosts5[[i]], sum(eachHighResidualCost5))
    
    ## low penalty
    # calculate the cost
    eachLowNeuralCost = simulate(neuralTree, testSet, standardTreeStructure, lowCostStructure[[i]])$cost
    eachLowResidualCost2 = simulate(residualTree2, testSet, standardTreeStructure, lowCostStructure[[i]])$cost
    eachLowResidualCost4 = simulate(residualTree4, testSet, residualTreeStructure4, lowCostStructure[[i]])$cost
    print('residual tree bin 4 low solved')
    eachLowResidualCost5 = simulate(residualTree5, testSet, residualTreeStructure5, lowCostStructure[[i]])$cost
    print('residual tree bin 5 low solved')
    # record costs of each round 
    lowNeuralCosts[[i]] = c(lowNeuralCosts[[i]], sum(eachLowNeuralCost))
    lowResidualCosts2[[i]] = c(lowResidualCosts2[[i]], sum(eachLowResidualCost2))
    lowResidualCosts4[[i]] = c(lowResidualCosts4[[i]], sum(eachLowResidualCost4))
    lowResidualCosts5[[i]] = c(lowResidualCosts5[[i]], sum(eachLowResidualCost5))
    
  }
  
  # save the results every ten round
  if ((eachRound %% 10) == 0){
    ## high penalty
    saveRDS(highNeuralCosts, paste0('results/highNeuralCosts', eachRound, '.rds'))
    saveRDS(highResidualCosts2, paste0('results/highResidualCosts2', eachRound, '.rds'))
    saveRDS(highResidualCosts4, paste0('results/highResidualCosts4', eachRound, '.rds'))
    saveRDS(highResidualCosts5, paste0('results/highResidualCosts5', eachRound, '.rds'))
    
    ## low penalty
    saveRDS(lowNeuralCosts, paste0('results/lowNeuralCosts', eachRound, '.rds'))
    saveRDS(lowResidualCosts2, paste0('results/lowResidualCosts2', eachRound, '.rds'))
    saveRDS(lowResidualCosts4, paste0('results/lowResidualCosts4', eachRound, '.rds'))
    saveRDS(lowResidualCosts5, paste0('results/lowResidualCosts5', eachRound, '.rds'))
    
    paste0('round ', eachRound, ' results saved')
  }
  print('---- done ----')
  print('')
}

###  ---- visualization cost ratio ----
# cost ratio between nerual gas and residual trees
highRatio2 = rep(list(c()), 3)
highRatio4 = rep(list(c()), 3)
highRatio5 = rep(list(c()), 3)
lowRatio2 = rep(list(c()), 3)
lowRatio4 = rep(list(c()), 3)
lowRatio5 = rep(list(c()), 3)
for (i in seq_along(flexibleK)){
  highRatio2[[i]] = highNeuralCosts[[i]] / highResidualCosts2[[i]]
  highRatio4[[i]] = highNeuralCosts[[i]] / highResidualCosts4[[i]]
  highRatio5[[i]] = highNeuralCosts[[i]] / highResidualCosts5[[i]]
  
  lowRatio2[[i]] = lowNeuralCosts[[i]] / lowResidualCosts2[[i]]
  lowRatio4[[i]] = lowNeuralCosts[[i]] / lowResidualCosts4[[i]]
  lowRatio5[[i]] = lowNeuralCosts[[i]] / lowResidualCosts5[[i]]
}

highPercentile2 = lapply(highRatio2, function(x){round(quantile(x, c(0.1, 0.9)), 3)})
highPercentile4 = lapply(highRatio4, function(x){round(quantile(x, c(0.1, 0.9)), 3)})
highPercentile5 = lapply(highRatio5, function(x){round(quantile(x, c(0.1, 0.9)), 3)})

lowPercentile2 = lapply(lowRatio2, function(x){round(quantile(x, c(0.1, 0.9)), 3)})
lowPercentile4 = lapply(lowRatio4, function(x){round(quantile(x, c(0.1, 0.9)), 3)})
lowPercentile5 = lapply(lowRatio5, function(x){round(quantile(x, c(0.1, 0.9)), 3)})


highFlexible1.5 = data.frame(bin2=highRatio2[[1]], bin4=highRatio4[[1]], bin5=highRatio5[[1]])
highFlexible6 = data.frame(bin2=highRatio2[[3]], bin4=highRatio4[[3]], bin5=highRatio5[[3]])

lowFlexible1.5 = data.frame(bin2=lowRatio2[[1]], bin4=lowRatio4[[1]], bin5=lowRatio5[[1]])
lowFlexible6 = data.frame(bin2=lowRatio2[[3]], bin4=lowRatio4[[3]], bin5=lowRatio5[[3]])


## box plot
# high penalty
x11(width=70,height=60)
par(mfrow=c(2,2))

boxplot(highFlexible1.5, outline=FALSE, ylim=c(0.35, 1.15), xlab='bin num (residual tree)', ylab='cost ratio (neuralGas / residual)')
mytitle = "high penalty(= 4.5 * flexible cost)"
mysubtitle = "flexible cost = 1.5"
mtext(side=3, line=2, cex=1.2, mytitle)
mtext(side=3, line=0.5, cex=1.1, mysubtitle)

boxplot(highFlexible6, outline=FALSE, ylim=c(0.35, 1.15), xlab='bin num (residual tree)', ylab='cost ratio (neuralGas / residual)')
mytitle = "high penalty(= 4.5 * flexible cost)"
mysubtitle = "flexible cost = 6"
mtext(side=3, line=2, cex=1.2, mytitle)
mtext(side=3, line=0.5, cex=1.1, mysubtitle)

# low penalty
boxplot(lowFlexible1.5, outline=FALSE, ylim=c(0.6, 1.3), xlab='bin num (residual tree)', ylab='cost ratio (neuralGas / residual)')
mytitle = "low penalty(= 1.5 * flexible cost)"
mysubtitle = "flexible cost = 1.5"
mtext(side=3, line=2, cex=1.2, mytitle)
mtext(side=3, line=0.5, cex=1.1, mysubtitle)

boxplot(lowFlexible6, outline=FALSE, ylim=c(0.6, 1.3), xlab='bin num (residual tree)', ylab='cost ratio (neuralGas / residual)')
mytitle = "low penalty(= 1.5 * flexible cost)"
mysubtitle = "flexible cost = 6"
mtext(side=3, line=2, cex=1.2, mytitle)
mtext(side=3, line=0.5, cex=1.1, mysubtitle)


### ---- analyze the neural gas tree further ----
# read realizations and test set
fileDate = '0619' # which ones to use
realizations = readRDS(paste0('realizations/', fileDate, '/realizations.rds'))

flexibleK = c(1.5, 3, 6) # parameters of the flexible cost
highCostStructure = rep(list(0), length(flexibleK)) # cost structure under high peanlty with different flexibleK
lowCostStructure = rep(list(0), length(flexibleK)) # cost structure under low peanlty with different flexibleK
for (i in seq_along(flexibleK)) {
  highCostStructure[[i]] = getCostStructure(flexibleK=flexibleK[i], penaltyK=4.5)
  lowCostStructure[[i]] = getCostStructure(flexibleK=flexibleK[i], penaltyK=1.5)
}

# results under high and low penalty 
highCost = rep(list(c()), 3)
lowCost = rep(list(c()), 3)

highFlexibleOrders = rep(list(c()), 3)
lowFlexibleOrders = rep(list(c()), 3)

highFixedOrders = rep(list(c()), 3)
lowFixedOrders = rep(list(c()), 3)

# start simulating
round = 30
standardTreeStructure = c(1, 2, 4, 8, 16)

for (eachRound in 1:round){
  print(paste0('round ', eachRound))
  # build trees
  neuralTree = getNeuralGasTree(standardTreeStructure, realizations)
  print('builded neural gas tree')
  
  # simulate under different cost structures
  testSet = readRDS(paste0('testSets/', fileDate, '/testSet', eachRound,'.rds'))
  for (i in seq_along(flexibleK)) {
    print(paste0('flexibleK：', flexibleK[i]))
    # high penalty
    eachHighResults = simulate(neuralTree, testSet, standardTreeStructure, highCostStructure[[i]])
    print('optimization done')
    
    highCost[[i]] = c(highCost[[i]], sum(eachHighResults$cost))
    highFlexibleOrders[[i]] = c(highFlexibleOrders[[i]], sum(eachHighResults$flexibleOrders))
    highFixedOrders[[i]] = c(highFixedOrders[[i]], sum(eachHighResults$fixedOrders))
    
    # low penalty
    eachLowResults = simulate(neuralTree, testSet, standardTreeStructure, lowCostStructure[[i]])
    
    lowCost[[i]] = c(lowCost[[i]], sum(eachLowResults$cost))
    lowFlexibleOrders[[i]] = c(lowFlexibleOrders[[i]], sum(eachLowResults$flexibleOrders))
    lowFixedOrders[[i]] = c(lowFixedOrders[[i]], sum(eachLowResults$fixedOrders))
  }
  
  # save the results at final round
  if (eachRound == round){
    # high penalty
    saveRDS(highCost, paste0('results/neuralGas/', fileDate, '/highCost.rds'))
    saveRDS(highFlexibleOrders, paste0('results/neuralGas/', fileDate, '/highFlexibleOrders.rds'))
    saveRDS(highFixedOrders, paste0('results/neuralGas/', fileDate, '/highFixedOrders.rds'))
    # low penalty
    saveRDS(lowCost, paste0('results/neuralGas/', fileDate, '/lowCost.rds'))
    saveRDS(lowFlexibleOrders, paste0('results/neuralGas/', fileDate, '/lowFlexibleOrders.rds'))
    saveRDS(lowFixedOrders, paste0('results/neuralGas/', fileDate, '/lowFixedOrders.rds'))
    
    paste0('round ', eachRound, ' results saved')
  }
  print('---- done ----')
  print('')
}
###  ---- visualization suppier ratio ----
highSupplierRatio = c()
lowSupplierRatio = c()
for (i in seq_along(flexibleK)){
  highSupplierRatio = c(highSupplierRatio, mean(highFlexibleOrders[[i]]) / (mean(highFlexibleOrders[[i]]) + mean(highFixedOrders[[i]])))
  lowSupplierRatio = c(lowSupplierRatio, mean(lowFlexibleOrders[[i]]) / (mean(lowFlexibleOrders[[i]]) + mean(lowFixedOrders[[i]])))
}

png('graphs/SupplierRatio.png')
plot(1:length(flexibleK), highSupplierRatio, type='b',lty=2, lwd=2, col='blue',
     xlab='flexible cost', ylab='flexible ratio', xaxt='n', yaxt='n', ylim=c(0, 0.4))
axis(1, at=1:length(flexibleK), labels=flexibleK)
axis(2, at=seq(0, 0.4, 0.05))
lines(1:length(flexibleK), lowSupplierRatio, type='b', lty=2, lwd=2, col='red')
legend('topright', legend=c('high penalty', 'low penalty'), col=c('blue', 'red'), text.col=c('blue', 'red'),
       lty=2, lwd=2, cex = 0.85)
dev.off()
### ----  analyze the value of flexibility ----
detach("package:scenario", unload = TRUE)
# modified buildtree function: able to build tree with single path
import::here(buildtree, .from = "BuildScenarioTree.R")

fileDate = '0619' # which ones to use
realizations = readRDS(paste0('realizations/', fileDate, '/realizations.rds'))

singlePathTreeStructure = c(1, 1, 1, 1, 1)
mediumTreeStructure = c(1, 4, 8, 16, 32)
largeTreeStructure = c(1, 8, 16, 32, 32) # the tree size can not be larger than the realization size

# start simulating
round = 30

highSingleCost = rep(list(c()), 3)
highMediumCost = rep(list(c()), 3)
highLargeCost = rep(list(c()), 3)

lowSingleCost = rep(list(c()), 3)
lowMediumCost = rep(list(c()), 3)
lowLargeCost = rep(list(c()), 3)

for (eachRound in 1:round){
  print(paste0('round ', eachRound))
  # build trees
  neuralTreeSingle = getNeuralGasTree(singlePathTreeStructure, realizations)
  print('builded neural gas tree single')
  
  neuralTreeMedium = getNeuralGasTree(mediumTreeStructure, realizations)
  print('builded neural gas tree medium')
  
  neuralTreeLarge = getNeuralGasTree(largeTreeStructure, realizations)
  print('builded neural gas tree large')
  
  # simulate under different cost structures
  testSet = readRDS(paste0('testSets/', fileDate, '/testSet', eachRound,'.rds'))
  for (i in seq_along(flexibleK)) {
    print(paste0('flexibleK：', flexibleK[i]))
    
    # high penalty
    eachHighCostSingle = simulate(neuralTreeSingle, testSet, singlePathTreeStructure, highCostStructure[[i]])$cost
    eachHighCostMedium = simulate(neuralTreeMedium, testSet, mediumTreeStructure, highCostStructure[[i]])$cost
    eachHighCostLarge = simulate(neuralTreeLarge, testSet, largeTreeStructure, highCostStructure[[i]])$cost
    print('optimization done')
    
    highSingleCost[[i]] = c(highSingleCost[[i]], sum(eachHighCostSingle))
    highMediumCost[[i]] = c(highMediumCost[[i]], sum(eachHighCostMedium))
    highLargeCost[[i]] = c(highLargeCost[[i]], sum(eachHighCostLarge))
    
    # low penalty
    eachLowCostSingle = simulate(neuralTreeSingle, testSet, singlePathTreeStructure, lowCostStructure[[i]])$cost
    eachLowCostMedium = simulate(neuralTreeMedium, testSet, mediumTreeStructure, lowCostStructure[[i]])$cost
    eachLowCostLarge = simulate(neuralTreeLarge, testSet, largeTreeStructure, lowCostStructure[[i]])$cost
    
    lowSingleCost[[i]] = c(lowSingleCost[[i]], sum(eachLowCostSingle))
    lowMediumCost[[i]] = c(lowMediumCost[[i]], sum(eachLowCostMedium))
    lowLargeCost[[i]] = c(lowLargeCost[[i]], sum(eachLowCostLarge))
  }
  
  # save the results at final round
  if (eachRound == round){
    # high penalty
    saveRDS(highSingleCost, paste0('results/neuralGas/', fileDate, '/highSingleCost', '.rds'))
    saveRDS(highMediumCost, paste0('results/neuralGas/', fileDate, '/highMediumCost', '.rds'))
    saveRDS(highLargeCost, paste0('results/neuralGas/', fileDate, '/highLargeCost', '.rds'))
    # low penalty
    saveRDS(lowSingleCost, paste0('results/neuralGas/', fileDate, '/lowSingleCost', '.rds'))
    saveRDS(lowMediumCost, paste0('results/neuralGas/', fileDate, '/lowMediumCost', '.rds'))
    saveRDS(lowLargeCost, paste0('results/neuralGas/', fileDate, '/lowLargeCost', '.rds'))
    
    paste0('round ', eachRound, ' results saved')
  }
  print('---- done ----')
  print('')
}

for (eachRound in 1:round){
  print(paste0('round ', eachRound))
  # build trees
  neuralTreeLarge = getNeuralGasTree(largeTreeStructure, realizations)
  print('builded neural gas tree large')
  
  # simulate under different cost structures
  testSet = readRDS(paste0('testSets/', fileDate, '/testSet', eachRound,'.rds'))
  for (i in seq_along(flexibleK)) {
    print(paste0('flexibleK：', flexibleK[i]))
    
    # high penalty
    eachHighCostLarge = simulate(neuralTreeLarge, testSet, largeTreeStructure, highCostStructure[[i]])$cost
    print('optimization done')
    highLargeCost[[i]] = c(highLargeCost[[i]], sum(eachHighCostLarge))
    
    # low penalty
    eachLowCostLarge = simulate(neuralTreeLarge, testSet, largeTreeStructure, lowCostStructure[[i]])$cost
    print('optimization done')
    lowLargeCost[[i]] = c(lowLargeCost[[i]], sum(eachLowCostLarge))
  }
  
  # save the results at final round
  if (eachRound == round){
    # high penalty
    saveRDS(highLargeCost, paste0('results/neuralGas/', fileDate, '/highLargeCost', '.rds'))
    # low penalty
    saveRDS(lowLargeCost, paste0('results/neuralGas/', fileDate, '/lowLargeCost', '.rds'))
    
    paste0('round ', eachRound, ' results saved')
  }
  print('---- done ----')
  print('')
}


###
singleRatioHigh = c()
mediumRatioHigh = c()
largeRatioHigh = c()

singleRatioLow = c()
mediumRatioLow = c()
largeRatioLow = c()

for (i in seq_along(flexibleK)){
  singleRatioHigh = c(singleRatioHigh, mean(highSingleCost[[i]]) / mean(highCost[[i]]))
  mediumRatioHigh = c(mediumRatioHigh, mean(highMediumCost[[i]]) / mean(highCost[[i]]))
  largeRatioHigh = c(largeRatioHigh, mean(highLargeCost[[i]]) / mean(highCost[[i]]))
  
  singleRatioLow = c(singleRatioLow, mean(lowSingleCost[[i]]) / mean(lowCost[[i]]))
  mediumRatioLow = c(mediumRatioLow, mean(lowMediumCost[[i]]) / mean(lowCost[[i]]))
  largeRatioLow = c(largeRatioLow, mean(lowLargeCost[[i]]) / mean(lowCost[[i]]))
}


x11(width=70,height=30)
par(mfrow=c(1,2))
plot(1:length(flexibleK), singleRatioHigh, type='b',lty=2, lwd=2, col='blue',
     xlab='flexible cost', ylab='cost ratio', xaxt='n', yaxt='n', ylim=c(0, 3), main='high penalty',)
axis(1, at=1:length(flexibleK), labels=flexibleK)
axis(2, at=seq(0, 3, 0.5))
lines(1:length(flexibleK), mediumRatioHigh, type='b', lty=2, lwd=2, col='green')
lines(1:length(flexibleK), largeRatioHigh, type='b', lty=2, lwd=2, col='red')
legend('topright', legend=c('single / stand', 'medium / stand', 'large / stand'),
       col=c('blue', 'green', 'red'), text.col=c('blue', 'green', 'red'), lty=2, lwd=2, cex = 0.85)

plot(1:length(flexibleK), singleRatioLow, type='b',lty=2, lwd=2, col='blue',
     xlab='flexible cost', ylab='cost ratio', xaxt='n', yaxt='n', ylim=c(0, 3), main='low penalty',)
axis(1, at=1:length(flexibleK), labels=flexibleK)
axis(2, at=seq(0, 3, 0.5))
lines(1:length(flexibleK), mediumRatioLow, type='b', lty=2, lwd=2, col='green')
lines(1:length(flexibleK), largeRatioLow, type='b', lty=2, lwd=2, col='red')
legend('topright', legend=c('single / stand', 'medium / stand', 'large / stand'),
       col=c('blue', 'green', 'red'), text.col=c('blue', 'green', 'red'), lty=2, lwd=2, cex = 0.85)

##

stdRatioHigh = c()
mediumRatioHigh = c()

stdRatioLow = c()
mediumRatioLow = c()

for (i in seq_along(flexibleK)){
  stdRatioHigh = c(stdRatioHigh, mean(highCost[[i]]) / mean(highSingleCost[[i]]))
  mediumRatioHigh = c(mediumRatioHigh, mean(highMediumCost[[i]]) / mean(highSingleCost[[i]]))
  
  stdRatioLow = c(stdRatioLow, mean(lowCost[[i]]) / mean(lowSingleCost[[i]]))
  mediumRatioLow = c(mediumRatioLow, mean(lowMediumCost[[i]]) / mean(lowSingleCost[[i]]))
}

x11(width=60,height=40)
par(mfrow=c(1,2))
plot(1:length(flexibleK), stdRatioHigh, type='b',lty=2, lwd=2, col='blue',
     xlab='flexible cost', ylab='cost ratio', xaxt='n', yaxt='n', ylim=c(0.4, 1.), main='high penalty',)
axis(1, at=1:length(flexibleK), labels=flexibleK)
axis(2, at=seq(0.4, 1, 0.05))
lines(1:length(flexibleK), mediumRatioHigh, type='b', lty=2, lwd=2, col='red')
legend('topright', legend=c('std / single', 'large / singe'),
       col=c('blue', 'red'), text.col=c('blue', 'red'), lty=2, lwd=2, cex = 1)

plot(1:length(flexibleK), stdRatioLow, type='b',lty=2, lwd=2, col='blue',
     xlab='flexible cost', ylab='cost ratio', xaxt='n', yaxt='n', ylim=c(0.4, 1), main='low penalty',)
axis(1, at=1:length(flexibleK), labels=flexibleK)
axis(2, at=seq(0.4, 1, 0.05))
lines(1:length(flexibleK), mediumRatioLow, type='b', lty=2, lwd=2, col='red')
legend('topright', legend=c('std / single', 'large / singe'),
       col=c('blue', 'red'), text.col=c('blue', 'red'), lty=2, lwd=2, cex = 1)