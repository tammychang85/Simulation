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
    saveRDS(highResidualCosts5, paste0('results/highResidualCosts4', eachRound, '.rds'))
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

###  ---- visualization ----
# cost ratio between nerual gas and residual trees
highRatio2 = c()
highRatio4 = c()
highRatio5 = c()
lowRatio2 = c()
lowRatio4 = c()
lowRatio5 = c()
for (i in seq_along(flexibleK)){
  highRatio2 = c(highRatio2, mean(highNeuralCosts[[i]]) / mean(highResidualCosts2[[i]]))
  highRatio4 = c(highRatio4, mean(highNeuralCosts[[i]]) / mean(highResidualCosts4[[i]]))
  highRatio5 = c(highRatio5, mean(highNeuralCosts[[i]]) / mean(highResidualCosts5[[i]]))
  
  lowRatio2 = c(lowRatio2, mean(lowNeuralCosts[[i]]) / mean(lowResidualCosts2[[i]]))
  lowRatio4 = c(lowRatio4, mean(lowNeuralCosts[[i]]) / mean(lowResidualCosts4[[i]]))
  lowRatio5 = c(lowRatio5, mean(lowNeuralCosts[[i]]) / mean(lowResidualCosts5[[i]]))
}

png('graphs/HighCostRatio.png')
plot(1:length(flexibleK), highRatio2, type='b',lty=2, lwd=2, col='blue',
     xlab='flexible cost', ylab='cost ratio', xaxt='n', yaxt='n', ylim=c(0.75, 1.2), main='high penalty')
axis(1, at=1:length(flexibleK), labels=flexibleK)
axis(2, at=seq(0.75, 1.2, 0.05))
lines(1:length(flexibleK), highRatio4, type='b', lty=2, lwd=2, col='orange')
lines(1:length(flexibleK), highRatio5, type='b', lty=2, lwd=2, col='red')
legend('topright', legend=c('nt / rt (bin2)', 'nt / rt (bin4)', 'nt / rt (bin5)'),
       col=c('blue', 'orange', 'red'), text.col=c('blue', 'orange', 'red'), lty=2, lwd=2, cex = 0.85)
dev.off()

png('graphs/LowCostRatio.png')
plot(1:length(flexibleK), lowRatio2, type='b',lty=2, lwd=2, col='blue',
     xlab='flexible cost', ylab='cost ratio', xaxt='n', yaxt='n', ylim=c(0.95, 1.1), main='high penalty')
axis(1, at=1:length(flexibleK), labels=flexibleK)
axis(2, at=seq(0.95, 1.1, 0.025))
lines(1:length(flexibleK), lowRatio4, type='b', lty=2, lwd=2, col='orange')
lines(1:length(flexibleK), lowRatio5, type='b', lty=2, lwd=2, col='red')
legend('topright', legend=c('nt / rt (bin2)', 'nt / rt (bin4)', 'nt / rt (bin5)'),
       col=c('blue', 'orange', 'red'), text.col=c('blue', 'orange', 'red'), lty=2, lwd=2, cex = 0.85)
dev.off()