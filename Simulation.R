###  ---- compare neural gas tree and residual tree ----
realizationSize = 50
testSize = 200
realizations = getRealizations(realizationSize)

standardTreeStructure = c(1, 2, 4, 8, 16)
binNum = 2
round = 30

flexibleK = c(1.5, 3, 6) # parameters of the flexible cost
highCostStructure = rep(list(0), length(flexibleK)) # cost structure under high peanlty with different flexibleK
lowCostStructure = rep(list(0), length(flexibleK)) # cost structure under low peanlty with different flexibleK
for (i in seq_along(flexibleK)) {
  highCostStructure[[i]] = getCostStructure(flexibleK=flexibleK[i], penaltyK=4.5)
  lowCostStructure[[i]] = getCostStructure(flexibleK=flexibleK[i], penaltyK=1.5)
}

highNeuralCosts = rep(list(c()), 3)
lowNeuralCosts = rep(list(c()), 3)
highResidualCosts = rep(list(c()), 3)
lowResidualCosts = rep(list(c()), 3)
for (eachRound in 1:round){
  print(paste0('round ', eachRound,'：'))
  # build trees
  neuralTree = getNeuralGasTree(standardTreeStructure, realizations)
  print('build neural gas tree')
  
  productStaticCovariates = getStaticCovariates()
  residualTree = getResidualTree(realizations, productStaticCovariates, binNum)
  print('build residual tree')
  
  # simulate under different cost structures
  testSet = getTestSet(productStaticCovariates, testSize)
  for (i in seq_along(flexibleK)) {
    # high penalty
    eachHighNeuralCost = simulate(neuralTree, testSet, standardTreeStructure, highCostStructure[[i]])$cost
    eachHighResidualCost = simulate(residualTree, testSet, standardTreeStructure, highCostStructure[[i]])$cost
    
    highNeuralCosts[[i]] = c(highNeuralCosts[[i]], sum(eachHighNeuralCost))
    highResidualCosts[[i]] = c(highResidualCosts[[i]], sum(eachHighResidualCost))
    
    # low penalty
    eachLowNeuralCost = simulate(neuralTree, testSet, standardTreeStructure, lowCostStructure[[i]])$cost
    eachLowResidualCost = simulate(residualTree, testSet, standardTreeStructure, lowCostStructure[[i]])$cost
    
    lowNeuralCosts[[i]] = c(lowNeuralCosts[[i]], sum(eachLowNeuralCost))
    lowResidualCosts[[i]] = c(lowResidualCosts[[i]], sum(eachLowResidualCost))
    
  }
  print('---- done ----')
  
  # save the results every ten round
  if ((eachRound %% 10) == 0){
    saveRDS(highNeuralCosts, paste0('results/highNeuralCosts', eachRound, '.rds'))
    saveRDS(highResidualCosts, paste0('results/highResidualCosts', eachRound, '.rds'))

    saveRDS(lowNeuralCosts, paste0('results/lowNeuralCosts', eachRound, '.rds'))
    saveRDS(lowResidualCosts, paste0('results/lowResidualCosts', eachRound, '.rds'))
    print('results saved')
  }
  print('')
}
