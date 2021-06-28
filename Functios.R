### ---- packages ----
## neural gas tree
library(scenario)
#import::here(buildtree, .from = "BuildScenarioTree.R")

## solver
library(Rglpk)


### ---- functions ----

## demand datas
# simulate static covariates of products 
getStaticCovariates = function(){
  
  x1 = rbinom(1, 1, 0.25)
  x2 = rbinom(1, 1, 0.5)
  x3 = rnorm(1, 900, 200)
  x4 = sample(seq(7.95, 22.95), 1)
  
  return(list(x1=x1, x2=x2, x3=x3, x4=x4))
}

# simulate demand paths of four periods
getDemandPath = function(staticCovariates){
  
  # static covariates
  x1 = staticCovariates$x1
  x2 = staticCovariates$x2
  x3 = staticCovariates$x3
  x4 = staticCovariates$x4
  
  # demands of period 1 to 4
  d1 = 0
  d2 = 0
  d3 = 0
  d4 = 0
  while(TRUE){
    d1 = (738 + 1072 * x1 - 403 * x2 + 2.8 * x3 - 55 * x4 + rnorm(1, 0, 1044))
    if(d1 >= 0){break}
  }
  while(TRUE){
    d2 = (-399 - 638 * x1 + 53 * x4 + 0.854 * d1 + rnorm(1, 0, 781))
    if(d2 >= 0){break}
  }
  while(TRUE){
    d3 = (-5 + 0.955 * d2 + rnorm(1, 0, 601))
    if(d3 >= 0){break}
  }
  while(TRUE){
    d4 = (874 - 46 * x4 + 0.516 * d2 + 0.318 * d3 + rnorm(1, 0, 820))
    if(d4 >= 0){break}
  }
  
  return(data.frame(d1, d2, d3, d4, x1, x2, x3, x4))
}

# simulate demand realizations
getRealizations = function(realizationSize){
  
  realizationDataFrame = data.frame()
  for (i in 1:realizationSize) {
    eachRealization = getDemandPath(getStaticCovariates())
    realizationDataFrame = rbind(realizationDataFrame, eachRealization)
  }
  realizationMatrix = rbind(rep(0, realizationSize))
  realizationMatrix = rbind(realizationMatrix, t(data.matrix(realizationDataFrame[, 1:4])))
  
  return(list(frame=realizationDataFrame, matrix=realizationMatrix))
}

getTestSet = function(staticCovariates, testSize){
  
  testDataFrame = data.frame()
  for (i in 1:testSize){
    eachTest = getDemandPath(staticCovariates)[1:4]
    testDataFrame = rbind(testDataFrame, eachTest)
  }
  testSetMatrix = rbind(rep(0, testSize))
  testSetMatrix = rbind(testSetMatrix, data.matrix(t(testDataFrame)))
  
  return(testSetMatrix)
}


## neural gas tree
# receive a vector of nodal structure and return a matrix of tree structure
getTreeStructureMatrix = function(nodePerPeriod){
  
  periodLength = length(nodePerPeriod)
  scenarioNum = nodePerPeriod[periodLength]
  
  # build the tree structure matrix
  treeStructure = c()
  nodeIndex = 1
  for(nodeNumEachPeriod in nodePerPeriod){
    for(eachNode in 1:nodeNumEachPeriod){
      treeStructure = c(treeStructure, rep(nodeIndex, scenarioNum / nodeNumEachPeriod))
      nodeIndex = nodeIndex + 1
    }
  }
  
  return(matrix(treeStructure, nrow=periodLength, byrow=TRUE))
}

getNeuralGasTree = function(nodePerPeriod, realizations, maxIteration=40000, plot = FALSE){
  
  treeStructure = getTreeStructureMatrix(nodePerPeriod)
  
  return(buildtree(realizations$matrix, treeStructure, jMax=maxIteration, plot=plot))
}


## residual tree
# learn from realizations and estimate demands using the given static covariate of the new product
getEstimatedDemands = function(staticCovariates, realizations){
  
  estimatedDemands = list()
  realizationsDf = realizations$frame
  periodLength = dim(realizations$matrix)[1]-1
  x = names(staticCovariates) # independent variables used in the linear model
  
  # perform least-squares regression on realizations of similar products sold in the past
  for (eachPeriod in 1:periodLength) {
    # learn from historical data
    y = paste(paste('d', eachPeriod, sep=''), '~')
    f = as.formula(paste(y, paste(x, collapse="+")))
    eachModel = lm(f, realizationsDf)

    # estimate demand paths of each period for the new product
    if (eachPeriod == 1){
      eachDemands = predict(eachModel, staticCovariates) + eachModel$residual
      eachDemands[which(eachDemands < 0)] = 0
      estimatedDemands = append(estimatedDemands, list(eachDemands)) 
    }
    else{
      eachDemands = c()
      for (i in seq_along(estimatedDemands[[1]])) {
        tempCovariates = staticCovariates
        for (j in 1:(eachPeriod - 1)) {
          tempCovariates = c(tempCovariates, estimatedDemands[[j]][i])
        }
        names(tempCovariates) = x
        eachDemands = c(eachDemands, predict(eachModel, tempCovariates))
      }
      eachDemands = eachDemands + eachModel$residual
      eachDemands[which(eachDemands < 0)] = 0
      estimatedDemands = append(estimatedDemands, list(eachDemands))
    }
    
    # add new dynamic covariates to the independent variables for the next period
    x = c(x, paste('d', eachPeriod, sep=''))
  }
  
  return(estimatedDemands)
}

# w：to winsorize or not
binDemands = function(demands, binNum, realizationSize, probs=c(0.05, 0.95), test=FALSE, w=FALSE){
  
  # winsorize the demands if need
  if (w){
    library(DescTools)
    demands = lapply(demands, function(x){Winsorize(x, probs=probs)})
  }
  
  # visulalize the bins if need
  if (test){
    x11(width=70,height=30)
    par(mfrow=c(2,2))
  }
  
  # bin demands into binNum bins for each period
  binnedDemands = list()
  demandProbs = list()
  
  period = 1
  for (eachDemand in demands){
    bins = cut(eachDemand, binNum)
    binsMedian = tapply(eachDemand, bins, median)
    binnedDemands = append(binnedDemands, list(binsMedian))
    demandProbs = append(demandProbs, list(as.vector(table(bins)) / realizationSize))
    
    # visulalize the bins if need
    if (test){
      barplot(table(bins), names.arg=lapply(bins.median, round), main=paste0('period', period)) 
      period = period + 1
    }
    
  }
  
  return(list(values=binnedDemands, probs=demandProbs))
}

getResidualTree = function(realizations, staticCovariates, binNum, probs=c(0.05, 0.95), test=FALSE, w=FALSE) {
  
  estimatedDemands = getEstimatedDemands(staticCovariates, realizations)
  binnedDemands = binDemands(estimatedDemands, binNum, dim(realizations$frame)[1], probs=probs, test=test, w=w)
  demands = binnedDemands$values
  probabilities = binnedDemands$probs
  
  # construct the residual tree
  demandPaths = data.frame()
  demandProbs = data.frame()
  for (i in (1:(length(demands) - 1))) {
    if (i == 1){
      for (j in seq_along(demands[[i]])) {
        curret.demand = demands[[i]][j]
        nextDemand = demands[[i + 1]]
        pathI = data.frame(curret.demand, nextDemand, row.names=NULL)
        demandPaths = rbind(demandPaths, pathI)
        
        curretProb = probabilities[[i]][j]
        nextProb = probabilities[[i + 1]]
        probI = data.frame(curretProb, nextProb, row.names=NULL)
        demandProbs = rbind(demandProbs, probI)
      }
      demandPaths = apply(demandPaths, 1, toString)
      demandProbs = apply(demandProbs, 1, toString)
    }
    else {
      currentPaths = data.frame()
      currentProbs = data.frame()
      for (j in seq_along(demandPaths)) {
        curretPeriod = rep(demandPaths[j], binNum)
        nextPeriod = demands[[i + 1]]
        pathI = data.frame(curretPeriod, nextPeriod, row.names=NULL)
        currentPaths = rbind(currentPaths, pathI)
        
        curretProb = rep(demandProbs[j], binNum)
        nextProb = probabilities[[i + 1]]
        probI = data.frame(curretProb, nextProb, row.names=NULL)
        currentProbs = rbind(currentProbs, probI) 
      }
      demandPaths = apply(currentPaths, 1, toString)
      demandProbs = apply(currentProbs, 1, toString)
    }
  }
  
  # convert the paths from string to float
  treeValues = list()
  branchProbabilities = c()
  for (i in seq_along(demandPaths)) {
    treeValues = append(treeValues, list(c(0, as.double(strsplit(demandPaths[i], ',')[[1]]))))
    branchProbabilities = c(branchProbabilities, prod(as.double(strsplit(demandProbs[i], ',')[[1]])))
  }
  treeValues = do.call(cbind, treeValues)
  
  return(list(tree_values=treeValues, branch_probabilities=branchProbabilities))
}


## linear programming
getCostStructure = function(fixedCost=1, holdingCost=0.25, flexibleK=1.5, penaltyK=1.5){
  
  flexibleCost = fixedCost * flexibleK
  penaltyCost = flexibleCost * penaltyK
  
  return(c(fixedCost=fixedCost, flexibleCost=flexibleCost, holdingCost=holdingCost, penaltyCost=penaltyCost))
}

# return a variables vector of the objective function
getVariableNames = function(nodePerPeriod) {
  
  variableNames = c()
  fixedSupplierVars = c() # variable names of the fixed supplier
  flexibleSupplierVars = c()  # variable names of the fixed supplier
  inventoryVars = c() # variable names of the inventory
  lostVars = c() # variable names of the lost
  
  for(i in 1:(length(nodePerPeriod) - 1)){
    for(j in 1:(nodePerPeriod[length(nodePerPeriod)])){
      time = as.character(i)
      scenario = as.character(j)
      #
      fixedSupplierVars = c(fixedSupplierVars, paste0('fi', time, scenario))
      flexibleSupplierVars = c(flexibleSupplierVars, paste0('fl', time, scenario))
      inventoryVars = c(inventoryVars, paste0('I', time, scenario))
      lostVars = c(lostVars, paste0('L', time, scenario))
    }
  }
  variableNames = c(variableNames, fixedSupplierVars, flexibleSupplierVars, inventoryVars, lostVars)
  
  return(variableNames)
}

# check to show objective function with variables' names or not
getObjctiveFunction = function(probability, nodePerPeriod, costStructure, check=FALSE) {
  
  obj = c() # the objective function
  fixedSupplier = c() # weights of the fixed supplier
  flexibleSupplier = c() # weights of the flexible supplier
  inventory = c() # weights of the inventory
  lost = c() # weights of the lost
  
  for (i in 1:(length(nodePerPeriod) - 1)) {
    fixedSupplier = c(fixedSupplier, costStructure[1] * probability)
    flexibleSupplier = c(flexibleSupplier, costStructure[2] * probability)
    inventory = c(inventory, costStructure[3] * probability)
    lost = c(lost, costStructure[4] * probability)
  }
  
  obj = c(fixedSupplier, flexibleSupplier, inventory, lost)
  
  # to show variables's names of the objective function
  if (check){
    objWithVariableNames = matrix(obj, nrow=1)
    colnames(objWithVariableNames) = getVariableNames(nodePerPeriod)
    print(objWithVariableNames)
  }

  return(obj)
}

# check to show objective function with variables' names or not
getConstraintsMatrix = function(objectiveFunction, nodePerPeriod, check=FALSE) {
  
  scenarioNum = nodePerPeriod[length(nodePerPeriod)]
  periodNum = length(nodePerPeriod) - 1
  VariableNum = length(objectiveFunction) # total numbers of variables of the objective function
  constraints = c()
  variableNames = NULL
  constraintIndex = NULL
  if (check){
    variableNames = getVariableNames(nodePerPeriod)
    constraintIndex = c()
  }
  
  # constraints of suppliers if total sceanrio num is more than one
  if(scenarioNum > 1){
    # constraints of fixed supplier of each period
    for (eachPeriod in 1:periodNum) {
      for (eachScenario in 1:(scenarioNum - 1)) {
        eachConstraint = rep(0, VariableNum)
        eachConstraint[(eachPeriod - 1) * scenarioNum + 1] = 1
        eachConstraint[(eachPeriod - 1) * scenarioNum + 1 + eachScenario] = -1
        constraints = c(constraints, eachConstraint)
        if (check){
          period = as.character(eachPeriod)
          scenario = as.character(eachScenario)
          constraintIndex = c(constraintIndex, paste0('fixed', period, '-', scenario)) 
        }
      }
    }
    
    # constraints of flexible supplier of period 1
    for (eachScenario in 1:(scenarioNum - 1)) {
      eachConstraint = rep(0, VariableNum)
      eachConstraint[periodNum * scenarioNum + 1] = 1
      eachConstraint[(periodNum * scenarioNum + 1 + eachScenario)] = -1
      constraints = c(constraints, eachConstraint)
      if (check){
        scenario = as.character(eachScenario)
        constraintIndex = c(constraintIndex, paste0('flexible1', '-', scenario)) 
      }
    }
    
    # constraints of flexible supplier after period 1
    if (periodNum > 1){
      # should have nodePerPeriod[eachPeriod] * ((scenarioNum/nodePerPeriod[eachPeriod]) - 1) constraints
      for (eachPeriod in 1:(periodNum - 1)){
        constraintCount = 1
        nodeSceanrioNum = scenarioNum / nodePerPeriod[eachPeriod + 1] # how many scenarios belong to a unique node of each period
        if (nodeSceanrioNum != 1){
          for(eachUniqueNode in 1:nodePerPeriod[eachPeriod + 1]){
            for(eachScenario in 1:(nodeSceanrioNum-1)){
              eachConstraint = rep(0, VariableNum)
              eachConstraint[scenarioNum * (periodNum + eachPeriod) + nodeSceanrioNum * (eachUniqueNode - 1) + 1] = 1
              eachConstraint[scenarioNum * (periodNum + eachPeriod) + nodeSceanrioNum * (eachUniqueNode - 1) + 1 + eachScenario] = -1
              constraints = c(constraints, eachConstraint)
              if (check){
                period = as.character(eachPeriod + 1)
                constraintIndex = c(constraintIndex, paste0('flexible', period, '-', as.character(constraintCount))) 
                constraintCount = constraintCount + 1
              }
            }
          } 
        }
      }
    }
  }
  
  # constraints of inventory and lost, should have scenarioNum *  periodNum constraints in total
  supplierVariableNum = scenarioNum * periodNum * 2
  for (eachPeriod in 1:(periodNum)){
    for (eachScenario in 1:(scenarioNum)){
      # constraints for inventory & lost of first period
      eachConstraint = rep(0, VariableNum)
      eachConstraint[(eachPeriod - 1) * scenarioNum + eachScenario] = 1 # fiexd supplier
      eachConstraint[(eachPeriod - 1) * scenarioNum + periodNum * scenarioNum + eachScenario] = 1 # flexible supplier
      if (eachPeriod != 1){
        eachConstraint[supplierVariableNum + (eachPeriod - 2) * scenarioNum + eachScenario] = 1 # inventory of last period 
      }
      eachConstraint[supplierVariableNum + (eachPeriod - 1) * scenarioNum + eachScenario] = -1 # inventory of the current period
      eachConstraint[supplierVariableNum + periodNum * scenarioNum + (eachPeriod - 1) * scenarioNum + eachScenario] = 1 # lost of the current period
      constraints = c(constraints, eachConstraint)
      if (check){
        period = as.character(eachPeriod)
        scenario = as.character(eachScenario)
        constraintIndex = c(constraintIndex, paste0('demand', period, '-', scenario)) 
      }
    }
  }
  
  constraintMatrix = matrix(constraints, ncol=VariableNum, byrow=TRUE)
  
  #
  if (check){
    checkConstraints = constraintMatrix
    colnames(checkConstraints) = variableNames
    rownames(checkConstraints) = constraintIndex
    View(checkConstraints)
  }
  
  return(constraintMatrix)
}

# return the right hand side of the constraints
getRHS = function(constraintMatrix, nodePerPeriod, scnearioTree) {
  
  scenarioNum = nodePerPeriod[length(nodePerPeriod)]
  periodNum = length(nodePerPeriod) - 1
  rhs = rep(0, (dim(constraintMatrix)[1]) - (scenarioNum * periodNum)) # the right hand side of constraints for suppliers
  for(eachPeriod in 1:periodNum){
    rhs = c(rhs, scnearioTree$tree_values[(eachPeriod + 1), ])
  }
  
  return(rhs)
}

optimize = function(scnearioTree, nodePerPeriod, costStructure){
  
  objectiveFunction = getObjctiveFunction(scnearioTree$branch_probabilities, nodePerPeriod, costStructure)
  constraintMatrix = getConstraintsMatrix(objectiveFunction, nodePerPeriod)
  constraintDirections = rep('==' ,dim(constraintMatrix)[1])
  rhs = getRHS(constraintMatrix, nodePerPeriod, scnearioTree)
  
  # optimize and return
  return(Rglpk_solve_LP(objectiveFunction, constraintMatrix, constraintDirections, rhs, max=FALSE))
}


## parse optimizaton results
# parse the solution from optimization results and return the order policy matrix
getOrderPolicy = function(nodePerPeriod, optimizationResults) {
  
  scenarioNum = nodePerPeriod[length(nodePerPeriod)]
  periodNum = length(nodePerPeriod) - 1
  orderPolicy = optimizationResults$solution[1:(scenarioNum * periodNum * 2)] # extract the order policy from the solution
  
  # extract fixed order policy
  fixedOrderPolicy = orderPolicy[1:(periodNum * scenarioNum)]
  uniqueFixedOrderPolicy = rep(NA, periodNum)
  index = 1
  for (eachPeriod in 1:periodNum){
    # record policy and remove duplicated ones
    uniqueFixedOrderPolicy[eachPeriod] = unique(fixedOrderPolicy[index:(eachPeriod * scenarioNum)])
    index = eachPeriod * scenarioNum + 1
  }
  
  # make a matrix of flexible policy
  flexiblePolicy = orderPolicy[(periodNum * scenarioNum + 1):length(orderPolicy)]
  flexiblePolicyMatrix = matrix(0, nrow=periodNum, ncol=scenarioNum)
  policyPerPeriod = list()
  index = 1
  for (eachPeriod in 1:periodNum){
    policyPerPeriod[[eachPeriod]] = flexiblePolicy[index:(eachPeriod * scenarioNum)]
    for(eachScenario in 1:scenarioNum){
      flexiblePolicyMatrix[eachPeriod, eachScenario] = policyPerPeriod[[eachPeriod]][eachScenario]
    }
    index = eachPeriod * scenarioNum + 1
  }
  
  # combine fixed and flexible policies
  fianlOrderPolicy = cbind(flexiblePolicyMatrix, uniqueFixedOrderPolicy)
  
  return(fianlOrderPolicy)
}

# decide which flexible options to apply in each perid for a given scenario and return the policy matrix
decideFlexiblePolicy = function(scenarioTree, observedDemands, orderPolicy) {
  
  estimatedDemands = scenarioTree$tree_values[-length(observedDemands), , drop=F] # exclude demands of the last period
  candidateFlexiblePolicy = orderPolicy[, -dim(orderPolicy)[2], drop=F] #  exclude policies of the fixed supplier
  chosenFlexiblePolicy = NULL
  # decide orders for each period of the observed demand path
  for (eachPeriod in 1:(length(observedDemands) - 1)) {
    eachEstimatedDemands = estimatedDemands[eachPeriod, ]
    
    # the distance between the observed demnad data and estimated demands of each period
    distanceMatrix = as.matrix(dist(matrix(c(observedDemands[eachPeriod], eachEstimatedDemands))))[, 1] 
    distanceMatrix = distanceMatrix[2:length(distanceMatrix)] # exclude the distance of the observed demand with itself
    
    # find the closest estimated demand to the observed demand
    smallestDistance = min(distanceMatrix)
    smallestDistanceIndex = which(distanceMatrix==smallestDistance)[1]
    closestEstimatedDemand = eachEstimatedDemands[smallestDistanceIndex]
    
    # filter the demand paths and policy
    estimatedDemandsIndex = estimatedDemands[eachPeriod, ] %in% closestEstimatedDemand
    estimatedDemands =  estimatedDemands[, estimatedDemandsIndex, drop=F] # filter the demand path
    candidateFlexiblePolicy = candidateFlexiblePolicy[, estimatedDemandsIndex, drop=F] # filter the policy
    if(dim(estimatedDemands)[2] == 1){
      break # stop filtering directly if only one demand path remains
    }
    
  }
  
  # remove the duplicated one
  if(dim(candidateFlexiblePolicy)[2] > 1){
    chosenFlexiblePolicy = candidateFlexiblePolicy[, 1]
  }
  
  if(!is.null(dim(candidateFlexiblePolicy))){
    chosenFlexiblePolicy = candidateFlexiblePolicy[, 1] # return in the form of a vector
  }
  
  return(chosenFlexiblePolicy)
}


## calculate costs
# calculate all periods of cost of the given demand path order policy
getCost = function(scenarioTree, observedDemands, costStructure, orderPolicy) {
  
  fixedOrderPolicy = orderPolicy[, dim(orderPolicy)[2]]
  flexibleOrderPolicy = decideFlexiblePolicy(scenarioTree, observedDemands, orderPolicy)
  costs = c()
  
  inventory = 0
  for (eachPeriod in 1:(length(observedDemands) - 1)) {
    quantity = inventory + fixedOrderPolicy[eachPeriod] + flexibleOrderPolicy[eachPeriod] # available items of each period
    difference = quantity - observedDemands[[eachPeriod + 1]] # inventory or unmet demands
    eachCost = 0 # cost of the current period
    
    # if have unmet demands
    if (difference < 0){
      eachCost = fixedOrderPolicy[eachPeriod] * costStructure[[1]] + flexibleOrderPolicy[eachPeriod] * costStructure[[2]] +  abs(difference) * costStructure[[4]]
      inventory = 0
    }
    # if have invetory
    else{
      eachCost = fixedOrderPolicy[eachPeriod] * costStructure[[1]] + flexibleOrderPolicy[eachPeriod] * costStructure[[2]] + difference * costStructure[[3]]
      inventory = difference
    }

    costs = c(costs, eachCost)
  }
  
  
  return(list(costs=costs, flexibleOrderPolicy=flexibleOrderPolicy, fixedOrderPolicy=fixedOrderPolicy))
}

# calculate the average cost of a given data set
getAverageCost = function(scenarioTree, testingDataSet, costStructure, orderPolicy) {
 
  costs = rep(0, 4) # costs of four period
  flexibleOrders = rep(0, 4) # orders of flexible supplier of four preriod
  fixedOrders = rep(0, 4) # orders of fixed supplier of of four preriod
  
  # get the total cost for all testing data
  for ( eachDemandPath in 1: (dim(testingDataSet)[2])){
    eachResults = getCost(scenarioTree, testingDataSet[, eachDemandPath], costStructure, orderPolicy)
    costs = costs + eachResults[[1]]
    flexibleOrders = flexibleOrders + eachResults[[2]]
    fixedOrders = fixedOrders + eachResults[[3]]
  }
  costs = costs / dim(testingDataSet)[2]
  flexibleOrders = flexibleOrders / dim(testingDataSet)[2]
  fixedOrders = fixedOrders / dim(testingDataSet)[2]
  
  return(list(cost=costs, flexibleOrders=flexibleOrders, fixedOrders=fixedOrders))
}


## coordinate functions above to do a round of simulation
simulate = function(scenarioTree, testingDataSet, nodePerPeriod, costStructure) {
  
  # get optimal solutions
  solutions = optimize(scenarioTree, nodePerPeriod, costStructure)
  # parse order policy
  orderPolicy = getOrderPolicy(nodePerPeriod, solutions)
  # results on testing datas
  results = getAverageCost(scenarioTree, testingDataSet, costStructure, orderPolicy)
  
  return(results)
}

## using LP to build the scenario tree
getWDTree = function(nodePerPeriod, realizations){
  k = nodePerPeriod[length(nodePerPeriod)] # number of scenario paths
  t = length(nodePerPeriod) - 1 # number of periods
  l = dim(realizations$matrix)[2] # number of observed demand paths
  
  obj = c(rep(1, 2 * t * k * l), rep(0, k * (t + l))) # objective function：e+, e-, d', a
  types = c(rep('C', (t * k * (2 * l + 1))), rep('B', k * l))
  constraints = c()  
  
  # e+ - e- - d' + (d)a = 0
  for (eachT in 1:t) {
    for (eachK in 1:k) {
      for (eachL in 1:l) {
        eachConstraint = rep(0, length(obj))
        eachConstraint[(2 * (eachT - 1) * k * l) + (2 * (eachK - 1) * l) + (2 * eachL) - 1] = 1 # e+
        eachConstraint[(2 * (eachT - 1) * k * l) + (2 * (eachK - 1) * l) + (2 * eachL)] = -1 # e-
        eachConstraint[(2 * t * k * l) + ((eachT - 1) * k) + eachK] = -1 # d'
        eachConstraint[(t * k * (2 * l +1)) + ((eachK - 1) * l) + eachL] = simpleRealiztions$matrix[[(eachT + 1), eachL]] # a
        constraints = c(constraints, eachConstraint)
      }
    }
  }

  # each scenario has at least one demand path (>= 1)
  for (eachK in 1:k) {
    eachConstraint = rep(0, length(obj))
    for (eachL in 1:l) {
      eachConstraint[(t * k * (2 * l +1)) + ((eachK - 1) * l) + eachL] = 1      
    }
    constraints = c(constraints, eachConstraint)
  }
  
  # one demand path could only be assinged to one scenario (= 1)
  for (eachL in 1:l) {
    eachConstraint = rep(0, length(obj))
    for (eachK in 1:k) {
      eachConstraint[(t * k * (2 * l +1)) + ((eachK - 1) * l) + eachL] = 1      
    }
    constraints = c(constraints, eachConstraint)
  }
  
  constraintMatrix = matrix(constraints, ncol=length(obj), byrow=TRUE)
  
  rhs = rep(0, (t * k * l))
  constraintDirections = rep('=', (t * k * l))
  for (eachConstraint in 1:(l + k)){
    rhs = c(rhs, 1)
  }
  for (eachConstraint in 1:k) {
    constraintDirections = c(constraintDirections, '>=')
  }
  for (eachConstraint in 1:l) {
    constraintDirections = c(constraintDirections, '=')
  }
  
  # constraints of scenario path for period 1
  for (eachK in 1:(k - 1)) {
    eachConstraint = rep(0, length(obj))
    eachConstraint[(2 * t * k * l) + 1] = 1
    eachConstraint[(2 * t * k * l) + 1 + eachK] = -1
    constraints = c(constraints, eachConstraint)
  }
  
  # constraints of scenario path after period 1
  if (t > 1){
    for (eachT in 1:(t - 1)){
      nodeSceanrioNum = k / nodePerPeriod[eachT + 1] # how many scenarios belong to a unique node of each period
      for(eachUniqueNode in 1:nodePerPeriod[eachT + 1]){
        for(eachScenario in 1:(nodeSceanrioNum-1)){
          eachConstraint = rep(0, length(obj))
          eachConstraint[(2 * t * k * l) + (eachT * k) + (nodeSceanrioNum * (eachUniqueNode - 1)) + 1] = 1
          eachConstraint[(2 * t * k * l) + (eachT * k) + (nodeSceanrioNum * (eachUniqueNode - 1)) + 1 + eachScenario] = -1
          # constraints = c(constraints, eachConstraint)
        }
      }
    }
  }
}

### ---- test ----

realizationSize = 30
realizations = getRealizations(realizationSize)
productStaticCovariates = getStaticCovariates()

simpleRealiztions = realizations
simpleRealiztions$matrix = simpleRealiztions$matrix[1:3, ] 

simpleRealiztions$matrix = simpleRealiztions$matrix[1:3, 1:4] 

# building tree test
standardTreeStructure = c(1, 2, 4, 16, 16)
simpleTreeStructure = c(1, 4, 4)
testSize = 50
testSet = getTestSet(productStaticCovariates, testSize)

neuralTree = getNeuralGasTree(standardTreeStructure, realizations)
simpleNeuralTree = getNeuralGasTree(simpleTreeStructure, simpleRealiztions)

binNum = 2
residudalTree = getResidualTree(realizations, productStaticCovariates, binNum)

# LP test
costStructure = getCostStructure()
obj = getObjctiveFunction(neuralTree$branch_probabilities, standardTreeStructure, costStructure)
constraints = getConstraintsMatrix(obj, standardTreeStructure, TRUE)
results = optimize(neuralTree, standardTreeStructure, costStructure)

# policy
orderPolicy = getOrderPolicy(standardTreeStructure, results)
flexibleOrderPolicy = decideFlexiblePolicy(neuralTree, testSet[, 1], orderPolicy)

# cost
cost = getCost(residudalTree, testSet[, 1], costStructure, orderPolicy)
avgCost = getAverageCost(residudalTree, testSet, costStructure, orderPolicy)

results = simulate(neuralTree, testSet, standardTreeStructure, costStructure)
results$cost
