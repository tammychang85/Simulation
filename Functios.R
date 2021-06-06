### ---- packages ----
## neural gas tree
library(scenario)
#import::here(buildtree, .from = "BuildScenarioTree.R")


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
  
  return(list(d1=d1, d2=d2, d3=d3, d4=d4, x1=x1, x2=x2, x3=x3, x4=x4))
}

# get demand realizations
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


## neural gas tree
# receive a vector of nodal structure and return a matrix of tree structure
getTreeStructureMatrix = function(noedPerPeriod){
  
  periodLength = length(noedPerPeriod)
  scenarioNum = noedPerPeriod[periodLength]
  
  # build the tree structure matrix
  treeStructure = c()
  nodeIndex = 1
  for(nodeNumEachPeriod in noedPerPeriod){
    for(eachNode in 1:nodeNumEachPeriod){
      treeStructure = c(treeStructure, rep(nodeIndex, scenarioNum / nodeNumEachPeriod))
      nodeIndex = nodeIndex + 1
    }
  }
  
  return(matrix(treeStructure, nrow=periodLength, byrow=TRUE))
}

getNeuralGasTree = function(noedPerPeriod, realizations, maxIteration=40000, plot = FALSE){
  
  treeStructure = getTreeStructureMatrix(noedPerPeriod)
  
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

### ---- test ----

realizationSize = 30
realizations = getRealizations(realizationSize)

standardTreeStructure = c(1, 2, 4, 8, 16)
neuralTree = getNeuralGasTree(standardTreeStructure, realizations)

binNum = 2
productStaticCovariates = getStaticCovariates()
residudalTree = getResidualTree(realizations, productStaticCovariates, binNum)
residudalTree2 = getResidualTree(realizations, productStaticCovariates, 3)
